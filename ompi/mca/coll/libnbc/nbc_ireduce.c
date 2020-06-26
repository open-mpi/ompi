/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014-2018 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */

#include "ompi_config.h"
#include "opal/align.h"
#include "opal/util/bit_ops.h"
#include "ompi/op/op.h"

#include "nbc_internal.h"

static inline int red_sched_binomial (int rank, int p, int root, const void *sendbuf, void *redbuf, char tmpredbuf, int count, MPI_Datatype datatype,
                                      MPI_Op op, char inplace, NBC_Schedule *schedule, void *tmpbuf);
static inline int red_sched_chain (int rank, int p, int root, const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
                                   MPI_Op op, int ext, size_t size, NBC_Schedule *schedule, void *tmpbuf, int fragsize);

static inline int red_sched_linear (int rank, int rsize, int root, const void *sendbuf, void *recvbuf, void *tmpbuf, int count, MPI_Datatype datatype,
                                    MPI_Op op, NBC_Schedule *schedule);
static inline int red_sched_redscat_gather(
    int rank, int comm_size, int root, const void *sbuf, void *rbuf,
    char tmpredbuf, int count, MPI_Datatype datatype, MPI_Op op, char inplace,
    NBC_Schedule *schedule, void *tmp_buf, struct ompi_communicator_t *comm);

#ifdef NBC_CACHE_SCHEDULE
/* tree comparison function for schedule cache */
int NBC_Reduce_args_compare(NBC_Reduce_args *a, NBC_Reduce_args *b, void *param) {
  if ((a->sendbuf == b->sendbuf) &&
      (a->recvbuf == b->recvbuf) &&
      (a->count == b->count) &&
      (a->datatype == b->datatype) &&
      (a->op == b->op) &&
      (a->root == b->root)) {
    return 0;
  }

  if (a->sendbuf < b->sendbuf) {
    return -1;
  }

  return 1;
}
#endif

/* the non-blocking reduce */
static int nbc_reduce_init(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype,
                           MPI_Op op, int root, struct ompi_communicator_t *comm, ompi_request_t ** request,
                           struct mca_coll_base_module_2_3_0_t *module, bool persistent) {
  int rank, p, res, segsize;
  size_t size;
  MPI_Aint ext;
  NBC_Schedule *schedule;
  char *redbuf=NULL, inplace;
  void *tmpbuf;
  char tmpredbuf = 0;
  enum { NBC_RED_BINOMIAL, NBC_RED_CHAIN, NBC_RED_REDSCAT_GATHER} alg;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;
  ptrdiff_t span, gap;

  NBC_IN_PLACE(sendbuf, recvbuf, inplace);

  rank = ompi_comm_rank (comm);
  p = ompi_comm_size (comm);

  res = ompi_datatype_type_extent(datatype, &ext);
  if (MPI_SUCCESS != res) {
    NBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
    return res;
  }

  res = ompi_datatype_type_size(datatype, &size);
  if (MPI_SUCCESS != res) {
    NBC_Error("MPI Error in ompi_datatype_type_size() (%i)", res);
    return res;
  }

  /* only one node -> copy data */
  if (1 == p && (!persistent || inplace)) {
    if (!inplace) {
      res = NBC_Copy (sendbuf, count, datatype, recvbuf, count, datatype, comm);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
    return nbc_get_noop_request(persistent, request);
  }

  span = opal_datatype_span(&datatype->super, count, &gap);

  /* algorithm selection */
  int nprocs_pof2 = opal_next_poweroftwo(p) >> 1;
  if (libnbc_ireduce_algorithm == 0) {
    if (ompi_op_is_commute(op) && p > 2 && count >= nprocs_pof2) {
      alg = NBC_RED_REDSCAT_GATHER;
    } else if (p > 4 || size * count < 65536 || !ompi_op_is_commute(op)) {
      alg = NBC_RED_BINOMIAL;
    } else {
      alg = NBC_RED_CHAIN;
    }
  } else {
    if (libnbc_ireduce_algorithm == 1) {
      alg = NBC_RED_CHAIN;
    } else if (libnbc_ireduce_algorithm == 2) {
      alg = NBC_RED_BINOMIAL;
    } else if (libnbc_ireduce_algorithm == 3 && ompi_op_is_commute(op) && p > 2 && count >= nprocs_pof2) {
      alg = NBC_RED_REDSCAT_GATHER;
    } else {
      alg = NBC_RED_CHAIN;
    }
  }

  /* allocate temporary buffers */
  if (alg == NBC_RED_REDSCAT_GATHER || alg == NBC_RED_BINOMIAL) {
    if (rank == root) {
      /* root reduces in receive buffer */
      tmpbuf = malloc(span);
      redbuf = recvbuf;
    } else {
      /* recvbuf may not be valid on non-root nodes */
      ptrdiff_t span_align = OPAL_ALIGN(span, datatype->super.align, ptrdiff_t);
      tmpbuf = malloc(span_align + span);
      redbuf = (char *)span_align - gap;
      tmpredbuf = 1;
    }
  } else {
    tmpbuf = malloc (span);
    segsize = 16384/2;
  }

  if (OPAL_UNLIKELY(NULL == tmpbuf)) {
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

#ifdef NBC_CACHE_SCHEDULE
  NBC_Reduce_args *args, *found, search;

  /* search schedule in communicator specific tree */
  search.sendbuf = sendbuf;
  search.recvbuf = recvbuf;
  search.count = count;
  search.datatype = datatype;
  search.op = op;
  search.root = root;
  found = (NBC_Reduce_args *) hb_tree_search ((hb_tree *) libnbc_module->NBC_Dict[NBC_REDUCE], &search);
  if (NULL == found) {
#endif
    schedule = OBJ_NEW(NBC_Schedule);
    if (OPAL_UNLIKELY(NULL == schedule)) {
      free(tmpbuf);
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    if (p == 1) {
      res = NBC_Sched_copy ((void *)sendbuf, false, count, datatype,
                            recvbuf, false, count, datatype, schedule, false);
    } else {
      switch(alg) {
        case NBC_RED_BINOMIAL:
          res = red_sched_binomial(rank, p, root, sendbuf, redbuf, tmpredbuf, count, datatype, op, inplace, schedule, tmpbuf);
          break;
        case NBC_RED_CHAIN:
          res = red_sched_chain(rank, p, root, sendbuf, recvbuf, count, datatype, op, ext, size, schedule, tmpbuf, segsize);
          break;
        case NBC_RED_REDSCAT_GATHER:
          res = red_sched_redscat_gather(rank, p, root, sendbuf, redbuf, tmpredbuf, count, datatype, op, inplace, schedule, tmpbuf, comm);
          break;
      }
    }

    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      OBJ_RELEASE(schedule);
      free(tmpbuf);
      return res;
    }

    res = NBC_Sched_commit(schedule);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      OBJ_RELEASE(schedule);
      free(tmpbuf);
      return res;
    }
#ifdef NBC_CACHE_SCHEDULE
    /* save schedule to tree */
    args = (NBC_Reduce_args *) malloc (sizeof (args));
    if (NULL != args) {
      args->sendbuf = sendbuf;
      args->recvbuf = recvbuf;
      args->count = count;
      args->datatype = datatype;
      args->op = op;
      args->root = root;
      args->schedule = schedule;
      res = hb_tree_insert ((hb_tree *) libnbc_module->NBC_Dict[NBC_REDUCE], args, args, 0);
      if (0 == res) {
        OBJ_RETAIN(schedule);

        /* increase number of elements for Reduce */
        if (++libnbc_module->NBC_Dict_size[NBC_REDUCE] > NBC_SCHED_DICT_UPPER) {
          NBC_SchedCache_dictwipe ((hb_tree *) libnbc_module->NBC_Dict[NBC_REDUCE],
                                   &libnbc_module->NBC_Dict_size[NBC_REDUCE]);
        }
      } else {
        NBC_Error("error in dict_insert() (%i)", res);
        free (args);
      }
    }
  } else {
    /* found schedule */
    schedule = found->schedule;
    OBJ_RETAIN(schedule);
  }
#endif

  res = NBC_Schedule_request(schedule, comm, libnbc_module, persistent, request, tmpbuf);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    free(tmpbuf);
    return res;
  }

  return OMPI_SUCCESS;
}

int ompi_coll_libnbc_ireduce(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype,
                             MPI_Op op, int root, struct ompi_communicator_t *comm, ompi_request_t ** request,
                             struct mca_coll_base_module_2_3_0_t *module) {
    int res = nbc_reduce_init(sendbuf, recvbuf, count, datatype, op, root,
                              comm, request, module, false);
    if (OPAL_LIKELY(OMPI_SUCCESS != res)) {
        return res;
    }
    res = NBC_Start(*(ompi_coll_libnbc_request_t **)request);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        NBC_Return_handle (*(ompi_coll_libnbc_request_t **)request);
        *request = &ompi_request_null.request;
        return res;
    }

    return OMPI_SUCCESS;
}

static int nbc_reduce_inter_init(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype,
                                 MPI_Op op, int root, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                 struct mca_coll_base_module_2_3_0_t *module, bool persistent) {
  int rank, res, rsize;
  NBC_Schedule *schedule;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;
  ptrdiff_t span, gap;
  void *tmpbuf;

  rank = ompi_comm_rank (comm);
  rsize = ompi_comm_remote_size (comm);

  span = opal_datatype_span(&datatype->super, count, &gap);
  tmpbuf = malloc (span);
  if (OPAL_UNLIKELY(NULL == tmpbuf)) {
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  schedule = OBJ_NEW(NBC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    free(tmpbuf);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  res = red_sched_linear (rank, rsize, root, sendbuf, recvbuf, (void *)(-gap), count, datatype, op, schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    free(tmpbuf);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  res = NBC_Sched_commit(schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    free(tmpbuf);
    return res;
  }

  res = NBC_Schedule_request(schedule, comm, libnbc_module, persistent, request, tmpbuf);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    free(tmpbuf);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  return OMPI_SUCCESS;
}

int ompi_coll_libnbc_ireduce_inter(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype,
                                   MPI_Op op, int root, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                   struct mca_coll_base_module_2_3_0_t *module) {
    int res = nbc_reduce_inter_init(sendbuf, recvbuf, count, datatype, op, root,
                                    comm, request, module, false);
    if (OPAL_LIKELY(OMPI_SUCCESS != res)) {
        return res;
    }
    res = NBC_Start(*(ompi_coll_libnbc_request_t **)request);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        NBC_Return_handle (*(ompi_coll_libnbc_request_t **)request);
        *request = &ompi_request_null.request;
        return res;
    }

    return OMPI_SUCCESS;
}


/* binomial reduce
 * if op is not commutative, reduce on rank 0, and then send the result to root rank
 *
 * working principle:
 * - each node gets a virtual rank vrank
 * - the 'root' node get vrank 0
 * - node 0 gets the vrank of the 'root'
 * - all other ranks stay identical (they do not matter)
 *
 * Algorithm:
 * pairwise exchange
 * round r:
 *  grp = rank % 2^r
 *  if grp == 0: receive from rank + 2^(r-1) if it exists and reduce value
 *  if grp == 1: send to rank - 2^(r-1) and exit function
 *
 * do this for R=log_2(p) rounds
 *
 */
#define RANK2VRANK(rank, vrank, root) \
{ \
  vrank = rank; \
  if (rank == 0) vrank = root; \
  if (rank == root) vrank = 0; \
}
#define VRANK2RANK(rank, vrank, root) \
{ \
  rank = vrank; \
  if (vrank == 0) rank = root; \
  if (vrank == root) rank = 0; \
}
static inline int red_sched_binomial (int rank, int p, int root, const void *sendbuf, void *redbuf, char tmpredbuf, int count, MPI_Datatype datatype,
                                      MPI_Op op, char inplace, NBC_Schedule *schedule, void *tmpbuf) {
  int vroot, vrank, vpeer, peer, res, maxr;
  char *rbuf, *lbuf, *buf;
  int tmprbuf, tmplbuf;
  ptrdiff_t gap;
  (void)opal_datatype_span(&datatype->super, count, &gap);

  if (ompi_op_is_commute(op)) {
    vroot = root;
  } else {
    vroot = 0;
  }
  RANK2VRANK(rank, vrank, vroot);
  maxr = (int)ceil((log((double)p)/LOG2));

  if (rank != root) {
    inplace = 0;
  }

  /* ensure the result ends up in redbuf on vrank 0 */
  if (0 == (maxr%2)) {
    rbuf = (void *)(-gap);
    tmprbuf = true;
    lbuf = redbuf;
    tmplbuf = tmpredbuf;
  } else {
    lbuf = (void *)(-gap);
    tmplbuf = true;
    rbuf = redbuf;
    tmprbuf = tmpredbuf;
    if (inplace) {
        res = NBC_Sched_copy(rbuf, false, count, datatype,
                             ((char *)tmpbuf)-gap, false, count, datatype,
                             schedule, true);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          return res;
        }
    }
  }

  for (int r = 1, firstred = 1 ; r <= maxr ; ++r) {
    if ((vrank % (1 << r)) == 0) {
      /* we have to receive this round */
      vpeer = vrank + (1 << (r - 1));
      VRANK2RANK(peer, vpeer, vroot)
      if (peer < p) {
        int tbuf;
        /* we have to wait until we have the data */
        res = NBC_Sched_recv (rbuf, tmprbuf, count, datatype, peer, schedule, true);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          return res;
        }

        /* perform the reduce in my local buffer */
        /* this cannot be done until tmpbuf is unused :-( so barrier after the op */
        if (firstred && !inplace) {
          /* perform the reduce with the senbuf */
          res = NBC_Sched_op (sendbuf, false, rbuf, tmprbuf, count, datatype, op, schedule, true);
          firstred = 0;
        } else {
          /* perform the reduce in my local buffer */
          res = NBC_Sched_op (lbuf, tmplbuf, rbuf, tmprbuf, count, datatype, op, schedule, true);
        }

        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          return res;
        }
        /* swap left and right buffers */
        buf = rbuf; rbuf = lbuf ; lbuf = buf;
        tbuf = tmprbuf; tmprbuf = tmplbuf; tmplbuf = tbuf;
      }
    } else {
      /* we have to send this round */
      vpeer = vrank - (1 << (r - 1));
      VRANK2RANK(peer, vpeer, vroot)
      if (firstred && !inplace) {
        /* we have to use the sendbuf in the first round .. */
        res = NBC_Sched_send (sendbuf, false, count, datatype, peer, schedule, false);
      } else {
        /* and the redbuf in all remaining rounds */
        res = NBC_Sched_send (lbuf, tmplbuf, count, datatype, peer, schedule, false);
      }

      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }

      /* leave the game */
      break;
    }
  }
  /* send to root if vroot ! root */
  if (vroot != root) {
    if (0 == rank) {
      res = NBC_Sched_send (redbuf, tmpredbuf, count, datatype, root, schedule, false);
    } else if (root == rank) {
      res = NBC_Sched_recv (redbuf, tmpredbuf, count, datatype, vroot, schedule, false);
    }
  }

  return OMPI_SUCCESS;
}

/* chain send ... */
static inline int red_sched_chain (int rank, int p, int root, const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
                                   MPI_Op op, int ext, size_t size, NBC_Schedule *schedule, void *tmpbuf, int fragsize) {
  int res, vrank, rpeer, speer, numfrag, fragcount, thiscount;
  long offset;

  RANK2VRANK(rank, vrank, root);
  VRANK2RANK(rpeer, vrank+1, root);
  VRANK2RANK(speer, vrank-1, root);

  if (0 == count) {
    return OMPI_SUCCESS;
  }

  numfrag = count * size / fragsize;
  if ((count * size) % fragsize != 0) {
    numfrag++;
  }

  fragcount = count / numfrag;

  for (int fragnum = 0 ; fragnum < numfrag ; ++fragnum) {
    offset = fragnum * fragcount * ext;
    thiscount = fragcount;
    if(fragnum == numfrag - 1) {
      /* last fragment may not be full */
      thiscount = count - fragcount * fragnum;
    }

    /* last node does not recv */
    if (vrank != p-1) {
      if (vrank == 0 && sendbuf != recvbuf) {
          res = NBC_Sched_recv ((char *)recvbuf+offset, false, thiscount, datatype, rpeer, schedule, true);
        } else {
          res = NBC_Sched_recv ((char *)offset, true, thiscount, datatype, rpeer, schedule, true);
        }
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }

      /* root reduces into receivebuf */
      if(vrank == 0) {
        if (sendbuf != recvbuf) {
            res = NBC_Sched_op ((char *) sendbuf + offset, false, (char *) recvbuf + offset, false,
                                 thiscount, datatype, op, schedule, true);
        } else {
            res = NBC_Sched_op ((char *)offset, true, (char *) recvbuf + offset, false,
                                 thiscount, datatype, op, schedule, true);
        }
      } else {
        res = NBC_Sched_op ((char *) sendbuf + offset, false, (char *) offset, true, thiscount,
                             datatype, op, schedule, true);
      }

      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }

    /* root does not send */
    if (vrank != 0) {
      /* rank p-1 has to send out of sendbuffer :) */
      /* the barrier here seems awkward but isn't!!!! */
      if (vrank == p-1) {
        res = NBC_Sched_send ((char *) sendbuf + offset, false, thiscount, datatype, speer, schedule, true);
      } else {
        res = NBC_Sched_send ((char *) offset, true, thiscount, datatype, speer, schedule, true);
      }

      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
  }

  return OMPI_SUCCESS;
}

/* simple linear algorithm for intercommunicators */
static inline int red_sched_linear (int rank, int rsize, int root, const void *sendbuf, void *recvbuf, void *tmpbuf, int count, MPI_Datatype datatype,
                                    MPI_Op op, NBC_Schedule *schedule) {
  int res;
  char *rbuf, *lbuf, *buf;
  int tmprbuf, tmplbuf;

  if (0 == count) {
    return OMPI_SUCCESS;
  }

  if (MPI_ROOT == root) {
    /* ensure the result ends up in recvbuf */
    if (0 == (rsize%2)) {
      lbuf = tmpbuf;
      tmplbuf = true;
      rbuf = recvbuf;
      tmprbuf = false;
    } else {
      rbuf = tmpbuf;
      tmprbuf = true;
      lbuf = recvbuf;
      tmplbuf = false;
    }

    res = NBC_Sched_recv (lbuf, tmplbuf, count, datatype, 0, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }

    for (int peer = 1 ; peer < rsize ; ++peer) {
      res = NBC_Sched_recv (rbuf, tmprbuf, count, datatype, peer, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }

      res = NBC_Sched_op (lbuf, tmplbuf, rbuf, tmprbuf, count, datatype, op, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
      /* swap left and right buffers */
      buf = rbuf; rbuf = lbuf ; lbuf = buf;
      tmprbuf ^= 1; tmplbuf ^= 1;
    }
  } else if (MPI_PROC_NULL != root) {
    res = NBC_Sched_send (sendbuf, false, count, datatype, root, schedule, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
  }

  return OMPI_SUCCESS;
}

/*
 * red_sched_redscat_gather:
 *
 * Description: an implementation of Rabenseifner's Reduce algorithm [1, 2].
 *   [1] Rajeev Thakur, Rolf Rabenseifner and William Gropp.
 *       Optimization of Collective Communication Operations in MPICH //
 *       The Int. Journal of High Performance Computing Applications. Vol 19,
 *       Issue 1, pp. 49--66.
 *   [2] http://www.hlrs.de/mpi/myreduce.html.
 *
 * This algorithm is a combination of a reduce-scatter implemented with
 * recursive vector halving and recursive distance doubling, followed either
 * by a binomial tree gather.
 *
 * Step 1. If the number of processes is not a power of two, reduce it to
 * the nearest lower power of two (p' = 2^{\floor{\log_2 p}})
 * by removing r = p - p' extra processes as follows. In the first 2r processes
 * (ranks 0 to 2r - 1), all the even ranks send the second half of the input
 * vector to their right neighbor (rank + 1), and all the odd ranks send
 * the first half of the input vector to their left neighbor (rank - 1).
 * The even ranks compute the reduction on the first half of the vector and
 * the odd ranks compute the reduction on the second half. The odd ranks then
 * send the result to their left neighbors (the even ranks). As a result,
 * the even ranks among the first 2r processes now contain the reduction with
 * the input vector on their right neighbors (the odd ranks). These odd ranks
 * do not participate in the rest of the algorithm, which leaves behind
 * a power-of-two number of processes. The first r even-ranked processes and
 * the last p - 2r processes are now renumbered from 0 to p' - 1.
 *
 * Step 2. The remaining processes now perform a reduce-scatter by using
 * recursive vector halving and recursive distance doubling. The even-ranked
 * processes send the second half of their buffer to rank + 1 and the odd-ranked
 * processes send the first half of their buffer to rank - 1. All processes
 * then compute the reduction between the local buffer and the received buffer.
 * In the next log_2(p') - 1 steps, the buffers are recursively halved, and the
 * distance is doubled. At the end, each of the p' processes has 1 / p' of the
 * total reduction result.
 *
 * Step 3. A binomial tree gather is performed by using recursive vector
 * doubling and distance halving. In the non-power-of-two case, if the root
 * happens to be one of those odd-ranked processes that would normally
 * be removed in the first step, then the role of this process and process 0
 * are interchanged.
 *
 * Limitations:
 *   count >= 2^{\floor{\log_2 p}}
 *   commutative operations only
 *   intra-communicators only
 *
 * Memory requirements (per process):
 *   rank != root: 2 * count * typesize + 4 * \log_2(p) * sizeof(int) = O(count)
 *   rank == root: count * typesize + 4 * \log_2(p) * sizeof(int) = O(count)
 *
 * Schedule length (rounds): O(\log(p))
 * Recommendations: root = 0, otherwise it is required additional steps
 *                  in the root process.
 */
static inline int red_sched_redscat_gather(
    int rank, int comm_size, int root, const void *sbuf, void *rbuf,
    char tmpredbuf, int count, MPI_Datatype datatype, MPI_Op op, char inplace,
    NBC_Schedule *schedule, void *tmp_buf, struct ompi_communicator_t *comm)
{
    int res = OMPI_SUCCESS;
    int *rindex = NULL, *rcount = NULL, *sindex = NULL, *scount = NULL;

    /* Find nearest power-of-two less than or equal to comm_size */
    int nsteps = opal_hibit(comm_size, comm->c_cube_dim + 1);   /* ilog2(comm_size) */
    if (nsteps < 1) {
        /* This case never happens (for comm_size < 2 other algorithms are used) */
        return OMPI_ERR_NOT_SUPPORTED;
    }
    int nprocs_pof2 = 1 << nsteps;                              /* flp2(comm_size) */

    ptrdiff_t lb, extent;
    ompi_datatype_get_extent(datatype, &lb, &extent);

    if ((rank != root) || !inplace) {
        res = NBC_Sched_copy((char *)sbuf, false, count, datatype,
                             rbuf, tmpredbuf, count, datatype, schedule, true);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
    }

    /*
     * Step 1. Reduce the number of processes to the nearest lower power of two
     * p' = 2^{\floor{\log_2 p}} by removing r = p - p' processes.
     * 1. In the first 2r processes (ranks 0 to 2r - 1), all the even ranks send
     *    the second half of the input vector to their right neighbor (rank + 1)
     *    and all the odd ranks send the first half of the input vector to their
     *    left neighbor (rank - 1).
     * 2. All 2r processes compute the reduction on their half.
     * 3. The odd ranks then send the result to their left neighbors
     *    (the even ranks).
     *
     * The even ranks (0 to 2r - 1) now contain the reduction with the input
     * vector on their right neighbors (the odd ranks). The first r even
     * processes and the p - 2r last processes are renumbered from
     * 0 to 2^{\floor{\log_2 p}} - 1. These odd ranks do not participate in the
     * rest of the algorithm.
     */

    int vrank, step, wsize;
    int nprocs_rem = comm_size - nprocs_pof2;

    if (rank < 2 * nprocs_rem) {
        int count_lhalf = count / 2;
        int count_rhalf = count - count_lhalf;

        if (rank % 2 != 0) {
            /*
             * Odd process -- exchange with rank - 1
             * Send the left half of the input vector to the left neighbor,
             * Recv the right half of the input vector from the left neighbor
             */
            res = NBC_Sched_send(rbuf, tmpredbuf, count_lhalf, datatype, rank - 1,
                                 schedule, false);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }

            res = NBC_Sched_recv((char *)tmp_buf + (ptrdiff_t)count_lhalf * extent,
                                 false, count_rhalf, datatype, rank - 1, schedule, true);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }

            res = NBC_Sched_op((char *)tmp_buf + (ptrdiff_t)count_lhalf * extent,
                               false, (char *)rbuf + (ptrdiff_t)count_lhalf * extent,
                               tmpredbuf, count_rhalf, datatype, op, schedule, true);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }

            /* Send the right half to the left neighbor */
            res = NBC_Sched_send((char *)rbuf + (ptrdiff_t)count_lhalf * extent,
                                 tmpredbuf, count_rhalf, datatype, rank - 1, schedule, true);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }

            /* This process does not participate in recursive doubling phase */
            vrank = -1;

        } else {
            /*
             * Even process -- exchange with rank + 1
             * Send the right half of the input vector to the right neighbor,
             * Recv the left half of the input vector from the right neighbor
             */
            res = NBC_Sched_send((char *)rbuf + (ptrdiff_t)count_lhalf * extent,
                                 tmpredbuf, count_rhalf, datatype, rank + 1, schedule, false);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }

            res = NBC_Sched_recv((char *)tmp_buf, false, count_lhalf, datatype, rank + 1,
                                 schedule, true);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }

            res = NBC_Sched_op(tmp_buf, false, rbuf, tmpredbuf, count_lhalf,
                               datatype, op, schedule, true);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }

            /* Recv the right half from the right neighbor */
            res = NBC_Sched_recv((char *)rbuf + (ptrdiff_t)count_lhalf * extent,
                                 tmpredbuf, count_rhalf, datatype, rank + 1, schedule, true);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }

            vrank = rank / 2;
        }
    } else { /* rank >= 2 * nprocs_rem */
        vrank = rank - nprocs_rem;
    }

    /*
     * Step 2. Reduce-scatter implemented with recursive vector halving and
     * recursive distance doubling. We have p' = 2^{\floor{\log_2 p}}
     * power-of-two number of processes with new ranks (vrank) and result in rbuf.
     *
     * The even-ranked processes send the right half of their buffer to rank + 1
     * and the odd-ranked processes send the left half of their buffer to
     * rank - 1. All processes then compute the reduction between the local
     * buffer and the received buffer. In the next \log_2(p') - 1 steps, the
     * buffers are recursively halved, and the distance is doubled. At the end,
     * each of the p' processes has 1 / p' of the total reduction result.
     */

    rindex = malloc(sizeof(*rindex) * nsteps);    /* O(\log_2(p)) */
    sindex = malloc(sizeof(*sindex) * nsteps);
    rcount = malloc(sizeof(*rcount) * nsteps);
    scount = malloc(sizeof(*scount) * nsteps);
    if (NULL == rindex || NULL == sindex || NULL == rcount || NULL == scount) {
        res = OMPI_ERR_OUT_OF_RESOURCE;
        goto cleanup_and_return;
    }

    if (vrank != -1) {
        step = 0;
        wsize = count;
        sindex[0] = rindex[0] = 0;

        for (int mask = 1; mask < nprocs_pof2; mask <<= 1) {
            /*
             * On each iteration: rindex[step] = sindex[step] -- begining of the
             * current window. Length of the current window is storded in wsize.
             */
            int vdest = vrank ^ mask;
            /* Translate vdest virtual rank to real rank */
            int dest = (vdest < nprocs_rem) ? vdest * 2 : vdest + nprocs_rem;

            if (rank < dest) {
                /*
                 * Recv into the left half of the current window, send the right
                 * half of the window to the peer (perform reduce on the left
                 * half of the current window)
                 */
                rcount[step] = wsize / 2;
                scount[step] = wsize - rcount[step];
                sindex[step] = rindex[step] + rcount[step];
            } else {
                /*
                 * Recv into the right half of the current window, send the left
                 * half of the window to the peer (perform reduce on the right
                 * half of the current window)
                 */
                scount[step] = wsize / 2;
                rcount[step] = wsize - scount[step];
                rindex[step] = sindex[step] + scount[step];
            }

            /* Send part of data from the rbuf, recv into the tmp_buf */
            res = NBC_Sched_send((char *)rbuf + (ptrdiff_t)sindex[step] * extent,
                                 tmpredbuf, scount[step], datatype, dest, schedule, false);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
            res = NBC_Sched_recv((char *)tmp_buf + (ptrdiff_t)rindex[step] * extent,
                                 false, rcount[step], datatype, dest, schedule, true);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }

            /* Local reduce: rbuf[] = tmp_buf[] <op> rbuf[] */
            res = NBC_Sched_op((char *)tmp_buf + (ptrdiff_t)rindex[step] * extent,
                               false, (char *)rbuf + (ptrdiff_t)rindex[step] * extent,
                               tmpredbuf, rcount[step], datatype, op, schedule, true);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }

            /* Move the current window to the received message */
            if (step + 1 < nsteps) {
                rindex[step + 1] = rindex[step];
                sindex[step + 1] = rindex[step];
                wsize = rcount[step];
                step++;
            }
        }
    }
    /*
     * Assertion: each process has 1 / p' of the total reduction result:
     * rcount[nsteps - 1] elements in the rbuf[rindex[nsteps - 1], ...].
     */

    /*
     * Setup the root process for gather operation.
     * Case 1: root < 2r and root is odd -- root process was excluded on step 1
     *         Recv data from process 0, vroot = 0, vrank = 0
     * Case 2: root < 2r and root is even: vroot = root / 2
     * Case 3: root >= 2r: vroot = root - r
     */
    int vroot = 0;
    if (root < 2 * nprocs_rem) {
        if (root % 2 != 0) {
            vroot = 0;
            if (rank == root) {
                /*
                 * Case 1: root < 2r and root is odd -- root process was
                 * excluded on step 1 (newrank == -1).
                 * Recv a data from the process 0.
                 */
                rindex[0] = 0;
                step = 0, wsize = count;
                for (int mask = 1; mask < nprocs_pof2; mask *= 2) {
                    rcount[step] = wsize / 2;
                    scount[step] = wsize - rcount[step];
                    rindex[step] = 0;
                    sindex[step] = rcount[step];
                    step++;
                    wsize /= 2;
                }

                res = NBC_Sched_recv(rbuf, tmpredbuf, rcount[nsteps - 1], datatype,
                                     0, schedule, true);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
                vrank = 0;

            } else if (vrank == 0) {
                /* Send a data to the root */
                res = NBC_Sched_send(rbuf, tmpredbuf, rcount[nsteps - 1], datatype,
                                     root, schedule, true);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
                vrank = -1;
            }
        } else {
            /* Case 2: root < 2r and a root is even: vroot = root / 2 */
            vroot = root / 2;
        }
    } else {
        /* Case 3: root >= 2r: newroot = root - r */
        vroot = root - nprocs_rem;
    }

    /*
     * Step 3. Gather result at the vroot by the binomial tree algorithm.
     * Each process has 1 / p' of the total reduction result:
     * rcount[nsteps - 1] elements in the rbuf[rindex[nsteps - 1], ...].
     * All exchanges are executed in reverse order relative
     * to recursive doubling (previous step).
     */

    if (vrank != -1) {
        int vdest_tree, vroot_tree;
        step = nsteps - 1; /* step = ilog2(p') - 1 */

        for (int mask = nprocs_pof2 >> 1; mask > 0; mask >>= 1) {
            int vdest = vrank ^ mask;
            /* Translate vdest virtual rank to real rank */
            int dest = (vdest < nprocs_rem) ? vdest * 2 : vdest + nprocs_rem;
            if ((vdest == 0) && (root < 2 * nprocs_rem) && (root % 2 != 0))
                dest = root;

            vdest_tree = vdest >> step;
            vdest_tree <<= step;
            vroot_tree = vroot >> step;
            vroot_tree <<= step;
            if (vdest_tree == vroot_tree) {
                /* Send data from rbuf and exit */

                res = NBC_Sched_send((char *)rbuf + (ptrdiff_t)rindex[step] * extent,
                                     tmpredbuf, rcount[step], datatype, dest, schedule, false);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
                break;
            } else {
                /* Recv and continue */
                res = NBC_Sched_recv((char *)rbuf + (ptrdiff_t)sindex[step] * extent,
                                     tmpredbuf, scount[step], datatype, dest, schedule, true);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
            }
            step--;
        }
    }

  cleanup_and_return:
    if (NULL != rindex)
        free(rindex);
    if (NULL != sindex)
        free(sindex);
    if (NULL != rcount)
        free(rcount);
    if (NULL != scount)
        free(scount);
    return res;
}

int ompi_coll_libnbc_reduce_init(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype,
                                 MPI_Op op, int root, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                 struct mca_coll_base_module_2_3_0_t *module) {
    int res = nbc_reduce_init(sendbuf, recvbuf, count, datatype, op, root,
                              comm, request, module, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
    }

    return OMPI_SUCCESS;
}

int ompi_coll_libnbc_reduce_inter_init(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype,
                                       MPI_Op op, int root, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                       struct mca_coll_base_module_2_3_0_t *module) {
    int res = nbc_reduce_inter_init(sendbuf, recvbuf, count, datatype, op, root,
                                    comm, request, module, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
    }

    return OMPI_SUCCESS;
}
