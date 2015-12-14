/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "nbc_internal.h"

static inline int red_sched_binomial (int rank, int p, int root, const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
                                      MPI_Op op, void *redbuf, NBC_Schedule *schedule, NBC_Handle *handle);
static inline int red_sched_chain (int rank, int p, int root, const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
                                   MPI_Op op, int ext, size_t size, NBC_Schedule *schedule, NBC_Handle *handle, int fragsize);

static inline int red_sched_linear (int rank, int rsize, int root, const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
                                    MPI_Op op, NBC_Schedule *schedule, NBC_Handle *handle);

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
int ompi_coll_libnbc_ireduce(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype,
                             MPI_Op op, int root, struct ompi_communicator_t *comm, ompi_request_t ** request,
                             struct mca_coll_base_module_2_1_0_t *module) {
  int rank, p, res, segsize;
  size_t size;
  MPI_Aint ext;
  NBC_Schedule *schedule;
  char *redbuf=NULL, inplace;
  enum { NBC_RED_BINOMIAL, NBC_RED_CHAIN } alg;
  NBC_Handle *handle;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

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
  if ((p == 1) && !inplace) {
    res = NBC_Copy (sendbuf, count, datatype, recvbuf, count, datatype, comm);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }

    *request = &ompi_request_empty;
    return OMPI_SUCCESS;
  }

  res = NBC_Init_handle (comm, &handle, libnbc_module);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    return res;
  }

  /* algorithm selection */
  if (p > 4 || size * count < 65536) {
    alg = NBC_RED_BINOMIAL;
    if(rank == root) {
      /* root reduces in receivebuffer */
      handle->tmpbuf = malloc (ext * count);
    } else {
      /* recvbuf may not be valid on non-root nodes */
      handle->tmpbuf = malloc (ext * count * 2);
      redbuf = (char*) handle->tmpbuf + ext * count;
    }
  } else {
    handle->tmpbuf = malloc (ext * count);
    alg = NBC_RED_CHAIN;
    segsize = 16384/2;
  }

  if (OPAL_UNLIKELY(NULL == handle->tmpbuf)) {
    NBC_Return_handle (handle);
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
      NBC_Return_handle (handle);
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* make sure the schedule is released with the handle on error */
    handle->schedule = schedule;

    switch(alg) {
      case NBC_RED_BINOMIAL:
        res = red_sched_binomial(rank, p, root, sendbuf, recvbuf, count, datatype, op, redbuf, schedule, handle);
        break;
      case NBC_RED_CHAIN:
        res = red_sched_chain(rank, p, root, sendbuf, recvbuf, count, datatype, op, ext, size, schedule, handle, segsize);
        break;
    }

    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      NBC_Return_handle (handle);
      return res;
    }

    res = NBC_Sched_commit(schedule);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      NBC_Return_handle (handle);
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

  res = NBC_Start(handle, schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    NBC_Return_handle (handle);
    return res;
  }

  *request = (ompi_request_t *) handle;

  /* tmpbuf is freed with the handle */
  return OMPI_SUCCESS;
}

int ompi_coll_libnbc_ireduce_inter(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype,
				   MPI_Op op, int root, struct ompi_communicator_t *comm, ompi_request_t ** request,
				   struct mca_coll_base_module_2_1_0_t *module) {
  int rank, res, rsize;
  NBC_Schedule *schedule;
  MPI_Aint ext;
  NBC_Handle *handle;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

  rank = ompi_comm_rank (comm);
  rsize = ompi_comm_remote_size (comm);

  res = ompi_datatype_type_extent (datatype, &ext);
  if (MPI_SUCCESS != res) {
    NBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
    return res;
  }

  res = NBC_Init_handle(comm, &handle, libnbc_module);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    return res;
  }

  handle->tmpbuf = malloc (ext * count);
  if (OPAL_UNLIKELY(NULL == handle->tmpbuf)) {
    NBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  schedule = OBJ_NEW(NBC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    NBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  res = red_sched_linear (rank, rsize, root, sendbuf, recvbuf, count, datatype, op, schedule, handle);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    NBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  res = NBC_Sched_commit(schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    NBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  res = NBC_Start(handle, schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    NBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  *request = (ompi_request_t *) handle;

  /* tmpbuf is freed with the handle */
  return OMPI_SUCCESS;
}


/* binomial reduce
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
static inline int red_sched_binomial (int rank, int p, int root, const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
                                      MPI_Op op, void *redbuf, NBC_Schedule *schedule, NBC_Handle *handle) {
  int vrank, vpeer, peer, res, maxr;

  RANK2VRANK(rank, vrank, root);
  maxr = (int)ceil((log((double)p)/LOG2));

  for (int r = 1, firstred = 1 ; r <= maxr ; ++r) {
    if ((vrank % (1 << r)) == 0) {
      /* we have to receive this round */
      vpeer = vrank + (1 << (r - 1));
      VRANK2RANK(peer, vpeer, root)
      if (peer < p) {
        /* we have to wait until we have the data */
        res = NBC_Sched_recv (0, true, count, datatype, peer, schedule, true);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          return res;
        }

        /* perform the reduce in my local buffer */
        /* this cannot be done until handle->tmpbuf is unused :-( so barrier after the op */
        if (firstred) {
          if (rank == root) {
            /* root is the only one who reduces in the receivebuffer
             * take data from sendbuf in first round - save copy */
            res = NBC_Sched_op (recvbuf, false, sendbuf, false, 0, true, count, datatype, op, schedule, true);
          } else {
            /* all others may not have a receive buffer
             * take data from sendbuf in first round - save copy */
            res = NBC_Sched_op ((char *) redbuf - (intptr_t) handle->tmpbuf, true, sendbuf, false, 0, true, count,
                                datatype, op, schedule, true);
          }
          firstred = 0;
        } else {
          if(rank == root) {
            /* root is the only one who reduces in the receivebuffer */
            res = NBC_Sched_op (recvbuf, false, recvbuf, false, 0, true, count, datatype, op, schedule, true);
          } else {
            /* all others may not have a receive buffer */
            res = NBC_Sched_op ((char *) redbuf - (intptr_t) handle->tmpbuf, true, (char *) redbuf - (intptr_t) handle->tmpbuf,
                                true, 0, true, count, datatype, op, schedule, true);
          }
        }

        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          return res;
        }
      }
    } else {
      /* we have to send this round */
      vpeer = vrank - (1 << (r - 1));
      VRANK2RANK(peer, vpeer, root)
      if (firstred) {
        /* we did not reduce anything */
        res = NBC_Sched_send (sendbuf, false, count, datatype, peer, schedule, false);
      } else {
        /* we have to use the redbuf the root (which works in receivebuf) is never sending .. */
        res = NBC_Sched_send ((char *) redbuf - (intptr_t) handle->tmpbuf, true, count, datatype, peer, schedule,
                              false);
      }

      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }

      /* leave the game */
      break;
    }
  }

  return OMPI_SUCCESS;
}

/* chain send ... */
static inline int red_sched_chain (int rank, int p, int root, const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
                                   MPI_Op op, int ext, size_t size, NBC_Schedule *schedule, NBC_Handle *handle, int fragsize) {
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
      res = NBC_Sched_recv ((char *) offset, true, thiscount, datatype, rpeer, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }

      /* root reduces into receivebuf */
      if(vrank == 0) {
        res = NBC_Sched_op ((char *) recvbuf + offset, false, (char *) sendbuf + offset, false, (char *) offset, true,
                            thiscount, datatype, op, schedule, true);
      } else {
        res = NBC_Sched_op ((char *) offset, true, (char *) sendbuf + offset, false, (char *) offset, true, thiscount,
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
static inline int red_sched_linear (int rank, int rsize, int root, const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
                                    MPI_Op op, NBC_Schedule *schedule, NBC_Handle *handle) {
  int res;

  if (0 == count) {
    return OMPI_SUCCESS;
  }

  if (MPI_ROOT == root) {
    res = NBC_Sched_recv (recvbuf, false, count, datatype, 0, schedule, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }

    for (int peer = 1 ; peer < rsize ; ++peer) {
      res = NBC_Sched_recv (0, true, count, datatype, peer, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }

      res = NBC_Sched_op (recvbuf, false, 0, true, recvbuf, false, count, datatype, op, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
  } else if (MPI_PROC_NULL != root) {
    res = NBC_Sched_send (sendbuf, false, count, datatype, root, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
  }

  return OMPI_SUCCESS;
}
