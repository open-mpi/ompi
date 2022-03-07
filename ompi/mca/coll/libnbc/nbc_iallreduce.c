/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2013-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014-2018 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2017-2022 IBM Corporation.  All rights reserved.
 * Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "nbc_internal.h"
#include "ompi/communicator/communicator.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/op/op.h"
#include "opal/util/bit_ops.h"

#include <assert.h>

static inline int allred_sched_diss(int rank, int p, int count, MPI_Datatype datatype, ptrdiff_t gap, const void *sendbuf,
                                    void *recvbuf, MPI_Op op, char inplace, NBC_Schedule *schedule, void *tmpbuf);
static inline int allred_sched_recursivedoubling(int rank, int p, const void *sendbuf, void *recvbuf,
                                                 int count, MPI_Datatype datatype, ptrdiff_t gap, MPI_Op op,
                                                 char inplace, NBC_Schedule *schedule, void *tmpbuf);
static inline int allred_sched_ring(int rank, int p, int count, MPI_Datatype datatype, const void *sendbuf,
                                    void *recvbuf, MPI_Op op, int size, int ext, NBC_Schedule *schedule,
                                    void *tmpbuf);
static inline int allred_sched_linear(int rank, int p, const void *sendbuf, void *recvbuf, int count,
                                      MPI_Datatype datatype, ptrdiff_t gap, MPI_Op op, int ext, int size,
                                      NBC_Schedule *schedule, void *tmpbuf);
static inline int allred_sched_redscat_allgather(
    int rank, int comm_size, int count, MPI_Datatype datatype, ptrdiff_t gap,
    const void *sbuf, void *rbuf, MPI_Op op, char inplace,
    NBC_Schedule *schedule, void *tmpbuf, struct ompi_communicator_t *comm);

#ifdef NBC_CACHE_SCHEDULE
/* tree comparison function for schedule cache */
int NBC_Allreduce_args_compare(NBC_Allreduce_args *a, NBC_Allreduce_args *b, void *param) {
  if ((a->sendbuf == b->sendbuf) &&
      (a->recvbuf == b->recvbuf) &&
      (a->count == b->count) &&
      (a->datatype == b->datatype) &&
      (a->op == b->op)) {
    return 0;
  }

  if( a->sendbuf < b->sendbuf ) {
    return -1;
  }

  return 1;
}
#endif

static int nbc_allreduce_init(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                              struct ompi_communicator_t *comm, ompi_request_t ** request,
                              mca_coll_base_module_t *module, bool persistent)
{
  int rank, p, res;
  ptrdiff_t ext, lb;
  NBC_Schedule *schedule;
  size_t size;
#ifdef NBC_CACHE_SCHEDULE
  NBC_Allreduce_args *args, *found, search;
#endif
  enum { NBC_ARED_BINOMIAL, NBC_ARED_RING, NBC_ARED_REDSCAT_ALLGATHER, NBC_ARED_RDBL } alg;
  char inplace;
  void *tmpbuf = NULL;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;
  ptrdiff_t span, gap;

  NBC_IN_PLACE(sendbuf, recvbuf, inplace);

  rank = ompi_comm_rank (comm);
  p = ompi_comm_size (comm);

  res = ompi_datatype_get_extent(datatype, &lb, &ext);
  if (OMPI_SUCCESS != res) {
    NBC_Error ("MPI Error in ompi_datatype_type_extent() (%i)", res);
    return res;
  }

  res = ompi_datatype_type_size (datatype, &size);
  if (OMPI_SUCCESS != res) {
    NBC_Error ("MPI Error in ompi_datatype_type_size() (%i)", res);
    return res;
  }

  if (1 == p && (!persistent || inplace)) {
    if (!inplace) {
      /* for a single node - copy data to receivebuf */
      res = NBC_Copy(sendbuf, count, datatype, recvbuf, count, datatype, comm);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
    return nbc_get_noop_request(persistent, request);
  }

  span = opal_datatype_span(&datatype->super, count, &gap);
  tmpbuf = malloc (span);
  if (OPAL_UNLIKELY(NULL == tmpbuf)) {
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  alg = NBC_ARED_RING;  /* default generic selection */
  /* algorithm selection */
  int nprocs_pof2 = opal_next_poweroftwo(p) >> 1;
  if (libnbc_iallreduce_algorithm == 0) {
    if(p < 4 || size*count < 65536 || !ompi_op_is_commute(op) || inplace) {
      alg = NBC_ARED_BINOMIAL;
    } else if (count >= nprocs_pof2 && ompi_op_is_commute(op)) {
      alg = NBC_ARED_REDSCAT_ALLGATHER;
    }
  } else {
    if (libnbc_iallreduce_algorithm == 1)
      alg = NBC_ARED_RING;
    else if (libnbc_iallreduce_algorithm == 2)
      alg = NBC_ARED_BINOMIAL;
    else if (libnbc_iallreduce_algorithm == 3 && count >= nprocs_pof2 && ompi_op_is_commute(op))
      alg = NBC_ARED_REDSCAT_ALLGATHER;
    else if (libnbc_iallreduce_algorithm == 4)
      alg = NBC_ARED_RDBL;
  }
#ifdef NBC_CACHE_SCHEDULE
  /* search schedule in communicator specific tree */
  search.sendbuf = sendbuf;
  search.recvbuf = recvbuf;
  search.count = count;
  search.datatype = datatype;
  search.op = op;
  found = (NBC_Allreduce_args *) hb_tree_search ((hb_tree *) libnbc_module->NBC_Dict[NBC_ALLREDUCE], &search);
  if (NULL == found) {
#endif
    schedule = OBJ_NEW(NBC_Schedule);
    if (NULL == schedule) {
      free(tmpbuf);
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    if (p == 1) {
      res = NBC_Sched_copy((void *)sendbuf, false, count, datatype,
                           recvbuf, false, count, datatype, schedule, false);
    } else {
      switch(alg) {
        case NBC_ARED_BINOMIAL:
          res = allred_sched_diss(rank, p, count, datatype, gap, sendbuf, recvbuf, op, inplace, schedule, tmpbuf);
          break;
        case NBC_ARED_REDSCAT_ALLGATHER:
          res = allred_sched_redscat_allgather(rank, p, count, datatype, gap, sendbuf, recvbuf, op, inplace, schedule, tmpbuf, comm);
          break;
        case NBC_ARED_RING:
          res = allred_sched_ring(rank, p, count, datatype, sendbuf, recvbuf, op, size, ext, schedule, tmpbuf);
          break;
        case NBC_ARED_RDBL:
          res = allred_sched_recursivedoubling(rank, p, sendbuf, recvbuf, count, datatype, gap, op, inplace, schedule, tmpbuf);
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
    args = (NBC_Allreduce_args *) malloc (sizeof(args));
    if (NULL != args) {
      args->sendbuf = sendbuf;
      args->recvbuf = recvbuf;
      args->count = count;
      args->datatype = datatype;
      args->op = op;
      args->schedule = schedule;
      res = hb_tree_insert ((hb_tree *) libnbc_module->NBC_Dict[NBC_ALLREDUCE], args, args, 0);
      if (0 == res) {
        OBJ_RETAIN(schedule);

        /* increase number of elements for A2A */
        if (++libnbc_module->NBC_Dict_size[NBC_ALLREDUCE] > NBC_SCHED_DICT_UPPER) {
          NBC_SchedCache_dictwipe ((hb_tree *) libnbc_module->NBC_Dict[NBC_ALLREDUCE],
                                   &libnbc_module->NBC_Dict_size[NBC_ALLREDUCE]);
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

  res = NBC_Schedule_request (schedule, comm, libnbc_module, persistent, request, tmpbuf);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    free(tmpbuf);
    return res;
  }

  return OMPI_SUCCESS;
}

int ompi_coll_libnbc_iallreduce(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                                struct ompi_communicator_t *comm, ompi_request_t ** request,
                                mca_coll_base_module_t *module) {
    int res = nbc_allreduce_init(sendbuf, recvbuf, count, datatype, op,
                                 comm, request, module, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
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

static int nbc_allreduce_inter_init(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                                    struct ompi_communicator_t *comm, ompi_request_t ** request,
                                    mca_coll_base_module_t *module, bool persistent)
{
  int rank, res, rsize;
  size_t size;
  MPI_Aint ext;
  NBC_Schedule *schedule;
  void *tmpbuf = NULL;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;
  ptrdiff_t span, gap;

  rank = ompi_comm_rank (comm);
  rsize = ompi_comm_remote_size (comm);

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

  res = allred_sched_linear (rank, rsize, sendbuf, recvbuf, count, datatype, gap, op,
                             ext, size, schedule, tmpbuf);
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

  res = NBC_Schedule_request(schedule, comm, libnbc_module, persistent, request, tmpbuf);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    free(tmpbuf);
    return res;
  }

  return OMPI_SUCCESS;
}

int ompi_coll_libnbc_iallreduce_inter(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                                      struct ompi_communicator_t *comm, ompi_request_t ** request,
                                      mca_coll_base_module_t *module) {
    int res = nbc_allreduce_inter_init(sendbuf, recvbuf, count, datatype, op,
                                       comm, request, module, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
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

/* binomial allreduce (binomial tree up and binomial bcast down)
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
 * followed by a Bcast:
 * Algorithm:
 * - each node with vrank > 2^r and vrank < 2^r+1 receives from node
 *   vrank - 2^r (vrank=1 receives from 0, vrank 0 receives never)
 * - each node sends each round r to node vrank + 2^r
 * - a node stops to send if 2^r > commsize
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
static inline int allred_sched_diss(int rank, int p, int count, MPI_Datatype datatype, ptrdiff_t gap, const void *sendbuf, void *recvbuf,
                                    MPI_Op op, char inplace, NBC_Schedule *schedule, void *tmpbuf) {
  int root, vrank, maxr, vpeer, peer, res;
  char *rbuf, *lbuf, *buf;
  int tmprbuf, tmplbuf;

  root = 0; /* this makes the code for ireduce and iallreduce nearly identical - could be changed to improve performance */
  RANK2VRANK(rank, vrank, root);
  maxr = ceil_of_log2(p);
  /* ensure the result ends up in recvbuf on vrank 0 */
  if (0 == (maxr%2)) {
    rbuf = (void *)(-gap);
    tmprbuf = true;
    lbuf = recvbuf;
    tmplbuf = false;
  } else {
    lbuf = (void *)(-gap);
    tmplbuf = true;
    rbuf = recvbuf;
    tmprbuf = false;
    if (inplace) {
        res = NBC_Sched_copy(rbuf, false, count, datatype,
                             ((char *)tmpbuf) - gap, false, count, datatype,
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
      VRANK2RANK(peer, vpeer, root)
      if (peer < p) {
        /* we have to wait until we have the data */
        res = NBC_Sched_recv (rbuf, tmprbuf, count, datatype, peer, schedule, true);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          return res;
        }

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
        tmprbuf ^= 1; tmplbuf ^= 1;
      }
    } else {
      /* we have to send this round */
      vpeer = vrank - (1 << (r - 1));
      VRANK2RANK(peer, vpeer, root)
      if (firstred && !inplace) {
        /* we have to use the sendbuf in the first round .. */
        res = NBC_Sched_send (sendbuf, false, count, datatype, peer, schedule, false);
      } else {
        /* and the recvbuf in all remaining rounds */
        res = NBC_Sched_send (lbuf, tmplbuf, count, datatype, peer, schedule, false);
      }

      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }

      /* leave the game */
      break;
    }
  }

  /* this is the Bcast part - copied with minor changes from nbc_ibcast.c
   * changed: buffer -> recvbuf  */
  RANK2VRANK(rank, vrank, root);

  /* receive from the right hosts  */
  if (vrank != 0) {
    for (int r = 0; r < maxr ; ++r) {
      if ((vrank >= (1 << r)) && (vrank < (1 << (r + 1)))) {
        VRANK2RANK(peer, vrank - (1 << r), root);
        res = NBC_Sched_recv (recvbuf, false, count, datatype, peer, schedule, false);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          return res;
        }
      }
    }

    res = NBC_Sched_barrier (schedule);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
  }

  if (0 == vrank) assert(lbuf == recvbuf);
  /* now send to the right hosts */
  for (int r = 0; r < maxr; ++r) {
    if (((vrank + (1 << r) < p) && (vrank < (1 << r))) || (vrank == 0)) {
      VRANK2RANK(peer, vrank + (1 << r), root);
      res = NBC_Sched_send (recvbuf, false, count, datatype, peer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
  }

  /* end of the bcast */
  return OMPI_SUCCESS;
}

/*
 *   allred_sched_recursivedoubling
 *
 *   Function:       Recursive doubling algorithm for iallreduce operation
 *
 *   Description:    Implements recursive doubling algorithm for iallreduce.
 *                   The algorithm preserves order of operations so it can
 *                   be used both by commutative and non-commutative operations.
 *   Schedule length: O(\log(p))
 *   Memory requirements:
 *     Each process requires a temporary buffer: count * typesize = O(count)
 *
 *   Example on 7 nodes:
 *   Initial state
 *   #      0       1      2       3      4       5      6
 *         [0]     [1]    [2]     [3]    [4]     [5]    [6]
 *   Initial adjustment step for non-power of two nodes.
 *   old rank      1              3              5      6
 *   new rank      0              1              2      3
 *               [0+1]          [2+3]          [4+5]   [6]
 *   Step 1
 *   old rank      1              3              5      6
 *   new rank      0              1              2      3
 *               [0+1+]         [0+1+]         [4+5+]  [4+5+]
 *               [2+3+]         [2+3+]         [6   ]  [6   ]
 *   Step 2
 *   old rank      1              3              5      6
 *   new rank      0              1              2      3
 *               [0+1+]         [0+1+]         [0+1+]  [0+1+]
 *               [2+3+]         [2+3+]         [2+3+]  [2+3+]
 *               [4+5+]         [4+5+]         [4+5+]  [4+5+]
 *               [6   ]         [6   ]         [6   ]  [6   ]
 *   Final adjustment step for non-power of two nodes
 *   #      0       1      2       3      4       5      6
 *        [0+1+] [0+1+] [0+1+]  [0+1+] [0+1+]  [0+1+] [0+1+]
 *        [2+3+] [2+3+] [2+3+]  [2+3+] [2+3+]  [2+3+] [2+3+]
 *        [4+5+] [4+5+] [4+5+]  [4+5+] [4+5+]  [4+5+] [4+5+]
 *        [6   ] [6   ] [6   ]  [6   ] [6   ]  [6   ] [6   ]
 *
 */
static inline int allred_sched_recursivedoubling(int rank, int p, const void *sendbuf, void *recvbuf,
                                                 int count, MPI_Datatype datatype, ptrdiff_t gap, MPI_Op op,
                                                 char inplace, NBC_Schedule *schedule, void *tmpbuf)
{
  int res, pof2, nprocs_rem, vrank;
  char *tmpsend = NULL, *tmprecv = NULL, *tmpswap = NULL;

  tmpsend = (char*) tmpbuf - gap;
  tmprecv = (char*) recvbuf;

  if (inplace) {
    res = NBC_Sched_copy(recvbuf, false, count, datatype, 
                         tmpsend, false, count, datatype, schedule, true);
  } else {
    res = NBC_Sched_copy((void *)sendbuf, false, count, datatype,
                         tmpsend, false, count, datatype, schedule, true);
  }
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { return res; }

  /* Get nearest power of two less than or equal to comm size */
  pof2 = opal_next_poweroftwo(p) >> 1;

  /* Handle non-power-of-two case:
    - Even ranks less than 2 * nprocs_rem send their data to (rank + 1), and
    sets new rank to -1.
    - Odd ranks less than 2 * nprocs_rem receive data from (rank - 1),
    apply appropriate operation, and set new rank to rank/2
    - Everyone else sets rank to rank - nprocs_rem
  */
  nprocs_rem = p - pof2;
  if (rank < 2 * nprocs_rem) {
    if (0 == rank % 2) {    /* Even */
      res = NBC_Sched_send(tmpsend, false, count, datatype, rank + 1, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { return res; }
      vrank = -1;
    } else {                /* Odd */
      res = NBC_Sched_recv(tmprecv, false, count, datatype, rank - 1, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { return res; }

      /* tmpsend = tmprecv (op) tmpsend */
      res = NBC_Sched_op(tmprecv, false, tmpsend, false, count, datatype, op, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { return res; }

      vrank = rank >> 1;
    }
  } else {
    vrank = rank - nprocs_rem;
  }

  /* Communication/Computation loop
      - Exchange message with remote node.
      - Perform appropriate operation taking in account order of operations:
      result = value (op) result
  */
  if (0 <= vrank) {
    for (int distance = 1; distance < pof2; distance <<= 1) {
      int remote = vrank ^ distance;

      /* Find real rank of remote node */
      if (remote < nprocs_rem) {
          remote = remote * 2 + 1;
      } else {
          remote += nprocs_rem;
      }

      /* Exchange the data */
      res = NBC_Sched_send(tmpsend, false, count, datatype, remote, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { return res; }

      res = NBC_Sched_recv(tmprecv, false, count, datatype, remote, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { return res; }

      /* Apply operation */
      if (rank < remote) {
        /* tmprecv = tmpsend (op) tmprecv */
        res = NBC_Sched_op(tmpsend, false, tmprecv, false,
                           count, datatype, op, schedule, true);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { return res; }
        
        /* Swap tmpsend and tmprecv buffers */
        tmpswap = tmprecv; tmprecv = tmpsend; tmpsend = tmpswap;
      } else {
        /* tmpsend = tmprecv (op) tmpsend */
        res = NBC_Sched_op(tmprecv, false, tmpsend, false,
                           count, datatype, op, schedule, true);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { return res; }
      }
    }
  }

  /* Handle non-power-of-two case:
      - Even ranks less than 2 * nprocs_rem receive result from (rank + 1)
      - Odd ranks less than 2 * nprocs_rem send result from tmpsend to (rank - 1)
  */
  if (rank < 2 * nprocs_rem) {
    if (0 == rank % 2) {    /* Even */
      res = NBC_Sched_recv(recvbuf, false, count, datatype, rank + 1, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { return res; }
      tmpsend = (char *)recvbuf;
    } else {                /* Odd */
      res = NBC_Sched_send(tmpsend, false, count, datatype, rank - 1, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { return res; }
    }
  }

  /* Copy result back into recvbuf */
  if (tmpsend != recvbuf) {
    res = NBC_Sched_copy(tmpsend, false, count, datatype,
                         recvbuf, false, count, datatype, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { return res; }
  }

  return OMPI_SUCCESS;
}

static inline int
allred_sched_ring(int r, int p,
                  int count, MPI_Datatype datatype, const void *sendbuf, void *recvbuf,
                  MPI_Op op, int size, int ext, NBC_Schedule *schedule, void *tmpbuf)
{
  int segsize, *segsizes, *segoffsets; /* segment sizes and offsets per segment (number of segments == number of nodes */
  int speer, rpeer; /* send and recv peers */
  int res = OMPI_SUCCESS;

  if (0 == count) {
    return OMPI_SUCCESS;
  }

  segsizes = (int *) malloc((2 * p + 1 ) *sizeof (int));
  if (NULL == segsizes) {
    return OMPI_ERR_OUT_OF_RESOURCE;
  }
  segoffsets = segsizes + p;

  segsize = count / p; /* size of the segments across the last ranks.
                          The remainder will be evenly distributed across the smaller ranks */

  segoffsets[0] = 0;
  for (int i = 0, mycount = count % p; i < p ; ++i) {
    segsizes[i] = segsize;
    if( mycount > 0 ) {  /* We have extra segments to distribute */
        segsizes[i]++;
        mycount--;
    }

    segoffsets[i+1] = segoffsets[i] + segsizes[i];
  }

  /* reduce peers */
  speer = (r + 1) % p;
  rpeer = (r - 1 + p) % p;

  /*  + -> reduced this round
   *  / -> sum (reduced in a previous step)
   *
   *     *** round 0 ***
   *    0        1        2
   *
   *   00       10       20      0: [1] -> 1
   *   01       11       21      1: [2] -> 2
   *   02       12       22      2: [0] -> 0  --> send element (r+1)%p to node (r+1)%p
   *
   *      *** round 1 ***
   *    0        1        2
   *
   *   00+20    10       20     0: red(0), [0] -> 1
   *   01       11+01    21     1: red(1), [1] -> 2
   *   02       12       22+12  2: red(2), [2] -> 0 --> reduce and send element (r+0)%p to node (r+1)%p
   *
   *      *** round 2 ***
   *    0        1        2
   *
   *   00/20    all      20     0: red(2), [2] -> 1
   *   01       11/01    all    1: red(0), [0] -> 2
   *   all      12       22/12  2: red(1), [1] -> 0 --> reduce and send (r-1)%p to node (r+1)%p
   *
   *      *** round 3 ***
   *    0        1        2
   *
   *   00/20    all      all    0: [1] -> 1
   *   all      11/01    all    1: [2] -> 2
   *   all      all      22/12  2: [0] -> 0 --> send element (r-2)%p to node (r+1)%p
   *
   *      *** round 4 ***
   *    0        1        2
   *
   *   all      all      all    0: done
   *   all      all      all    1: done
   *   all      all      all    2: done
   *
   * -> 4
   *     *** round 0 ***
   *    0        1        2        3
   *
   *   00       10       20       30       0: [1] -> 1
   *   01       11       21       31       1: [2] -> 2
   *   02       12       22       32       2: [3] -> 3
   *   03       13       23       33       3: [0] -> 0 --> send element (r+1)%p to node (r+1)%p
   *
   *      *** round 1 ***
   *    0        1        2        3
   *
   *   00+30    10       20       30       0: red(0), [0] -> 1
   *   01       11+01    21       31       1: red(1), [1] -> 2
   *   02       12       22+12    32       2: red(2), [2] -> 3
   *   03       13       23       33+23    3: red(3), [3] -> 0 --> reduce and send element (r+0)%p to node (r+1)%p
   *
   *      *** round 2 ***
   *    0        1        2        3
   *
   *   00/30    10+00/30 20       30       0: red(3), [3] -> 1
   *   01       11/01    21+11/01 31       1: red(0), [0] -> 2
   *   02       12       22/12    32+22/12 2: red(1), [1] -> 3
   *   03+33/23 13       23       33/23    3: red(2), [2] -> 0 --> reduce and send (r-1)%p to node (r+1)%p
   *
   *      *** round 3 ***
   *    0        1        2        3
   *
   *   00/30    10/00/30 all      30       0: red(2), [2] -> 1
   *   01       11/01    21/11/01 all      1: red(3), [3] -> 2
   *   all      12       22/12    32/22/12 2: red(0), [0] -> 3
   *   03/33/23 all      23       33/23    3: red(1), [1] -> 0 --> reduce and send (r-2)%p to node (r+1)%p
   *
   *      *** round 4 ***
   *    0        1        2        3
   *
   *   00/30    10/00/30 all      all      0: [1] -> 1
   *   all      11/01    21/11/01 all      1: [2] -> 2
   *   all      all      22/12    32/22/12 2: [3] -> 3
   *   03/33/23 all      all      33/23    3: [0] -> 0 --> receive and send element (r+1)%p to node (r+1)%p
   *
   *      *** round 5 ***
   *    0        1        2        3
   *
   *   all      10/00/30 all      all      0: [0] -> 1
   *   all      all      21/11/01 all      1: [1] -> 2
   *   all      all      all      32/22/12 2: [3] -> 3
   *   03/33/23 all      all      all      3: [4] -> 4 --> receive and send element (r-0)%p to node (r+1)%p
   *
   *      *** round 6 ***
   *    0        1        2        3
   *
   *   all      all      all      all
   *   all      all      all      all
   *   all      all      all      all
   *   all      all      all      all     receive element (r-1)%p
   *
   *   2p-2 rounds ... every node does p-1 reductions and p-1 sends
   *
   */
  /* first p-1 rounds are reductions */
  for (int round = 0 ; round < p - 1 ; ++round) {
    int selement = (r+1-round + 2*p /*2*p avoids negative mod*/)%p; /* the element I am sending */
    size_t soffset = segoffsets[selement]*(size_t)ext;
    int relement = (r-round + 2*p /*2*p avoids negative mod*/)%p; /* the element that I receive from my neighbor */
    size_t roffset = segoffsets[relement]*(size_t)ext;

    /* first message come out of sendbuf */
    if (round == 0) {
      res = NBC_Sched_send ((char *) sendbuf + soffset, false, segsizes[selement], datatype, speer,
                            schedule, false);
    } else {
      res = NBC_Sched_send ((char *) recvbuf + soffset, false, segsizes[selement], datatype, speer,
                            schedule, false);
    }

    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      goto free_and_return;
    }
    if( recvbuf != sendbuf ) {  /* check for MPI_IN_PLACE */
        res = NBC_Sched_recv ((char *) recvbuf + roffset, false, segsizes[relement], datatype, rpeer,
                              schedule, true);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          goto free_and_return;
        }
        res = NBC_Sched_op ((char *) sendbuf + roffset, false, (char *) recvbuf + roffset, false,
                             segsizes[relement], datatype, op, schedule, true);
    } else {
        res = NBC_Sched_recv ((char *) tmpbuf, false, segsizes[relement], datatype, rpeer,
                              schedule, true);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          goto free_and_return;
        }
        res = NBC_Sched_op ((char *) tmpbuf, false, (char *) recvbuf + roffset, false,
                             segsizes[relement], datatype, op, schedule, true);
    }
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      goto free_and_return;
    }
  }
  for (int round = p - 1 ; round < 2 * p - 2 ; ++round) {
    int selement = (r+1-round + 2*p /*2*p avoids negative mod*/)%p; /* the element I am sending */
    size_t soffset = segoffsets[selement]*(size_t)ext;
    int relement = (r-round + 2*p /*2*p avoids negative mod*/)%p; /* the element that I receive from my neighbor */
    size_t roffset = segoffsets[relement]*(size_t)ext;

    res = NBC_Sched_send ((char *) recvbuf + soffset, false, segsizes[selement], datatype, speer,
                          schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      break;
    }
    res = NBC_Sched_recv ((char *) recvbuf + roffset, false, segsizes[relement], datatype, rpeer,
                          schedule, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      break;
    }
  }
free_and_return:
  free (segsizes);

  return res;
}

static inline int allred_sched_linear(int rank, int rsize, const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
				      ptrdiff_t gap, MPI_Op op, int ext, int size, NBC_Schedule *schedule, void *tmpbuf) {
  int res;

  if (0 == count) {
    return OMPI_SUCCESS;
  }

  /* send my data to the remote root */
  res = NBC_Sched_send (sendbuf, false, count, datatype, 0, schedule, false);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    return res;
  }

  /* recv my data to the remote root */
  if (0 != rank || 1 ==(rsize%2)) {
    res = NBC_Sched_recv (recvbuf, false, count, datatype, 0, schedule, false);
  } else {
    res = NBC_Sched_recv ((void *)(-gap), true, count, datatype, 0, schedule, false);
  }
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    return res;
  }

  if (0 == rank) {
    char *rbuf, *lbuf, *buf;
    int tmprbuf, tmplbuf;

    res = NBC_Sched_barrier (schedule);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }

    /* ensure the result ends up in recvbuf */
    if (0 == (rsize%2)) {
      lbuf = (void *)(-gap);
      tmplbuf = true;
      rbuf = recvbuf;
      tmprbuf = false;
    } else {
      rbuf = (void *)(-gap);
      tmprbuf = true;
      lbuf = recvbuf;
      tmplbuf = false;
    }

    /* get data from remote peers and reduce */
    for (int rpeer = 1 ; rpeer < rsize ; ++rpeer) {
      res = NBC_Sched_recv (rbuf, tmprbuf, count, datatype, rpeer, schedule, true);
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

    /* exchange our result with the remote root (each root will broadcast to the other's peers) */
    res = NBC_Sched_recv ((void *)(-gap), true, count, datatype, 0, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }

    /* wait for data from remote root */
    res = NBC_Sched_send (recvbuf, false, count, datatype, 0, schedule, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }

    /* broadcast the result to all remote peers */
    for (int rpeer = 1 ; rpeer < rsize ; ++rpeer) {
      res = NBC_Sched_send ((void *)(-gap), true, count, datatype, rpeer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
  }

  return OMPI_SUCCESS;
}

/*
 * allred_sched_redscat_allgather:
 *
 * Description: an implementation of Rabenseifner's Allreduce algorithm [1, 2].
 *   [1] Rajeev Thakur, Rolf Rabenseifner and William Gropp.
 *       Optimization of Collective Communication Operations in MPICH //
 *       The Int. Journal of High Performance Computing Applications. Vol 19,
 *       Issue 1, pp. 49--66.
 *   [2] http://www.hlrs.de/mpi/myreduce.html.
 *
 * This algorithm is a combination of a reduce-scatter implemented with
 * recursive vector halving and recursive distance doubling, followed either
 * by an allgather implemented with recursive doubling.
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
 * Step 3. An allgather is performed by using recursive vector doubling and
 * distance halving. All exchanges are executed in reverse order relative
 * to recursive doubling on previous step. If the number of processes is not
 * a power of two, the total result vector must be sent to the r processes
 * that were removed in the first step.
 *
 * Limitations:
 *   count >= 2^{\floor{\log_2 p}}
 *   commutative operations only
 *   intra-communicators only
 *
 * Memory requirements (per process):
 *   count * typesize + 4 * \log_2(p) * sizeof(int) = O(count)
 *
 * Schedule length (rounds): O(\log(p))
 */
static inline int allred_sched_redscat_allgather(
    int rank, int comm_size, int count, MPI_Datatype datatype, ptrdiff_t gap,
    const void *sbuf, void *rbuf, MPI_Op op, char inplace,
    NBC_Schedule *schedule, void *tmpbuf, struct ompi_communicator_t *comm)
{
    int res = OMPI_SUCCESS;
    int *rindex = NULL, *rcount = NULL, *sindex = NULL, *scount = NULL;
     /* Find nearest power-of-two less than or equal to comm_size */
    int nsteps = opal_hibit(comm_size, comm->c_cube_dim + 1);   /* ilog2(comm_size) */
    int nprocs_pof2 = 1 << nsteps;                              /* flp2(comm_size) */
     if (!inplace) {
        res = NBC_Sched_copy((char *)sbuf, false, count, datatype,
                             rbuf, false, count, datatype, schedule, true);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
    }
    char *tmp_buf = (char *)tmpbuf - gap;
    ptrdiff_t lb, extent;
    ompi_datatype_get_extent(datatype, &lb, &extent);
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
     * 0 to 2^{\floor{\log_2 p}} - 1.
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
            res = NBC_Sched_send(rbuf, false, count_lhalf, datatype, rank - 1,
                                 schedule, false);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
             res = NBC_Sched_recv(tmp_buf + (ptrdiff_t)count_lhalf * extent,
                                 false, count_rhalf, datatype, rank - 1, schedule, true);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
             res = NBC_Sched_op(tmp_buf + (ptrdiff_t)count_lhalf * extent,
                               false, (char *)rbuf + (ptrdiff_t)count_lhalf * extent,
                               false, count_rhalf, datatype, op, schedule, true);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
             /* Send the right half to the left neighbor */
            res = NBC_Sched_send((char *)rbuf + (ptrdiff_t)count_lhalf * extent,
                                 false, count_rhalf, datatype, rank - 1, schedule, true);
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
                                 false, count_rhalf, datatype, rank + 1, schedule, false);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
             res = NBC_Sched_recv(tmp_buf, false, count_lhalf, datatype, rank + 1,
                                 schedule, true);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
             res = NBC_Sched_op(tmp_buf, false, rbuf, false, count_lhalf,
                               datatype, op, schedule, true);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
             /* Recv the right half from the right neighbor */
            res = NBC_Sched_recv((char *)rbuf + (ptrdiff_t)count_lhalf * extent,
                                 false, count_rhalf, datatype, rank + 1, schedule, true);
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
    rindex = malloc(sizeof(*rindex) * nsteps);
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
                                 false, scount[step], datatype, dest, schedule, false);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
            res = NBC_Sched_recv((char *)tmp_buf + (ptrdiff_t)rindex[step] * extent,
                                 false, rcount[step], datatype, dest, schedule, true);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
             /* Local reduce: rbuf[] = tmp_buf[] <op> rbuf[] */
            res = NBC_Sched_op((char *)tmp_buf + (ptrdiff_t)rindex[step] * extent,
                               false, (char *)rbuf + (ptrdiff_t)rindex[step] * extent,
                               false, rcount[step], datatype, op, schedule, true);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
             /* Move the current window to the received message */
            if (step + 1 < nsteps) {
                rindex[step + 1] = rindex[step];
                sindex[step + 1] = rindex[step];
                wsize = rcount[step];
                step++;
            }
        }
        /*
         * Assertion: each process has 1 / p' of the total reduction result:
         * rcount[nsteps - 1] elements in the rbuf[rindex[nsteps - 1], ...].
         */
         /*
         * Step 3. Allgather by the recursive doubling algorithm.
         * Each process has 1 / p' of the total reduction result:
         * rcount[nsteps - 1] elements in the rbuf[rindex[nsteps - 1], ...].
         * All exchanges are executed in reverse order relative
         * to recursive doubling (previous step).
         */
        step = nsteps - 1;
         for (int mask = nprocs_pof2 >> 1; mask > 0; mask >>= 1) {
            int vdest = vrank ^ mask;
            /* Translate vdest virtual rank to real rank */
            int dest = (vdest < nprocs_rem) ? vdest * 2 : vdest + nprocs_rem;
             /*
             * Send rcount[step] elements from rbuf[rindex[step]...]
             * Recv scount[step] elements to rbuf[sindex[step]...]
             */
            res = NBC_Sched_send((char *)rbuf + (ptrdiff_t)rindex[step] * extent,
                                 false, rcount[step], datatype, dest, schedule, false);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
            res = NBC_Sched_recv((char *)rbuf + (ptrdiff_t)sindex[step] * extent,
                                 false, scount[step], datatype, dest, schedule, true);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
            step--;
        }
    }
     /*
     * Step 4. Send total result to excluded odd ranks.
     */
    if (rank < 2 * nprocs_rem) {
        if (rank % 2 != 0) {
            /* Odd process -- recv result from rank - 1 */
            res = NBC_Sched_recv(rbuf, false, count, datatype, rank - 1, schedule, false);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
        } else {
            /* Even process -- send result to rank + 1 */
            res = NBC_Sched_send(rbuf, false, count, datatype, rank + 1, schedule, false);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
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

int ompi_coll_libnbc_allreduce_init(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                                    struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                    mca_coll_base_module_t *module) {
    int res = nbc_allreduce_init(sendbuf, recvbuf, count, datatype, op,
                                 comm, request, module, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
    }

    return OMPI_SUCCESS;
}

int ompi_coll_libnbc_allreduce_inter_init(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                                          struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                          mca_coll_base_module_t *module) {
    int res = nbc_allreduce_inter_init(sendbuf, recvbuf, count, datatype, op,
                                       comm, request, module, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
    }

    return OMPI_SUCCESS;
}
