/*
 * Copyright (c) 2006       The Trustees of Indiana University and Indiana
 *                          University Research and Technology
 *                          Corporation.  All rights reserved.
 * Copyright (c) 2006       The Technical University of Chemnitz. All
 *                          rights reserved.
 * Copyright (c) 2013       Los Alamos National Security, LLC. All rights
 *                          reserved.
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "nbc_internal.h"
#include "ompi/communicator/communicator.h"
#include "ompi/datatype/ompi_datatype.h"

#include <assert.h>

static inline int allred_sched_diss(int rank, int p, int count, MPI_Datatype datatype, void *sendbuf, void *recvbuf, MPI_Op op, NBC_Schedule *schedule, NBC_Handle *handle);
static inline int allred_sched_ring(int rank, int p, int count, MPI_Datatype datatype, void *sendbuf, void *recvbuf, MPI_Op op, int size, int ext, NBC_Schedule *schedule, NBC_Handle *handle);
static inline int allred_sched_linear(int rank, int p, void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype, MPI_Op op, int ext, int size, NBC_Schedule *schedule, NBC_Handle *handle);

#ifdef NBC_CACHE_SCHEDULE
/* tree comparison function for schedule cache */
int NBC_Allreduce_args_compare(NBC_Allreduce_args *a, NBC_Allreduce_args *b, void *param) {

        if( (a->sendbuf == b->sendbuf) &&
      (a->recvbuf == b->recvbuf) &&
      (a->count == b->count) &&
      (a->datatype == b->datatype) &&
      (a->op == b->op) ) {
    return  0;
  }
        if( a->sendbuf < b->sendbuf ) {
    return -1;
        }
        return +1;
}
#endif

int ompi_coll_libnbc_iallreduce(void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                                struct ompi_communicator_t *comm, ompi_request_t ** request,
                                struct mca_coll_base_module_2_0_0_t *module)
{
  int rank, p, res;
  OPAL_PTRDIFF_TYPE ext, lb;
  NBC_Schedule *schedule;
  size_t size;
#ifdef NBC_CACHE_SCHEDULE
  NBC_Allreduce_args *args, *found, search;
#endif
  enum { NBC_ARED_BINOMIAL, NBC_ARED_RING } alg;
  char inplace;
  NBC_Handle *handle;
  ompi_coll_libnbc_request_t **coll_req = (ompi_coll_libnbc_request_t**) request;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

  NBC_IN_PLACE(sendbuf, recvbuf, inplace);

  res = NBC_Init_handle(comm, coll_req, libnbc_module);
  if(res != NBC_OK) { printf("Error in NBC_Init_handle(%i)\n", res); return res; }
  handle = (*coll_req);

  rank = ompi_comm_rank (comm);
  p = ompi_comm_size (comm);
  res = ompi_datatype_get_extent(datatype, &lb, &ext);
  if (OMPI_SUCCESS != res) { printf("MPI Error in MPI_Type_extent() (%i)\n", res); return res; }
  res = ompi_datatype_type_size (datatype, &size);
  if (OMPI_SUCCESS != res) { printf("MPI Error in MPI_Type_size() (%i)\n", res); return res; }

  handle->tmpbuf = malloc(ext*count);
  if(handle->tmpbuf == NULL) { printf("Error in malloc() (%i)\n", res); return NBC_OOR; }

  if((p == 1) && !inplace) {
    /* for a single node - copy data to receivebuf */
    res = NBC_Copy(sendbuf, count, datatype, recvbuf, count, datatype, comm);
    if (NBC_OK != res) { printf("Error in NBC_Copy() (%i)\n", res); return res; }
  }

  /* algorithm selection */
  if(p < 4 || size*count < 65536 || inplace) {
    alg = NBC_ARED_BINOMIAL;
  } else {
    alg = NBC_ARED_RING;
  }

#ifdef NBC_CACHE_SCHEDULE
  /* search schedule in communicator specific tree */
  search.sendbuf=sendbuf;
  search.recvbuf=recvbuf;
  search.count=count;
  search.datatype=datatype;
  search.op=op;
  found = (NBC_Allreduce_args*)hb_tree_search((hb_tree*)handle->comminfo->NBC_Dict[NBC_ALLREDUCE], &search);
  if(found == NULL) {
#endif
    schedule = (NBC_Schedule*)malloc(sizeof(NBC_Schedule));
    if (NULL == schedule) { printf("Error in malloc()\n"); return res; }

    res = NBC_Sched_create(schedule);
    if(res != NBC_OK) { printf("Error in NBC_Sched_create (%i)\n", res); return res; }

    switch(alg) {
      case NBC_ARED_BINOMIAL:
        res = allred_sched_diss(rank, p, count, datatype, sendbuf, recvbuf, op, schedule, handle);
        break;
      case NBC_ARED_RING:
        res = allred_sched_ring(rank, p, count, datatype, sendbuf, recvbuf, op, size, ext, schedule, handle);
        break;
    }
    if (NBC_OK != res) { printf("Error in Schedule creation() (%i)\n", res); return res; }

    res = NBC_Sched_commit(schedule);
    if(res != NBC_OK) { free(handle->tmpbuf); printf("Error in NBC_Sched_commit() (%i)\n", res); return res; }

#ifdef NBC_CACHE_SCHEDULE
    /* save schedule to tree */
    args = (NBC_Allreduce_args*)malloc(sizeof(NBC_Allreduce_args));
    args->sendbuf=sendbuf;
    args->recvbuf=recvbuf;
    args->count=count;
    args->datatype=datatype;
    args->op=op;
    args->schedule=schedule;
    res = hb_tree_insert ((hb_tree*)handle->comminfo->NBC_Dict[NBC_ALLREDUCE], args, args, 0);
    if(res != 0) printf("error in dict_insert() (%i)\n", res);
    /* increase number of elements for A2A */
    if(++handle->comminfo->NBC_Dict_size[NBC_ALLREDUCE] > NBC_SCHED_DICT_UPPER) {
      NBC_SchedCache_dictwipe((hb_tree*)handle->comminfo->NBC_Dict[NBC_ALLREDUCE], &handle->comminfo->NBC_Dict_size[NBC_ALLREDUCE]);
    }
  } else {
    /* found schedule */
    schedule=found->schedule;
  }
#endif

  res = NBC_Start(handle, schedule);
  if(res != NBC_OK) { free(handle->tmpbuf); printf("Error in NBC_Start() (%i)\n", res); return res; }

  /* tmpbuf is freed with the handle */
  return NBC_OK;
}

int ompi_coll_libnbc_iallreduce_inter(void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                                      struct ompi_communicator_t *comm, ompi_request_t ** request,
                                      struct mca_coll_base_module_2_0_0_t *module)
{
  int rank, res, size, rsize;
  MPI_Aint ext;
  NBC_Schedule *schedule;
  NBC_Handle *handle;
  ompi_coll_libnbc_request_t **coll_req = (ompi_coll_libnbc_request_t**) request;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

  res = NBC_Init_handle(comm, coll_req, libnbc_module);
  if(res != NBC_OK) { printf("Error in NBC_Init_handle(%i)\n", res); return res; }
  handle = (*coll_req);
  res = MPI_Comm_rank(comm, &rank);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_rank() (%i)\n", res); return res; }
  res = MPI_Comm_remote_size(comm, &rsize);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_remote_size() (%i)\n", res); return res; }
  res = MPI_Type_extent(datatype, &ext);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Type_extent() (%i)\n", res); return res; }
  res = MPI_Type_size(datatype, &size);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Type_size() (%i)\n", res); return res; }

  handle->tmpbuf = malloc(ext*count);
  if(handle->tmpbuf == NULL) { printf("Error in malloc() (%i)\n", res); return NBC_OOR; }

  schedule = (NBC_Schedule*)malloc(sizeof(NBC_Schedule));
  if (NULL == schedule) { printf("Error in malloc()\n"); return res; }

  res = NBC_Sched_create(schedule);
  if(res != NBC_OK) { printf("Error in NBC_Sched_create (%i)\n", res); return res; }

  res = allred_sched_linear(rank, rsize, sendbuf, recvbuf, count, datatype, op, ext, size, schedule, handle);
  if (NBC_OK != res) { printf("Error in Schedule creation() (%i)\n", res); return res; }

  res = NBC_Sched_commit(schedule);
  if(res != NBC_OK) { free(handle->tmpbuf); printf("Error in NBC_Sched_commit() (%i)\n", res); return res; }

  res = NBC_Start(handle, schedule);
  if(res != NBC_OK) { free(handle->tmpbuf); printf("Error in NBC_Start() (%i)\n", res); return res; }

  /* tmpbuf is freed with the handle */
  return NBC_OK;
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
static inline int allred_sched_diss(int rank, int p, int count, MPI_Datatype datatype, void *sendbuf, void *recvbuf, MPI_Op op, NBC_Schedule *schedule, NBC_Handle *handle) {
  int root, vrank, r, maxr, firstred, vpeer, peer, res;

  root = 0; /* this makes the code for ireduce and iallreduce nearly identical - could be changed to improve performance */
  RANK2VRANK(rank, vrank, root);
  maxr = (int)ceil((log(p)/LOG2));

  firstred = 1;
  for(r=1; r<=maxr; r++) {
    if((vrank % (1<<r)) == 0) {
      /* we have to receive this round */
      vpeer = vrank + (1<<(r-1));
      VRANK2RANK(peer, vpeer, root)
      if(peer<p) {
        res = NBC_Sched_recv(0, true, count, datatype, peer, schedule);
        if(res != NBC_OK) { free(handle->tmpbuf); printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
        /* we have to wait until we have the data */
        res = NBC_Sched_barrier(schedule);
        if(res != NBC_OK) { free(handle->tmpbuf); printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }
        if(firstred && MPI_IN_PLACE != sendbuf) {
          /* perform the reduce with the senbuf */
          res = NBC_Sched_op(recvbuf, false, sendbuf, false, 0, true, count, datatype, op, schedule);
          firstred = 0;
        } else {
          /* perform the reduce in my local buffer */
          res = NBC_Sched_op(recvbuf, false, recvbuf, false, 0, true, count, datatype, op, schedule);
        }
        if(res != NBC_OK) { free(handle->tmpbuf); printf("Error in NBC_Sched_op() (%i)\n", res); return res; }
        /* this cannot be done until handle->tmpbuf is unused :-( */
        res = NBC_Sched_barrier(schedule);
        if(res != NBC_OK) { free(handle->tmpbuf); printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }
      }
    } else {
      /* we have to send this round */
      vpeer = vrank - (1<<(r-1));
      VRANK2RANK(peer, vpeer, root)
      if(firstred && MPI_IN_PLACE != sendbuf) {
        /* we have to use the sendbuf in the first round .. */
        res = NBC_Sched_send(sendbuf, false, count, datatype, peer, schedule);
      } else {
        /* and the recvbuf in all remeining rounds */
        res = NBC_Sched_send(recvbuf, false, count, datatype, peer, schedule);
      }
      if(res != NBC_OK) { free(handle->tmpbuf); printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
      /* leave the game */
      break;
    }
  }

  /* this is the Bcast part - copied with minor changes from nbc_ibcast.c
   * changed: buffer -> recvbuf  */
  RANK2VRANK(rank, vrank, root);

  /* receive from the right hosts  */
  if(vrank != 0) {
    for(r=0; r<maxr; r++) {
      if((vrank >= (1<<r)) && (vrank < (1<<(r+1)))) {
        VRANK2RANK(peer, vrank-(1<<r), root);
        res = NBC_Sched_recv(recvbuf, false, count, datatype, peer, schedule);
        if(res != NBC_OK) { free(handle->tmpbuf); printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
      }
    }
    res = NBC_Sched_barrier(schedule);
    if(NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }
  }

  /* now send to the right hosts */
  for(r=0; r<maxr; r++) {
    if(((vrank + (1<<r) < p) && (vrank < (1<<r))) || (vrank == 0)) {
      VRANK2RANK(peer, vrank+(1<<r), root);
      res = NBC_Sched_send(recvbuf, false, count, datatype, peer, schedule);
      if(res != NBC_OK) { free(handle->tmpbuf); printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
    }
  }
  /* end of the bcast */

  return NBC_OK;
}

static inline int allred_sched_ring(int r, int p, int count, MPI_Datatype datatype, void *sendbuf, void *recvbuf, MPI_Op op, int size, int ext, NBC_Schedule *schedule, NBC_Handle *handle) {
  int i; /* runner */
  int segsize, *segsizes, *segoffsets; /* segment sizes and offsets per segment (number of segments == number of nodes */
  int speer, rpeer; /* send and recvpeer */

  if(count == 0) return NBC_OK;

  {
    int mycount; /* temporary */
    segsizes = (int*)malloc(sizeof(int)*p);
    segoffsets = (int*)malloc(sizeof(int)*p);
    segsize = count/p; /* size of the segments */
    if(count%p != 0) segsize++;
    mycount = count;
    segoffsets[0] = 0;
    for(i = 0; i<p;i++) {
      mycount -= segsize;
      segsizes[i] = segsize;
      if(mycount < 0) {
        segsizes[i] = segsize+mycount;
        mycount = 0;
      }
      if(i) segoffsets[i] = segoffsets[i-1] + segsizes[i-1];
      /*if(!r) printf("count: %i, (%i) size: %i, offset: %i\n", count, i, segsizes[i], segoffsets[i]); */
    }
  }

  /* reduce peers */
  speer = (r+1)%p;
  rpeer = (r-1+p)%p;

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
  {
    int round = 0;
    /* first p-1 rounds are reductions */
    do {
      int selement = (r+1-round + 2*p /*2*p avoids negative mod*/)%p; /* the element I am sending */
      int soffset = segoffsets[selement]*ext;
      int relement = (r-round + 2*p /*2*p avoids negative mod*/)%p; /* the element that I receive from my neighbor */
      int roffset = segoffsets[relement]*ext;

      /* first message come out of sendbuf */
      if(round == 0) {
        NBC_Sched_send((char*)sendbuf+soffset, false, segsizes[selement], datatype, speer, schedule);
      } else {
        NBC_Sched_send((char*)recvbuf+soffset, false, segsizes[selement], datatype, speer, schedule);
      }
      NBC_Sched_recv((char*)recvbuf+roffset, false, segsizes[relement], datatype, rpeer, schedule);

      NBC_Sched_barrier(schedule);
      NBC_Sched_op((char*)recvbuf+roffset, false, (char*)sendbuf+roffset, false, (char*)recvbuf+roffset, false, segsizes[relement], datatype, op, schedule);
      NBC_Sched_barrier(schedule);

      round++;
    } while(round < p-1);

    do {
      int selement = (r+1-round + 2*p /*2*p avoids negative mod*/)%p; /* the element I am sending */
      int soffset = segoffsets[selement]*ext;
      int relement = (r-round + 2*p /*2*p avoids negative mod*/)%p; /* the element that I receive from my neighbor */
      int roffset = segoffsets[relement]*ext;

      NBC_Sched_send((char*)recvbuf+soffset, false, segsizes[selement], datatype, speer, schedule);
      NBC_Sched_recv((char*)recvbuf+roffset, false, segsizes[relement], datatype, rpeer, schedule);
      NBC_Sched_barrier(schedule);
      round++;
    } while (round < 2*p-2);
  }

  return NBC_OK;
}

static inline int allred_sched_linear(int rank, int rsize, void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
				      MPI_Op op, int ext, int size, NBC_Schedule *schedule, NBC_Handle *handle) {
  int res, rpeer;

  if(count == 0) return NBC_OK;

  /* send my data to the remote root */
  res = NBC_Sched_send (sendbuf, false, count, datatype, 0, schedule);
  if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }

  res = NBC_Sched_recv (recvbuf, false, count, datatype, 0, schedule);
  if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }

  if (0 == rank) {
    /* wait for data from the remote root */
    res = NBC_Sched_barrier (schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }

    /* get data from remote peers and reduce */
    for (rpeer = 1 ; rpeer < rsize ; ++rpeer) {
      res = NBC_Sched_recv (0, true, count, datatype, rpeer, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }

      res = NBC_Sched_barrier(schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }

      res = NBC_Sched_op (recvbuf, false, 0, true, recvbuf, false, count, datatype, op, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_op() (%i)\n", res); return res; }

      res = NBC_Sched_barrier(schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }
    }

    /* exchange our result with the remote root (each root will broadcast to the other's peers) */
    res = NBC_Sched_recv (0, true, count, datatype, 0, schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }

    res = NBC_Sched_send (recvbuf, false, count, datatype, 0, schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }

    /* wait for data from remote root */
    res = NBC_Sched_barrier(schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }

    /* broadcast the result to all remote peers */
    for (rpeer = 1 ; rpeer < rsize ; ++rpeer) {
      res = NBC_Sched_send (0, true, count, datatype, rpeer, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
    }
  }

  return NBC_OK;
}
