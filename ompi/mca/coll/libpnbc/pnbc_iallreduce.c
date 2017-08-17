/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2013-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * Copyright (c) 2017      Ian Bradley Morgan and Anthony Skjellum. All 
 *                         rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 
 * 3. Neither the name of the copyright holder nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "pnbc_internal.h"
#include "ompi/communicator/communicator.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/op/op.h"

#include <assert.h>

static inline int allred_sched_diss(int rank, int p, int count, MPI_Datatype datatype, ptrdiff_t gap, const void *sendbuf,
                                    void *recvbuf, MPI_Op op, char inplace, PNBC_Schedule *schedule, PNBC_Handle *handle);
static inline int allred_sched_ring(int rank, int p, int count, MPI_Datatype datatype, const void *sendbuf,
                                    void *recvbuf, MPI_Op op, int size, int ext, PNBC_Schedule *schedule,
                                    PNBC_Handle *handle);
static inline int allred_sched_linear(int rank, int p, const void *sendbuf, void *recvbuf, int count,
                                      MPI_Datatype datatype, ptrdiff_t gap, MPI_Op op, int ext, int size,
                                      PNBC_Schedule *schedule, PNBC_Handle *handle);


int ompi_coll_libpnbc_iallreduce_init(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                                struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                struct mca_coll_base_module_2_2_0_t *module)
{

  PNBC_DEBUG(5, "*** IALLREDUCE_INIT ***\n");
  PNBC_DEBUG(5, "*** IALLREDUCE_INIT _send buffer size = %u  | receive buffer size =%u  | count=%d **\n", sizeof(sendbuf), sizeof(recvbuf), count);

  int rank, p, res;
  ptrdiff_t ext, lb;
  PNBC_Schedule *schedule;
  size_t size;

  enum { PNBC_ARED_BINOMIAL, PNBC_ARED_RING } alg;
  char inplace;
  PNBC_Handle *handle = NULL;
  ompi_coll_libpnbc_module_t *libpnbc_module = (ompi_coll_libpnbc_module_t*) module;
  ptrdiff_t span, gap;

  PNBC_IN_PLACE(sendbuf, recvbuf, inplace);

  rank = ompi_comm_rank (comm);
  p = ompi_comm_size (comm);

  res = ompi_datatype_get_extent(datatype, &lb, &ext);
  if (OMPI_SUCCESS != res) {
    PNBC_Error ("MPI Error in ompi_datatype_type_extent() (%i)", res);
    return res;
  }

  res = ompi_datatype_type_size (datatype, &size);
  if (OMPI_SUCCESS != res) {
    PNBC_Error ("MPI Error in ompi_datatype_type_size() (%i)", res);
    return res;
  }

  /*
   * FIXME - this is an initialisation function
   *         ** it must not do any real work **
   *         this should instead create a short
   *         schedule with just PNBC_Sched_copy
   *         Move this into algorithm selection
   */
  if (1 == p) {
    if (!inplace) {
      /* for a single node - copy data to receivebuf */
      res = PNBC_Copy(sendbuf, count, datatype, recvbuf, count, datatype, comm);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        PNBC_Return_handle (handle);
        return res;
      }
    }
    *request = &ompi_request_empty;
    return OMPI_SUCCESS;
  }

  res = PNBC_Init_handle (comm, &handle, libpnbc_module);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    return res;
  }

  span = opal_datatype_span(&datatype->super, count, &gap);
  handle->tmpbuf = malloc (span);
  if (OPAL_UNLIKELY(NULL == handle->tmpbuf)) {
    PNBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  /* algorithm selection */
  if(p < 4 || size*count < 65536 || !ompi_op_is_commute(op) || inplace) {
    alg = PNBC_ARED_BINOMIAL;
  } else {
    alg = PNBC_ARED_RING;
  }

  schedule = OBJ_NEW(PNBC_Schedule);
  if (NULL == schedule) {
    PNBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  /* ensure the schedule is released with the handle on error */
  handle->schedule = schedule;

  PNBC_DEBUG(7, "** finalizing request schedule **\n");

  switch(alg) {
    case PNBC_ARED_BINOMIAL:
      res = allred_sched_diss(rank, p, count, datatype, gap, sendbuf, recvbuf, op, inplace, schedule, handle);
      break;
    case PNBC_ARED_RING:
      res = allred_sched_ring(rank, p, count, datatype, sendbuf, recvbuf, op, size, ext, schedule, handle);
      break;
  }

  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    PNBC_Return_handle (handle);
    return res;
  }

  res = PNBC_Sched_commit(schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    PNBC_Return_handle (handle);
    return res;
  }

  handle->schedule = schedule;

  *request = (ompi_request_t *) handle;

  PNBC_DEBUG(7, "** allreduce_init complete **\n");

  return OMPI_SUCCESS;
}

int ompi_coll_libpnbc_iallreduce_inter(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                                      struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                      struct mca_coll_base_module_2_2_0_t *module)
{
  int rank, res, rsize;
  size_t size;
  MPI_Aint ext;
  PNBC_Schedule *schedule;
  PNBC_Handle *handle;
  ompi_coll_libpnbc_module_t *libpnbc_module = (ompi_coll_libpnbc_module_t*) module;
  ptrdiff_t span, gap;

  rank = ompi_comm_rank (comm);
  rsize = ompi_comm_remote_size (comm);

  res = ompi_datatype_type_extent(datatype, &ext);
  if (MPI_SUCCESS != res) {
    PNBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
    return res;
  }

  res = ompi_datatype_type_size(datatype, &size);
  if (MPI_SUCCESS != res) {
    PNBC_Error("MPI Error in ompi_datatype_type_size() (%i)", res);
    return res;
  }

  res = PNBC_Init_handle (comm, &handle, libpnbc_module);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    return res;
  }

  span = opal_datatype_span(&datatype->super, count, &gap);
  handle->tmpbuf = malloc (span);
  if (OPAL_UNLIKELY(NULL == handle->tmpbuf)) {
    PNBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  schedule = OBJ_NEW(PNBC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    PNBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  /* ensure the schedule is released with the handle on error */
  handle->schedule = schedule;

  res = allred_sched_linear (rank, rsize, sendbuf, recvbuf, count, datatype, gap, op,
                             ext, size, schedule, handle);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    PNBC_Return_handle (handle);
    return res;
  }

  res = PNBC_Sched_commit(schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    PNBC_Return_handle (handle);
    return res;
  }

  /*
   * FIXME - if this is a persistent initialisation function
   *         then the schedule must not be started yet
   *         if this is a nonblocking collective function
   *         then we should let the NBC module provide it
   *         i.e. this function should not be in this module
   */
  res = PNBC_Start_internal(handle, schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    PNBC_Return_handle (handle);
    return res;
  }

  *request = (ompi_request_t *) handle;

  /* tmpbuf is freed with the handle */
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
                                    MPI_Op op, char inplace, PNBC_Schedule *schedule, PNBC_Handle *handle) {
  int root, vrank, maxr, vpeer, peer, res;
  char *rbuf, *lbuf, *buf;
  int tmprbuf, tmplbuf;

  root = 0; /* this makes the code for ireduce and iallreduce nearly identical - could be changed to improve performance */
  RANK2VRANK(rank, vrank, root);
  maxr = (int)ceil((log((double)p)/LOG2));
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
        res = PNBC_Copy(rbuf, count, datatype, ((char *)handle->tmpbuf) - gap, count, datatype, MPI_COMM_SELF);
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
        res = PNBC_Sched_recv (rbuf, tmprbuf, count, datatype, peer, schedule, true);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          return res;
        }

        /* this cannot be done until handle->tmpbuf is unused :-( so barrier after the op */
        if (firstred && !inplace) {
          /* perform the reduce with the senbuf */
          res = PNBC_Sched_op (sendbuf, false, rbuf, tmprbuf, count, datatype, op, schedule, true);
          firstred = 0;
        } else {
          /* perform the reduce in my local buffer */
          res = PNBC_Sched_op (lbuf, tmplbuf, rbuf, tmprbuf, count, datatype, op, schedule, true);
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
        res = PNBC_Sched_send (sendbuf, false, count, datatype, peer, schedule, false);
      } else {
        /* and the recvbuf in all remaining rounds */
        res = PNBC_Sched_send (lbuf, tmplbuf, count, datatype, peer, schedule, false);
      }

      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }

      /* leave the game */
      break;
    }
  }

  /* this is the Bcast part - copied with minor changes from pnbc_ibcast.c
   * changed: buffer -> recvbuf  */
  RANK2VRANK(rank, vrank, root);

  /* receive from the right hosts  */
  if (vrank != 0) {
    for (int r = 0; r < maxr ; ++r) {
      if ((vrank >= (1 << r)) && (vrank < (1 << (r + 1)))) {
        VRANK2RANK(peer, vrank - (1 << r), root);
        res = PNBC_Sched_recv (recvbuf, false, count, datatype, peer, schedule, false);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          return res;
        }
      }
    }

    res = PNBC_Sched_barrier (schedule);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
  }

  if (0 == vrank) assert(lbuf == recvbuf);
  /* now send to the right hosts */
  for (int r = 0; r < maxr; ++r) {
    if (((vrank + (1 << r) < p) && (vrank < (1 << r))) || (vrank == 0)) {
      VRANK2RANK(peer, vrank + (1 << r), root);
      res = PNBC_Sched_send (recvbuf, false, count, datatype, peer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
  }

  /* end of the bcast */
  return OMPI_SUCCESS;
}

static inline int allred_sched_ring (int r, int p, int count, MPI_Datatype datatype, const void *sendbuf, void *recvbuf, MPI_Op op,
                                     int size, int ext, PNBC_Schedule *schedule, PNBC_Handle *handle) {
  int segsize, *segsizes, *segoffsets; /* segment sizes and offsets per segment (number of segments == number of nodes */
  int speer, rpeer; /* send and recvpeer */
  int res = OMPI_SUCCESS;

  if (count == 0) {
    return OMPI_SUCCESS;
  }

  segsizes = (int *) malloc (sizeof (int) * p);
  segoffsets = (int *) malloc (sizeof (int) * p);
  if (NULL == segsizes || NULL == segoffsets) {
    free (segsizes);
    free (segoffsets);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  segsize = (count + p - 1) / p; /* size of the segments */

  segoffsets[0] = 0;
  for (int i = 0, mycount = count ; i < p ; ++i) {
    mycount -= segsize;
    segsizes[i] = segsize;
    if (mycount < 0) {
      segsizes[i] = segsize + mycount;
      mycount = 0;
    }

    if (i) {
      segoffsets[i] = segoffsets[i-1] + segsizes[i-1];
    }
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
    int soffset = segoffsets[selement]*ext;
    int relement = (r-round + 2*p /*2*p avoids negative mod*/)%p; /* the element that I receive from my neighbor */
    int roffset = segoffsets[relement]*ext;

    /* first message come out of sendbuf */
    if (round == 0) {
      res = PNBC_Sched_send ((char *) sendbuf + soffset, false, segsizes[selement], datatype, speer,
                            schedule, false);
    } else {
      res = PNBC_Sched_send ((char *) recvbuf + soffset, false, segsizes[selement], datatype, speer,
                            schedule, false);
    }

    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      break;
    }

    res = PNBC_Sched_recv ((char *) recvbuf + roffset, false, segsizes[relement], datatype, rpeer,
                          schedule, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      break;
    }

    res = PNBC_Sched_op ((char *) sendbuf + roffset, false, (char *) recvbuf + roffset, false,
                         segsizes[relement], datatype, op, schedule, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      break;
    }
  }

  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    free (segsizes);
    free (segoffsets);
    return res;
  }

  for (int round = p - 1 ; round < 2 * p - 2 ; ++round) {
    int selement = (r+1-round + 2*p /*2*p avoids negative mod*/)%p; /* the element I am sending */
    int soffset = segoffsets[selement]*ext;
    int relement = (r-round + 2*p /*2*p avoids negative mod*/)%p; /* the element that I receive from my neighbor */
    int roffset = segoffsets[relement]*ext;

    res = PNBC_Sched_send ((char *) recvbuf + soffset, false, segsizes[selement], datatype, speer,
                          schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      break;
    }

    res = PNBC_Sched_recv ((char *) recvbuf + roffset, false, segsizes[relement], datatype, rpeer,
                          schedule, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      break;
    }
  }

  free (segsizes);
  free (segoffsets);

  return res;
}

static inline int allred_sched_linear(int rank, int rsize, const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
				      ptrdiff_t gap, MPI_Op op, int ext, int size, PNBC_Schedule *schedule, PNBC_Handle *handle) {
  int res;

  if (0 == count) {
    return OMPI_SUCCESS;
  }

  /* send my data to the remote root */
  res = PNBC_Sched_send (sendbuf, false, count, datatype, 0, schedule, false);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    return res;
  }

  /* recv my data to the remote root */
  if (0 != rank || 1 ==(rsize%2)) {
    res = PNBC_Sched_recv (recvbuf, false, count, datatype, 0, schedule, false);
  } else {
    res = PNBC_Sched_recv ((void *)(-gap), true, count, datatype, 0, schedule, false);
  }
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    return res;
  }

  if (0 == rank) {
    char *rbuf, *lbuf, *buf;
    int tmprbuf, tmplbuf;

    res = PNBC_Sched_barrier (schedule);
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
      res = PNBC_Sched_recv (rbuf, tmprbuf, count, datatype, rpeer, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }

      res = PNBC_Sched_op (lbuf, tmplbuf, rbuf, tmprbuf, count, datatype, op, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
      /* swap left and right buffers */
      buf = rbuf; rbuf = lbuf ; lbuf = buf;
      tmprbuf ^= 1; tmplbuf ^= 1;
    }

    /* exchange our result with the remote root (each root will broadcast to the other's peers) */
    res = PNBC_Sched_recv ((void *)(-gap), true, count, datatype, 0, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }

    /* wait for data from remote root */
    res = PNBC_Sched_send (recvbuf, false, count, datatype, 0, schedule, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }

    /* broadcast the result to all remote peers */
    for (int rpeer = 1 ; rpeer < rsize ; ++rpeer) {
      res = PNBC_Sched_send ((void *)(-gap), true, count, datatype, rpeer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
  }

  return OMPI_SUCCESS;
}
