/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
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

#include "opal/include/opal/align.h"
#include "ompi/op/op.h"

#include "pnbc_internal.h"

static inline int red_sched_binomial (int rank, int p, int root, const void *sendbuf, void *redbuf, char tmpredbuf, int count, MPI_Datatype datatype,
                                      MPI_Op op, char inplace, PNBC_Schedule *schedule, PNBC_Handle *handle);
static inline int red_sched_chain (int rank, int p, int root, const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
                                   MPI_Op op, int ext, size_t size, PNBC_Schedule *schedule, PNBC_Handle *handle, int fragsize);

static inline int red_sched_linear (int rank, int rsize, int root, const void *sendbuf, void *recvbuf, void *tmpbuf, int count, MPI_Datatype datatype,
                                    MPI_Op op, PNBC_Schedule *schedule, PNBC_Handle *handle);

/* the non-blocking reduce */
int ompi_coll_libpnbc_ireduce_init(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype,
                             MPI_Op op, int root, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                             struct mca_coll_base_module_2_2_0_t *module) {
  int rank, p, res, segsize;
  size_t size;
  MPI_Aint ext;
  PNBC_Schedule *schedule;
  char *redbuf=NULL, inplace;
  char tmpredbuf = 0;
  enum { PNBC_RED_BINOMIAL, PNBC_RED_CHAIN } alg;
  PNBC_Handle *handle;
  ompi_coll_libpnbc_module_t *libpnbc_module = (ompi_coll_libpnbc_module_t*) module;
  ptrdiff_t span, gap;

  PNBC_IN_PLACE(sendbuf, recvbuf, inplace);

  rank = ompi_comm_rank (comm);
  p = ompi_comm_size (comm);

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

  /* only one node -> copy data */
  if (p == 1) {
    if (!inplace) {
      res = PNBC_Copy (sendbuf, count, datatype, recvbuf, count, datatype, comm);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
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

  /* algorithm selection */
  if (p > 4 || size * count < 65536 || !ompi_op_is_commute(op)) {
    alg = PNBC_RED_BINOMIAL;
    if(rank == root) {
      /* root reduces in receivebuffer */
      handle->tmpbuf = malloc (span);
      redbuf = recvbuf;
    } else {
      /* recvbuf may not be valid on non-root nodes */
      ptrdiff_t span_align = OPAL_ALIGN(span, datatype->super.align, ptrdiff_t);
      handle->tmpbuf = malloc (span_align + span);
      redbuf = (char*)span_align - gap;
      tmpredbuf = 1;
    }
  } else {
    handle->tmpbuf = malloc (span);
    alg = PNBC_RED_CHAIN;
    segsize = 16384/2;
  }

  if (OPAL_UNLIKELY(NULL == handle->tmpbuf)) {
    PNBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

    schedule = OBJ_NEW(PNBC_Schedule);
    if (OPAL_UNLIKELY(NULL == schedule)) {
      PNBC_Return_handle (handle);
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* make sure the schedule is released with the handle on error */
    handle->schedule = schedule;

    switch(alg) {
      case PNBC_RED_BINOMIAL:
        res = red_sched_binomial(rank, p, root, sendbuf, redbuf, tmpredbuf, count, datatype, op, inplace, schedule, handle);
        break;
      case PNBC_RED_CHAIN:
        res = red_sched_chain(rank, p, root, sendbuf, recvbuf, count, datatype, op, ext, size, schedule, handle, segsize);
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

  return OMPI_SUCCESS;
}

int ompi_coll_libpnbc_ireduce_inter(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype,
				   MPI_Op op, int root, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
				   struct mca_coll_base_module_2_2_0_t *module) {
  int rank, res, rsize;
  PNBC_Schedule *schedule;
  PNBC_Handle *handle;
  ompi_coll_libpnbc_module_t *libpnbc_module = (ompi_coll_libpnbc_module_t*) module;
  ptrdiff_t span, gap;

  rank = ompi_comm_rank (comm);
  rsize = ompi_comm_remote_size (comm);

  res = PNBC_Init_handle(comm, &handle, libpnbc_module);
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

  res = red_sched_linear (rank, rsize, root, sendbuf, recvbuf, (void *)(-gap), count, datatype, op, schedule, handle);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    PNBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  res = PNBC_Sched_commit(schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    PNBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  res = PNBC_Start_internal(handle, schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    PNBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  *request = (ompi_request_t *) handle;

  /* tmpbuf is freed with the handle */
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
                                      MPI_Op op, char inplace, PNBC_Schedule *schedule, PNBC_Handle *handle) {
  int vroot, vrank, vpeer, peer, res, maxr;
  char *rbuf, *lbuf, *buf, tmpbuf;
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
        res = PNBC_Copy(rbuf, count, datatype, ((char *)handle->tmpbuf)-gap, count, datatype, MPI_COMM_SELF);
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
        /* we have to wait until we have the data */
        res = PNBC_Sched_recv (rbuf, tmprbuf, count, datatype, peer, schedule, true);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          return res;
        }

        /* perform the reduce in my local buffer */
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
        tmpbuf = tmprbuf; tmprbuf = tmplbuf; tmplbuf = tmpbuf;
      }
    } else {
      /* we have to send this round */
      vpeer = vrank - (1 << (r - 1));
      VRANK2RANK(peer, vpeer, vroot)
      if (firstred && !inplace) {
        /* we have to use the sendbuf in the first round .. */
        res = PNBC_Sched_send (sendbuf, false, count, datatype, peer, schedule, false);
      } else {
        /* and the redbuf in all remaining rounds */
        res = PNBC_Sched_send (lbuf, tmplbuf, count, datatype, peer, schedule, false);
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
      res = PNBC_Sched_send (redbuf, tmpredbuf, count, datatype, root, schedule, false);
    } else if (root == rank) {
      res = PNBC_Sched_recv (redbuf, tmpredbuf, count, datatype, vroot, schedule, false);
    }
  }

  return OMPI_SUCCESS;
}

/* chain send ... */
static inline int red_sched_chain (int rank, int p, int root, const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
                                   MPI_Op op, int ext, size_t size, PNBC_Schedule *schedule, PNBC_Handle *handle, int fragsize) {
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
          res = PNBC_Sched_recv ((char *)recvbuf+offset, false, thiscount, datatype, rpeer, schedule, true);
        } else {
          res = PNBC_Sched_recv ((char *)offset, true, thiscount, datatype, rpeer, schedule, true);
        }
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }

      /* root reduces into receivebuf */
      if(vrank == 0) {
        if (sendbuf != recvbuf) {
            res = PNBC_Sched_op ((char *) sendbuf + offset, false, (char *) recvbuf + offset, false,
                                 thiscount, datatype, op, schedule, true);
        } else {
            res = PNBC_Sched_op ((char *)offset, true, (char *) recvbuf + offset, false,
                                 thiscount, datatype, op, schedule, true);
        }
      } else {
        res = PNBC_Sched_op ((char *) sendbuf + offset, false, (char *) offset, true, thiscount,
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
        res = PNBC_Sched_send ((char *) sendbuf + offset, false, thiscount, datatype, speer, schedule, true);
      } else {
        res = PNBC_Sched_send ((char *) offset, true, thiscount, datatype, speer, schedule, true);
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
                                    MPI_Op op, PNBC_Schedule *schedule, PNBC_Handle *handle) {
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

    res = PNBC_Sched_recv (lbuf, tmplbuf, count, datatype, 0, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }

    for (int peer = 1 ; peer < rsize ; ++peer) {
      res = PNBC_Sched_recv (rbuf, tmprbuf, count, datatype, peer, schedule, true);
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
  } else if (MPI_PROC_NULL != root) {
    res = PNBC_Sched_send (sendbuf, false, count, datatype, root, schedule, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
  }

  return OMPI_SUCCESS;
}
