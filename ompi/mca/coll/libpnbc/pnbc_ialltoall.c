/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
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

static inline int a2a_sched_linear(int rank, int p, MPI_Aint sndext, MPI_Aint rcvext, PNBC_Schedule *schedule,
                                   const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf,
                                   int recvcount, MPI_Datatype recvtype, MPI_Comm comm);
static inline int a2a_sched_pairwise(int rank, int p, MPI_Aint sndext, MPI_Aint rcvext, PNBC_Schedule *schedule,
                                     const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf,
                                     int recvcount, MPI_Datatype recvtype, MPI_Comm comm);
static inline int a2a_sched_diss(int rank, int p, MPI_Aint sndext, MPI_Aint rcvext, PNBC_Schedule* schedule,
                                 const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf,
                                 int recvcount, MPI_Datatype recvtype, MPI_Comm comm, PNBC_Handle *handle);
static inline int a2a_sched_inplace(int rank, int p, PNBC_Schedule* schedule, void* buf, int count,
                                   MPI_Datatype type, MPI_Aint ext, ptrdiff_t gap, MPI_Comm comm);


/* simple linear MPI_Ialltoall the (simple) algorithm just sends to all nodes */
int ompi_coll_libpnbc_ialltoall_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                               MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                               struct mca_coll_base_module_2_2_0_t *module)
{
  int rank, p, res, datasize;
  size_t a2asize, sndsize;
  PNBC_Schedule *schedule;
  MPI_Aint rcvext, sndext;

  char *rbuf, *sbuf, inplace;
  enum {PNBC_A2A_LINEAR, PNBC_A2A_PAIRWISE, PNBC_A2A_DISS, PNBC_A2A_INPLACE} alg;
  PNBC_Handle *handle;
  ompi_coll_libpnbc_module_t *libpnbc_module = (ompi_coll_libpnbc_module_t*) module;
  ptrdiff_t span, gap;

  PNBC_IN_PLACE(sendbuf, recvbuf, inplace);

  rank = ompi_comm_rank (comm);
  p = ompi_comm_size (comm);

  res = ompi_datatype_type_extent(sendtype, &sndext);
  if (MPI_SUCCESS != res) {
    PNBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
    return res;
  }

  res = ompi_datatype_type_extent(recvtype, &rcvext);
  if (MPI_SUCCESS != res) {
    PNBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
    return res;
  }

  res = ompi_datatype_type_size(sendtype, &sndsize);
  if (MPI_SUCCESS != res) {
    PNBC_Error("MPI Error in ompi_datatype_type_size() (%i)", res);
    return res;
  }

  /* algorithm selection */
  a2asize = sndsize * sendcount * p;
  /* this number is optimized for TCP on odin.cs.indiana.edu */
  if (inplace) {
    alg = PNBC_A2A_INPLACE;
  } else if((p <= 8) && ((a2asize < 1<<17) || (sndsize*sendcount < 1<<12))) {
    /* just send as fast as we can if we have less than 8 peers, if the
     * total communicated size is smaller than 1<<17 *and* if we don't
     * have eager messages (msgsize < 1<<13) */
    alg = PNBC_A2A_LINEAR;
  } else if(a2asize < (1<<12)*(unsigned int)p) {
    /*alg = PNBC_A2A_DISS;*/
    alg = PNBC_A2A_LINEAR;
  } else
    alg = PNBC_A2A_LINEAR; /*PNBC_A2A_PAIRWISE;*/

  /*
   * FIXME - this is an initialisation function
   *         ** it must not do any real work **
   *         this should instead create a short
   *         schedule with just PNBC_Sched_copy
   *         Move this into algorithm selection
   */
  if (!inplace) {
    /* copy my data to receive buffer */
    rbuf = (char *) recvbuf + rank * recvcount * rcvext;
    sbuf = (char *) sendbuf + rank * sendcount * sndext;
    res = PNBC_Copy (sbuf, sendcount, sendtype, rbuf, recvcount, recvtype, comm);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
  }

  res = PNBC_Init_handle(comm, &handle, libpnbc_module);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    return res;
  }

  /* allocate temp buffer if we need one */
  if (alg == PNBC_A2A_INPLACE) {
    span = opal_datatype_span(&recvtype->super, recvcount, &gap);
    handle->tmpbuf = malloc(span);
    if (OPAL_UNLIKELY(NULL == handle->tmpbuf)) {
      PNBC_Return_handle (handle);
      return OMPI_ERR_OUT_OF_RESOURCE;
    }
  } else if (alg == PNBC_A2A_DISS) {

  /*
   * FIXME - this is an initialisation function
   *         ** it must not do any real work **
   *         this does Pack, memcpy, and so on
   */

    /* only A2A_DISS needs buffers */
    if(PNBC_Type_intrinsic(sendtype)) {
      datasize = sndext * sendcount;
    } else {
      res = PMPI_Pack_size (sendcount, sendtype, comm, &datasize);
      if (MPI_SUCCESS != res) {
        PNBC_Error("MPI Error in PMPI_Pack_size() (%i)", res);
        PNBC_Return_handle (handle);
        return res;
      }
    }

    /* allocate temporary buffers */
    if ((p & 1) == 0) {
      handle->tmpbuf = malloc (datasize * p * 2);
    } else {
      /* we cannot divide p by two, so alloc more to be safe ... */
      handle->tmpbuf = malloc (datasize * (p / 2 + 1) * 2 * 2);
    }

    if (OPAL_UNLIKELY(NULL == handle->tmpbuf)) {
      PNBC_Return_handle (handle);
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* phase 1 - rotate n data blocks upwards into the tmpbuffer */
#if OPAL_CUDA_SUPPORT
    if (PNBC_Type_intrinsic(sendtype) && !(opal_cuda_check_bufs((char *)sendbuf, (char *)recvbuf))) {
#else
    if (PNBC_Type_intrinsic(sendtype)) {
#endif /* OPAL_CUDA_SUPPORT */
      /* contiguous - just copy (1st copy) */
      memcpy (handle->tmpbuf, (char *) sendbuf + datasize * rank, datasize * (p - rank));
      if (rank != 0) {
        memcpy ((char *) handle->tmpbuf + datasize * (p - rank), sendbuf, datasize * rank);
      }
    } else {
      int pos=0;

      /* non-contiguous - pack */
      res = PMPI_Pack ((char *) sendbuf + rank * sendcount * sndext, (p - rank) * sendcount, sendtype, handle->tmpbuf,
                      (p - rank) * datasize, &pos, comm);
      if (OPAL_UNLIKELY(MPI_SUCCESS != res)) {
        PNBC_Error("MPI Error in PMPI_Pack() (%i)", res);
        PNBC_Return_handle (handle);
        return res;
      }

      if (rank != 0) {
        pos = 0;
        res = PMPI_Pack(sendbuf, rank * sendcount, sendtype, (char *) handle->tmpbuf + datasize * (p - rank),
                       rank * datasize, &pos, comm);
        if (OPAL_UNLIKELY(MPI_SUCCESS != res)) {
          PNBC_Error("MPI Error in PMPI_Pack() (%i)", res);
          PNBC_Return_handle (handle);
          return res;
        }
      }
    }
  }


    /* not found - generate new schedule */
    schedule = OBJ_NEW(PNBC_Schedule);
    if (OPAL_UNLIKELY(NULL == schedule)) {
      PNBC_Return_handle (handle);
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* ensure the schedule is released with the handle on error */
    handle->schedule = schedule;

    switch(alg) {
      case PNBC_A2A_INPLACE:
        res = a2a_sched_inplace(rank, p, schedule, recvbuf, recvcount, recvtype, rcvext, gap, comm);
        break;
      case PNBC_A2A_LINEAR:
        res = a2a_sched_linear(rank, p, sndext, rcvext, schedule, sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm);
        break;
      case PNBC_A2A_DISS:
        res = a2a_sched_diss(rank, p, sndext, rcvext, schedule, sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, handle);
        break;
      case PNBC_A2A_PAIRWISE:
        res = a2a_sched_pairwise(rank, p, sndext, rcvext, schedule, sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm);
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

int ompi_coll_libpnbc_ialltoall_inter (const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
				      MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
				      struct mca_coll_base_module_2_2_0_t *module)
{
  int res, rsize;
  MPI_Aint sndext, rcvext;
  PNBC_Schedule *schedule;
  char *rbuf, *sbuf;
  PNBC_Handle *handle;
  ompi_coll_libpnbc_module_t *libpnbc_module = (ompi_coll_libpnbc_module_t*) module;

  rsize = ompi_comm_remote_size (comm);

  res = ompi_datatype_type_extent (sendtype, &sndext);
  if (MPI_SUCCESS != res) {
    PNBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
    return res;
  }

  res = ompi_datatype_type_extent (recvtype, &rcvext);
  if (MPI_SUCCESS != res) {
    PNBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
    return res;
  }

  schedule = OBJ_NEW(PNBC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  for (int i = 0; i < rsize; i++) {
    /* post all sends */
    sbuf = (char *) sendbuf + i * sendcount * sndext;
    res = PNBC_Sched_send (sbuf, false, sendcount, sendtype, i, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      break;
    }

    /* post all receives */
    rbuf = (char *) recvbuf + i * recvcount * rcvext;
    res = PNBC_Sched_recv (rbuf, false, recvcount, recvtype, i, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      break;
    }
  }

  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    return res;
  }

  res = PNBC_Sched_commit (schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    return res;
  }

  res = PNBC_Init_handle (comm, &handle, libpnbc_module);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
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

  return OMPI_SUCCESS;
}

static inline int a2a_sched_pairwise(int rank, int p, MPI_Aint sndext, MPI_Aint rcvext, PNBC_Schedule* schedule,
                                     const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                                     MPI_Datatype recvtype, MPI_Comm comm) {
  int res;

  if (p < 2) {
    return OMPI_SUCCESS;
  }

  for (int r = 1 ; r < p ; ++r) {
    int sndpeer = (rank + r) % p;
    int rcvpeer = (rank - r + p) % p;

    char *rbuf = (char *) recvbuf + rcvpeer * recvcount * rcvext;
    res = PNBC_Sched_recv (rbuf, false, recvcount, recvtype, rcvpeer, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }

    char *sbuf = (char *) sendbuf + sndpeer * sendcount * sndext;
    res = PNBC_Sched_send (sbuf, false, sendcount, sendtype, sndpeer, schedule, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
  }

  return OMPI_SUCCESS;
}

static inline int a2a_sched_linear(int rank, int p, MPI_Aint sndext, MPI_Aint rcvext, PNBC_Schedule* schedule,
                                   const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                                   MPI_Datatype recvtype, MPI_Comm comm) {
  int res;

  for (int r = 0 ; r < p ; ++r) {
    /* easy algorithm */
    if (r == rank) {
      continue;
    }

    char *rbuf = (char *) recvbuf + r * recvcount * rcvext;
    res = PNBC_Sched_recv (rbuf, false, recvcount, recvtype, r, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }

    char *sbuf = (char *) sendbuf + r * sendcount * sndext;
    res = PNBC_Sched_send (sbuf, false, sendcount, sendtype, r, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
  }

  return OMPI_SUCCESS;
}

static inline int a2a_sched_diss(int rank, int p, MPI_Aint sndext, MPI_Aint rcvext, PNBC_Schedule* schedule,
                                 const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                                 MPI_Datatype recvtype, MPI_Comm comm, PNBC_Handle *handle) {
  int res, speer, rpeer, datasize, offset, virtp;
  char *rbuf, *rtmpbuf, *stmpbuf;

  if (p < 2) {
    return OMPI_SUCCESS;
  }

  if(PNBC_Type_intrinsic(sendtype)) {
    datasize = sndext*sendcount;
  } else {
    res = PMPI_Pack_size(sendcount, sendtype, comm, &datasize);
    if (MPI_SUCCESS != res) {
      PNBC_Error("MPI Error in PMPI_Pack_size() (%i)", res);
      return res;
    }
  }

  /* allocate temporary buffers */
  if ((p & 1) == 0) {
    rtmpbuf = (char *) handle->tmpbuf + datasize * p;
    stmpbuf = (char *) handle->tmpbuf + datasize * (p + p / 2);
  } else {
    /* we cannot divide p by two, so alloc more to be safe ... */
    virtp = (p / 2 + 1) * 2;
    rtmpbuf = (char *) handle->tmpbuf + datasize * p;
    stmpbuf = (char *) handle->tmpbuf + datasize * (p + virtp / 2);
  }

  /* phase 2 - communicate */
  for (int r = 1; r < p; r <<= 1) {
    offset = 0;
    for (int i = 1 ; i < p; ++i) {
      /* test if bit r is set in rank number i */
      if (i & r) {
        /* copy data to sendbuffer (2nd copy) - could be avoided using iovecs */
        /*printf("[%i] round %i: copying element %i to buffer %lu\n", rank, r, i, (unsigned long)(stmpbuf+offset));*/
        res = PNBC_Sched_copy((void *)(intptr_t)(i * datasize), true, datasize, MPI_BYTE, stmpbuf + offset -
                             (intptr_t) handle->tmpbuf, true, datasize, MPI_BYTE, schedule, false);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          return res;
        }
        offset += datasize;
      }
    }

    speer = (rank + r) % p;
    /* add p because modulo does not work with negative values */
    rpeer = ((rank - r) + p) % p;

    res = PNBC_Sched_recv (rtmpbuf - (intptr_t) handle->tmpbuf, true, offset, MPI_BYTE, rpeer, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }

    res = PNBC_Sched_send (stmpbuf - (intptr_t) handle->tmpbuf, true, offset, MPI_BYTE, speer, schedule, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }

    /* unpack from buffer */
    offset = 0;
    for (int i = 1; i < p; ++i) {
      /* test if bit r is set in rank number i */
      if (i & r) {
        /* copy data to tmpbuffer (3rd copy) - could be avoided using iovecs */
        res = PNBC_Sched_copy (rtmpbuf + offset - (intptr_t) handle->tmpbuf, true, datasize, MPI_BYTE,
                              (void *)(intptr_t)(i * datasize), true, datasize, MPI_BYTE, schedule,
                              false);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          return res;
        }

        offset += datasize;
      }
    }
  }

  /* phase 3 - reorder - data is now in wrong order in handle->tmpbuf -
   * reorder it into recvbuf */
  for (int i = 0 ; i < p; ++i) {
    rbuf = (char *) recvbuf + ((rank - i + p) % p) * recvcount * rcvext;
    res = PNBC_Sched_unpack ((void *)(intptr_t) (i * datasize), true, recvcount, recvtype, rbuf, false, schedule,
                            false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
  }

  return OMPI_SUCCESS;
}

static inline int a2a_sched_inplace(int rank, int p, PNBC_Schedule* schedule, void* buf, int count,
                                   MPI_Datatype type, MPI_Aint ext, ptrdiff_t gap, MPI_Comm comm) {
  int res;

  for (int i = 1 ; i < (p+1)/2 ; i++) {
    int speer = (rank + i) % p;
    int rpeer = (rank + p - i) % p;
    char *sbuf = (char *) buf + speer * count * ext;
    char *rbuf = (char *) buf + rpeer * count * ext;

    res = PNBC_Sched_copy (rbuf, false, count, type,
                          (void *)(-gap), true, count, type,
                          schedule, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
    res = PNBC_Sched_send (sbuf, false , count, type, speer, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
    res = PNBC_Sched_recv (rbuf, false , count, type, rpeer, schedule, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }

    res = PNBC_Sched_send ((void *)(-gap), true, count, type, rpeer, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
    res = PNBC_Sched_recv (sbuf, false, count, type, speer, schedule, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
  }
  if (0 == (p%2)) {
    int peer = (rank + p/2) % p;

    char *tbuf = (char *) buf + peer * count * ext;
    res = PNBC_Sched_copy (tbuf, false, count, type,
                          (void *)(-gap), true, count, type,
                          schedule, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
    res = PNBC_Sched_send ((void *)(-gap), true , count, type, peer, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
    res = PNBC_Sched_recv (tbuf, false , count, type, peer, schedule, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
  }

  return OMPI_SUCCESS;
}
