/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2014-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
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

static inline int a2av_sched_linear(int rank, int p, PNBC_Schedule *schedule,
                                    const void *sendbuf, const int *sendcounts,
                                    const int *sdispls, MPI_Aint sndext, MPI_Datatype sendtype,
                                    void *recvbuf, const int *recvcounts,
                                    const int *rdispls, MPI_Aint rcvext, MPI_Datatype recvtype);

static inline int a2av_sched_pairwise(int rank, int p, PNBC_Schedule *schedule,
                                      const void *sendbuf, const int *sendcounts, const int *sdispls,
                                      MPI_Aint sndext, MPI_Datatype sendtype,
                                      void *recvbuf, const int *recvcounts, const int *rdispls,
                                      MPI_Aint rcvext, MPI_Datatype recvtype);

static inline int a2av_sched_inplace(int rank, int p, PNBC_Schedule *schedule,
                                    void *buf, const int *counts, const int *displs,
                                    MPI_Aint ext, MPI_Datatype type, ptrdiff_t gap);

/* simple linear Alltoallv */
int ompi_coll_libpnbc_ialltoallv_init(const void* sendbuf, const int *sendcounts, const int *sdispls,
                                MPI_Datatype sendtype, void* recvbuf, const int *recvcounts, const int *rdispls,
                                MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                struct mca_coll_base_module_2_2_0_t *module)
{
  int rank, p, res;
  MPI_Aint sndext, rcvext;
  PNBC_Schedule *schedule;
  char *rbuf, *sbuf, inplace;
  ptrdiff_t gap, span;
  PNBC_Handle *handle;
  ompi_coll_libpnbc_module_t *libpnbc_module = (ompi_coll_libpnbc_module_t*) module;

  PNBC_IN_PLACE(sendbuf, recvbuf, inplace);

  rank = ompi_comm_rank (comm);
  p = ompi_comm_size (comm);

  res = ompi_datatype_type_extent (recvtype, &rcvext);
  if (MPI_SUCCESS != res) {
    PNBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
    return res;
  }

  res = PNBC_Init_handle (comm, &handle, libpnbc_module);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    return res;
  }

  /*
   * FIXME - this is an initialisation function
   *         ** it must not do any real work **
   *         this should instead create a short
   *         schedule with just PNBC_Sched_copy
   *         Move this into algorithm selection
   */
  /* copy data to receivbuffer */
  if (inplace) {
    int count = 0;
    for (int i = 0; i < p; i++) {
      if (recvcounts[i] > count) {
        count = recvcounts[i];
      }
    }
    span = opal_datatype_span(&recvtype->super, count, &gap);
    if (OPAL_UNLIKELY(0 == span)) {
      *request = &ompi_request_empty;
      PNBC_Return_handle (handle);
      return MPI_SUCCESS;
    }
    handle->tmpbuf = malloc(span);
    if (OPAL_UNLIKELY(NULL == handle->tmpbuf)) {
      PNBC_Return_handle (handle);
      return OMPI_ERR_OUT_OF_RESOURCE;
    }
    sendcounts = recvcounts;
    sdispls = rdispls;
  } else {
    res = ompi_datatype_type_extent (sendtype, &sndext);
    if (MPI_SUCCESS != res) {
      PNBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
      PNBC_Return_handle (handle);
      return res;
    }
    if (sendcounts[rank] != 0) {
      rbuf = (char *) recvbuf + rdispls[rank] * rcvext;
      sbuf = (char *) sendbuf + sdispls[rank] * sndext;
      res = PNBC_Copy (sbuf, sendcounts[rank], sendtype, rbuf, recvcounts[rank], recvtype, comm);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
  }

  schedule = OBJ_NEW(PNBC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    PNBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }


  if (inplace) {
    res = a2av_sched_inplace(rank, p, schedule, recvbuf, recvcounts,
                                 rdispls, rcvext, recvtype, gap);
  } else {
    res = a2av_sched_linear(rank, p, schedule,
                            sendbuf, sendcounts, sdispls, sndext, sendtype,
                            recvbuf, recvcounts, rdispls, rcvext, recvtype);
  }
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    PNBC_Return_handle (handle);
    OBJ_RELEASE(schedule);
    return res;
  }

  res = PNBC_Sched_commit (schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    PNBC_Return_handle (handle);
    OBJ_RELEASE(schedule);
    return res;
  }

  handle->schedule = schedule;

  *request = (ompi_request_t *) handle;

  return OMPI_SUCCESS;
}

/* simple linear Alltoallv */
int ompi_coll_libpnbc_ialltoallv_inter (const void* sendbuf, const int *sendcounts, const int *sdispls,
				       MPI_Datatype sendtype, void* recvbuf, const int *recvcounts, const int *rdispls,
				       MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
				       struct mca_coll_base_module_2_2_0_t *module)
{
  int res, rsize;
  MPI_Aint sndext, rcvext;
  PNBC_Schedule *schedule;
  PNBC_Handle *handle;
  ompi_coll_libpnbc_module_t *libpnbc_module = (ompi_coll_libpnbc_module_t*) module;


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

  rsize = ompi_comm_remote_size (comm);

  schedule = OBJ_NEW(PNBC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  for (int i = 0; i < rsize; i++) {
    /* post all sends */
    if (sendcounts[i] != 0) {
      char *sbuf = (char *) sendbuf + sdispls[i] * sndext;
      res = PNBC_Sched_send (sbuf, false, sendcounts[i], sendtype, i, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        OBJ_RELEASE(schedule);
        return res;
      }
    }
    /* post all receives */
    if (recvcounts[i] != 0) {
      char *rbuf = (char *) recvbuf + rdispls[i] * rcvext;
      res = PNBC_Sched_recv (rbuf, false, recvcounts[i], recvtype, i, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        OBJ_RELEASE(schedule);
        return res;
      }
    }
  }

  res = PNBC_Sched_commit(schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    return res;
  }

  res = PNBC_Init_handle(comm, &handle, libpnbc_module);
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
    OBJ_RELEASE(schedule);
    return res;
  }

  *request = (ompi_request_t *) handle;

  return OMPI_SUCCESS;
}

__opal_attribute_unused__
static inline int a2av_sched_linear(int rank, int p, PNBC_Schedule *schedule,
                                    const void *sendbuf, const int *sendcounts, const int *sdispls,
                                    MPI_Aint sndext, MPI_Datatype sendtype,
                                    void *recvbuf, const int *recvcounts, const int *rdispls,
                                    MPI_Aint rcvext, MPI_Datatype recvtype) {
  int res;

  for (int i = 0 ; i < p ; ++i) {
    if (i == rank) {
      continue;
    }

    /* post send */
    if (sendcounts[i] != 0) {
      char *sbuf = ((char *) sendbuf) + (sdispls[i] * sndext);
      res = PNBC_Sched_send(sbuf, false, sendcounts[i], sendtype, i, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }

    /* post receive */
    if (recvcounts[i] != 0) {
      char *rbuf = ((char *) recvbuf) + (rdispls[i] * rcvext);
      res = PNBC_Sched_recv(rbuf, false, recvcounts[i], recvtype, i, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
  }

  return OMPI_SUCCESS;
}

__opal_attribute_unused__
static inline int a2av_sched_pairwise(int rank, int p, PNBC_Schedule *schedule,
                                      const void *sendbuf, const int *sendcounts, const int *sdispls,
                                      MPI_Aint sndext, MPI_Datatype sendtype,
                                      void *recvbuf, const int *recvcounts, const int *rdispls,
                                      MPI_Aint rcvext, MPI_Datatype recvtype) {
  int res;

  for (int i = 1 ; i < p ; ++i) {
    int sndpeer = (rank + i) % p;
    int rcvpeer = (rank + p - i) %p;

    /* post send */
    if (sendcounts[sndpeer] != 0) {
      char *sbuf = ((char *) sendbuf) + (sdispls[sndpeer] * sndext);
      res = PNBC_Sched_send(sbuf, false, sendcounts[sndpeer], sendtype, sndpeer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }

    /* post receive */
    if (recvcounts[rcvpeer] != 0) {
      char *rbuf = ((char *) recvbuf) + (rdispls[rcvpeer] * rcvext);
      res = PNBC_Sched_recv(rbuf, false, recvcounts[rcvpeer], recvtype, rcvpeer, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
  }

  return OMPI_SUCCESS;
}

static inline int a2av_sched_inplace(int rank, int p, PNBC_Schedule *schedule,
                                    void *buf, const int *counts, const int *displs,
                                    MPI_Aint ext, MPI_Datatype type, ptrdiff_t gap) {
  int res;

  for (int i = 1; i < (p+1)/2; i++) {
    int speer = (rank + i) % p;
    int rpeer = (rank + p - i) % p;
    char *sbuf = (char *) buf + displs[speer] * ext;
    char *rbuf = (char *) buf + displs[rpeer] * ext;

    if (0 != counts[rpeer]) {
      res = PNBC_Sched_copy (rbuf, false, counts[rpeer], type,
                            (void *)(-gap), true, counts[rpeer], type,
                            schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
    if (0 != counts[speer]) {
      res = PNBC_Sched_send (sbuf, false , counts[speer], type, speer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
    if (0 != counts[rpeer]) {
      res = PNBC_Sched_recv (rbuf, false , counts[rpeer], type, rpeer, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }

    if (0 != counts[rpeer]) {
      res = PNBC_Sched_send ((void *)(-gap), true, counts[rpeer], type, rpeer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
    if (0 != counts[speer]) {
      res = PNBC_Sched_recv (sbuf, false, counts[speer], type, speer, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
  }
  if (0 == (p%2)) {
    int peer = (rank + p/2) % p;

    char *tbuf = (char *) buf + displs[peer] * ext;
    res = PNBC_Sched_copy (tbuf, false, counts[peer], type,
                          (void *)(-gap), true, counts[peer], type,
                          schedule, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
    if (0 != counts[peer]) {
      res = PNBC_Sched_send ((void *)(-gap), true , counts[peer], type, peer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
      res = PNBC_Sched_recv (tbuf, false , counts[peer], type, peer, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
  }

  return OMPI_SUCCESS;
}
