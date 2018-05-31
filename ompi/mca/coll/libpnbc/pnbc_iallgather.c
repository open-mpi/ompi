/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2014-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
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

/* simple linear MPI_Iallgather
 * the algorithm uses p-1 rounds
 * each node sends the packet it received last round (or has in round 0) to it's right neighbor (modulo p)
 * each node receives from it's left (modulo p) neighbor */
int ompi_coll_libpnbc_iallgather_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                                MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                struct mca_coll_base_module_2_2_0_t *module)
{
  int rank, p, res;
  MPI_Aint rcvext;
  PNBC_Schedule *schedule;
  char *rbuf, *sbuf, inplace;
  PNBC_Handle *handle;
  ompi_coll_libpnbc_module_t *libpnbc_module = (ompi_coll_libpnbc_module_t*) module;

  PNBC_IN_PLACE(sendbuf, recvbuf, inplace);

  rank = ompi_comm_rank (comm);
  p = ompi_comm_size (comm);

  res = ompi_datatype_type_extent(recvtype, &rcvext);
  if (MPI_SUCCESS != res) {
    return res;
  }

  /*
   * FIXME - this is an initialisation function
   *         ** it must not do any real work **
   *         this should instead create a short
   *         schedule with just PNBC_Sched_copy
   *         Move this into algorithm selection
   */
  if (inplace) {
    sendtype = recvtype;
    sendcount = recvcount;
  } else {
    /* copy my data to receive buffer */
    rbuf = (char *) recvbuf + rank * recvcount * rcvext;
    res = PNBC_Copy (sendbuf, sendcount, sendtype, rbuf, recvcount, recvtype, comm);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
  }
  if (1 == p) {
    *request = &ompi_request_empty;
    return OMPI_SUCCESS;
  }

  schedule = OBJ_NEW(PNBC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  sbuf = (char *)recvbuf + rank * recvcount * rcvext;
  /* do p-1 rounds */
  for(int r = 0 ; r < p ; ++r) {
    if(r != rank) {
      /* recv from rank r */
      rbuf = (char *)recvbuf + r * recvcount * rcvext;
      res = PNBC_Sched_recv (rbuf, false, recvcount, recvtype, r, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        OBJ_RELEASE(schedule);
        return res;
      }

      /* send to rank r - not from the sendbuf to optimize MPI_IN_PLACE */
      res = PNBC_Sched_send (sbuf, false, recvcount, recvtype, r, schedule, false);
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

  res = PNBC_Init_handle (comm, &handle, libpnbc_module);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    return res;
  }

  handle->schedule = schedule;

  *request = (ompi_request_t *) handle;

  return OMPI_SUCCESS;

}

int ompi_coll_libpnbc_iallgather_inter(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
				      MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
				      struct mca_coll_base_module_2_2_0_t *module)
{
  int res, rsize;
  MPI_Aint rcvext;
  PNBC_Schedule *schedule;
  char *rbuf;
  PNBC_Handle *handle;
  ompi_coll_libpnbc_module_t *libpnbc_module = (ompi_coll_libpnbc_module_t*) module;

  res = ompi_datatype_type_extent(recvtype, &rcvext);
  if (MPI_SUCCESS != res) {
    PNBC_Error ("MPI Error in ompi_datatype_type_extent() (%i)", res);
    return res;
  }

  rsize = ompi_comm_remote_size (comm);

  /* set up schedule */
  schedule = OBJ_NEW(PNBC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  /* do rsize - 1 rounds */
  for (int r = 0 ; r < rsize ; ++r) {
    /* recv from rank r */
    rbuf = (char *) recvbuf + r * recvcount * rcvext;
    res = PNBC_Sched_recv (rbuf, false, recvcount, recvtype, r, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      OBJ_RELEASE(schedule);
      return res;
    }

    /* send to rank r */
    res = PNBC_Sched_send (sendbuf, false, sendcount, sendtype, r, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      OBJ_RELEASE(schedule);
      return res;
    }
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
  res = PNBC_Start_internal (handle, schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OMPI_COLL_LIBPNBC_REQUEST_RETURN(handle);
    return res;
  }

  *request = (ompi_request_t *) handle;

  return OMPI_SUCCESS;
}
