/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
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

int ompi_coll_libpnbc_ineighbor_allgather_init(const void *sbuf, int scount, MPI_Datatype stype, void *rbuf,
                                         int rcount, MPI_Datatype rtype, struct ompi_communicator_t *comm,
										 MPI_Info info, ompi_request_t ** request, struct mca_coll_base_module_2_2_0_t *module) {
  int res, indegree, outdegree, *srcs, *dsts;
  MPI_Aint rcvext;
  PNBC_Handle *handle;
  ompi_coll_libpnbc_module_t *libpnbc_module = (ompi_coll_libpnbc_module_t*) module;
  PNBC_Schedule *schedule;

  res = ompi_datatype_type_extent (rtype, &rcvext);
  if (MPI_SUCCESS != res) {
    PNBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
    return res;
  }

    schedule = OBJ_NEW(PNBC_Schedule);
    if (OPAL_UNLIKELY(NULL == schedule)) {
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    res = PNBC_Comm_neighbors (comm, &srcs, &indegree, &dsts, &outdegree);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      OBJ_RELEASE(schedule);
      return res;
    }

    for (int i = 0 ; i < indegree ; ++i) {
      if (MPI_PROC_NULL != srcs[i]) {
        res = PNBC_Sched_recv ((char *) rbuf + i * rcount * rcvext, true, rcount, rtype, srcs[i], schedule, false);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          break;
        }
      }
    }

    free (srcs);

    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      OBJ_RELEASE(schedule);
      free (dsts);
      return res;
    }

    for (int i = 0 ; i < outdegree ; ++i) {
      if (MPI_PROC_NULL != dsts[i]) {
        res = PNBC_Sched_send ((char *) sbuf, false, scount, stype, dsts[i], schedule, false);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          break;
        }
      }
    }

    free (dsts);

    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      OBJ_RELEASE(schedule);
      return res;
    }

    res = PNBC_Sched_commit (schedule);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      OBJ_RELEASE(schedule);
      return res;
    }

  res = PNBC_Init_handle(comm, &handle, libpnbc_module);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    return res;
  }

  handle->schedule = schedule;

  *request = (ompi_request_t *) handle;

  return OMPI_SUCCESS;
}
