/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "pnbc_internal.h"

/* linear iscan
 * working principle:
 * 1. each node (but node 0) receives from left neighbor
 * 2. performs op
 * 3. all but rank p-1 do sends to it's right neighbor and exits
 *
 */
int ompi_coll_libpnbc_iscan_init(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                           struct ompi_communicator_t *comm, ompi_request_t ** request,
                           struct mca_coll_base_module_2_2_0_t *module) {
  int rank, p, res;
  ptrdiff_t gap, span;
  PNBC_Schedule *schedule;
  char inplace;
  PNBC_Handle *handle;
  ompi_coll_libpnbc_module_t *libpnbc_module = (ompi_coll_libpnbc_module_t*) module;

  PNBC_IN_PLACE(sendbuf, recvbuf, inplace);

  rank = ompi_comm_rank (comm);
  p = ompi_comm_size (comm);

  if (!inplace) {
    /* copy data to receivebuf */
    res = PNBC_Copy (sendbuf, count, datatype, recvbuf, count, datatype, comm);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
  }

  res = PNBC_Init_handle(comm, &handle, libpnbc_module);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    return res;
  }

    schedule = OBJ_NEW(PNBC_Schedule);
    if (OPAL_UNLIKELY(NULL == schedule)) {
      PNBC_Return_handle (handle);
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* ensure the schedule is released with the handle */
    handle->schedule = schedule;

    if(rank != 0) {
      span = opal_datatype_span(&datatype->super, count, &gap);
      handle->tmpbuf = malloc (span);
      if (NULL == handle->tmpbuf) {
        PNBC_Return_handle (handle);
        return OMPI_ERR_OUT_OF_RESOURCE;
      }

      /* we have to wait until we have the data */
      res = PNBC_Sched_recv ((void *)(-gap), true, count, datatype, rank-1, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        PNBC_Return_handle (handle);
        return res;
      }

      /* perform the reduce in my local buffer */
      /* this cannot be done until handle->tmpbuf is unused :-( so barrier after the op */
      res = PNBC_Sched_op ((void *)(-gap), true, recvbuf, false, count, datatype, op, schedule,
                          true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        PNBC_Return_handle (handle);
        return res;
      }
    }

    if (rank != p-1) {
      res = PNBC_Sched_send (recvbuf, false, count, datatype, rank+1, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        PNBC_Return_handle (handle);
        return res;
      }
    }

    res = PNBC_Sched_commit (schedule);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      PNBC_Return_handle (handle);
      return res;
    }

  res = PNBC_Start(handle, schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    PNBC_Return_handle (handle);
    return res;
  }

  *request = (ompi_request_t *) handle;

  /* tmpbuf is freed with the handle */
  return OMPI_SUCCESS;
}
