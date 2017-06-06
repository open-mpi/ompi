/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2013      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "pnbc_internal.h"

int ompi_coll_libpnbc_igatherv_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype,
                              void* recvbuf, const int *recvcounts, const int *displs, MPI_Datatype recvtype,
                              int root, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                              struct mca_coll_base_module_2_2_0_t *module) {
  int rank, p, res;
  MPI_Aint rcvext = 0;
  PNBC_Schedule *schedule;
  char *rbuf, inplace = 0;
  PNBC_Handle *handle;
  ompi_coll_libpnbc_module_t *libpnbc_module = (ompi_coll_libpnbc_module_t*) module;

  rank = ompi_comm_rank (comm);
  if (root == rank) {
    PNBC_IN_PLACE(sendbuf, recvbuf, inplace);
  }
  p = ompi_comm_size (comm);

  if (rank == root) {
    res = ompi_datatype_type_extent(recvtype, &rcvext);
    if (MPI_SUCCESS != res) {
      PNBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
      return res;
    }
  }

  schedule = OBJ_NEW(PNBC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  /* send to root */
  if (rank != root) {
    /* send msg to root */
    res = PNBC_Sched_send (sendbuf, false, sendcount, sendtype, root, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      OBJ_RELEASE(schedule);
      return res;
    }
  } else {
    for (int i = 0 ; i < p ; ++i) {
      rbuf = (char *) recvbuf + displs[i] * rcvext;
      if (i == root) {
        if (!inplace) {

  /*
   * FIXME - this is an initialisation function
   *         ** it must not do any real work **
   *         this should instead create a short
   *         schedule with just PNBC_Sched_copy
   *         Move this into algorithm selection
   */

          /* if I am the root - just copy the message */
          res = PNBC_Copy (sendbuf, sendcount, sendtype, rbuf, recvcounts[i], recvtype,
                          comm);
          if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
            OBJ_RELEASE(schedule);
            return res;
          }
        }
      } else {
        /* root receives message to the right buffer */
        res = PNBC_Sched_recv (rbuf, false, recvcounts[i], recvtype, i, schedule, false);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          OBJ_RELEASE(schedule);
          return res;
        }
      }
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

  handle->schedule = schedule;

  *request = (ompi_request_t *) handle;

  return OMPI_SUCCESS;
}

int ompi_coll_libpnbc_igatherv_inter (const void* sendbuf, int sendcount, MPI_Datatype sendtype,
				     void* recvbuf, const int *recvcounts, const int *displs, MPI_Datatype recvtype,
				     int root, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
				     struct mca_coll_base_module_2_2_0_t *module) {
  int res, rsize;
  MPI_Aint rcvext;
  PNBC_Schedule *schedule;
  char *rbuf;
  PNBC_Handle *handle;
  ompi_coll_libpnbc_module_t *libpnbc_module = (ompi_coll_libpnbc_module_t*) module;

  rsize = ompi_comm_remote_size (comm);

  if (MPI_ROOT == root) {
    res = ompi_datatype_type_extent(recvtype, &rcvext);
    if (MPI_SUCCESS != res) {
      PNBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
      return res;
    }
  }

  schedule = OBJ_NEW(PNBC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  /* send to root */
  if (MPI_ROOT != root && MPI_PROC_NULL != root) {
    /* send msg to root */
    res = PNBC_Sched_send (sendbuf, false, sendcount, sendtype, root, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      OBJ_RELEASE(schedule);
      return res;
    }
  } else if (MPI_ROOT == root) {
    for (int i = 0 ; i < rsize ; ++i) {
      rbuf = (char *) recvbuf + displs[i] * rcvext;
      /* root receives message to the right buffer */
      res = PNBC_Sched_recv (rbuf, false, recvcounts[i], recvtype, i, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        OBJ_RELEASE(schedule);
        return res;
      }
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
  res = PNBC_Start_internal(handle, schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    PNBC_Return_handle (handle);
    return res;
  }

  *request = (ompi_request_t *) handle;

  return OMPI_SUCCESS;
}
