/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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

/* Dissemination implementation of MPI_Ibarrier */
int ompi_coll_libpnbc_ibarrier_init(struct ompi_communicator_t *comm, ompi_request_t ** request,
                              struct mca_coll_base_module_2_2_0_t *module)
{
  int rank, p, maxround, res, recvpeer, sendpeer;
  PNBC_Schedule *schedule;
  PNBC_Handle *handle;
  ompi_coll_libpnbc_module_t *libpnbc_module = (ompi_coll_libpnbc_module_t*) module;

  rank = ompi_comm_rank (comm);
  p = ompi_comm_size (comm);

  res = PNBC_Init_handle(comm, &handle, libpnbc_module);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    return res;
  }

    schedule = OBJ_NEW(PNBC_Schedule);
    if (OPAL_UNLIKELY(NULL == schedule)) {
      PNBC_Return_handle (handle);
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* ensure the schedule is released with the handle on error */
    handle->schedule = schedule;

    maxround = (int)ceil((log((double)p)/LOG2)-1);

    for (int round = 0 ; round <= maxround ; ++round) {
      sendpeer = (rank + (1 << round)) % p;
      /* add p because modulo does not work with negative values */
      recvpeer = ((rank - (1 << round)) + p) % p;

      /* send msg to sendpeer */
      res = PNBC_Sched_send (NULL, false, 0, MPI_BYTE, sendpeer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        PNBC_Return_handle (handle);
        return OMPI_ERR_OUT_OF_RESOURCE;
      }

      /* recv msg from recvpeer */
      res = PNBC_Sched_recv (NULL, false, 0, MPI_BYTE, recvpeer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        PNBC_Return_handle (handle);
        return OMPI_ERR_OUT_OF_RESOURCE;
      }

      /* end communication round */
      if (round < maxround) {
        res = PNBC_Sched_barrier (schedule);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          PNBC_Return_handle (handle);
          return OMPI_ERR_OUT_OF_RESOURCE;
        }
      }
    }

    res = PNBC_Sched_commit (schedule);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      PNBC_Return_handle (handle);
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

  handle->schedule = schedule;

  *request = (ompi_request_t *) handle;

  return OMPI_SUCCESS;
}

int ompi_coll_libpnbc_ibarrier_inter(struct ompi_communicator_t *comm, ompi_request_t ** request,
                                    struct mca_coll_base_module_2_2_0_t *module)
{
  int rank, res, rsize;
  PNBC_Schedule *schedule;
  PNBC_Handle *handle;
  ompi_coll_libpnbc_module_t *libpnbc_module = (ompi_coll_libpnbc_module_t*) module;

  rank = ompi_comm_rank (comm);
  rsize = ompi_comm_remote_size (comm);

  res = PNBC_Init_handle(comm, &handle, libpnbc_module);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    return res;
  }

  schedule = OBJ_NEW(PNBC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    PNBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  /* ensure the schedule is released with the handle on error */
  handle->schedule = schedule;

  if (0 == rank) {
    for (int peer = 1 ; peer < rsize ; ++peer) {
      res = PNBC_Sched_recv (NULL, false, 0, MPI_BYTE, peer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        PNBC_Return_handle (handle);
        return OMPI_ERR_OUT_OF_RESOURCE;
      }
    }
  }

  /* synchronize with the remote root */
  res = PNBC_Sched_recv (NULL, false, 0, MPI_BYTE, 0, schedule, false);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    PNBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  res = PNBC_Sched_send (NULL, false, 0, MPI_BYTE, 0, schedule, false);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    PNBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  if (0 == rank) {
    /* wait for the remote root */
    res = PNBC_Sched_barrier (schedule);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      PNBC_Return_handle (handle);
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* inform remote peers that all local peers have entered the barrier */
    for (int peer = 1; peer < rsize ; ++peer) {
      res = PNBC_Sched_send (NULL, false, 0, MPI_BYTE, peer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        PNBC_Return_handle (handle);
        return OMPI_ERR_OUT_OF_RESOURCE;
      }
    }
  }

  res = PNBC_Sched_commit (schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    PNBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
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
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  *request = (ompi_request_t *) handle;

  return OMPI_SUCCESS;
}
