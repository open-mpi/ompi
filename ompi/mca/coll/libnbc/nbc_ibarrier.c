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
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "nbc_internal.h"

/* Dissemination implementation of MPI_Ibarrier */
int ompi_coll_libnbc_ibarrier(struct ompi_communicator_t *comm, ompi_request_t ** request,
                              struct mca_coll_base_module_2_1_0_t *module)
{
  int rank, p, maxround, res, recvpeer, sendpeer;
  NBC_Schedule *schedule;
  NBC_Handle *handle;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

  rank = ompi_comm_rank (comm);
  p = ompi_comm_size (comm);

  res = NBC_Init_handle(comm, &handle, libnbc_module);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    return res;
  }

  handle->tmpbuf = malloc (2);
  if (OPAL_UNLIKELY(NULL == handle->tmpbuf)) {
    NBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

#ifdef NBC_CACHE_SCHEDULE
  /* there only one argument set per communicator -> hang it directly at
   * the tree-position, NBC_Dict_size[...] is 0 for not initialized and
   * 1 for initialized. NBC_Dict[...] is a pointer to the schedule in
   * this case */
  if (libnbc_module->NBC_Dict_size[NBC_BARRIER] == 0) {
    /* we did not init it yet */
#endif
    schedule = OBJ_NEW(NBC_Schedule);
    if (OPAL_UNLIKELY(NULL == schedule)) {
      NBC_Return_handle (handle);
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
      res = NBC_Sched_send ((void *) 0, true, 1, MPI_BYTE, sendpeer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        NBC_Return_handle (handle);
        return OMPI_ERR_OUT_OF_RESOURCE;
      }

      /* recv msg from recvpeer */
      res = NBC_Sched_recv ((void *) 1, true, 1, MPI_BYTE, recvpeer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        NBC_Return_handle (handle);
        return OMPI_ERR_OUT_OF_RESOURCE;
      }

      /* end communication round */
      if (round < maxround) {
        res = NBC_Sched_barrier (schedule);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          NBC_Return_handle (handle);
          return OMPI_ERR_OUT_OF_RESOURCE;
        }
      }
    }

    res = NBC_Sched_commit (schedule);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      NBC_Return_handle (handle);
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

#ifdef NBC_CACHE_SCHEDULE
    /* add it */
    libnbc_module->NBC_Dict[NBC_BARRIER] = (hb_tree *) schedule;
    libnbc_module->NBC_Dict_size[NBC_BARRIER] = 1;
  } else {
    /* we found it */
    handle->schedule = schedule = (NBC_Schedule *) libnbc_module->NBC_Dict[NBC_BARRIER];
  }
  OBJ_RETAIN(schedule);
#endif

  res = NBC_Start (handle, schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    NBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  *request = (ompi_request_t *) handle;

  return OMPI_SUCCESS;
}

int ompi_coll_libnbc_ibarrier_inter(struct ompi_communicator_t *comm, ompi_request_t ** request,
                                    struct mca_coll_base_module_2_1_0_t *module)
{
  int rank, res, rsize;
  NBC_Schedule *schedule;
  NBC_Handle *handle;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

  rank = ompi_comm_rank (comm);
  rsize = ompi_comm_remote_size (comm);

  res = NBC_Init_handle(comm, &handle, libnbc_module);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    return res;
  }

  handle->tmpbuf = malloc (2);
  if (OPAL_UNLIKELY(NULL == handle->tmpbuf)) {
    NBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  schedule = OBJ_NEW(NBC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    NBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  /* ensure the schedule is released with the handle on error */
  handle->schedule = schedule;

  if (0 == rank) {
    for (int peer = 1 ; peer < rsize ; ++peer) {
      res = NBC_Sched_recv (0, true, 1, MPI_BYTE, peer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        NBC_Return_handle (handle);
        return OMPI_ERR_OUT_OF_RESOURCE;
      }
    }
  }

  /* synchronize with the remote root */
  res = NBC_Sched_recv (0, true, 1, MPI_BYTE, 0, schedule, false);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    NBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  res = NBC_Sched_send (0, true, 1, MPI_BYTE, 0, schedule, false);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    NBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  if (0 == rank) {
    /* wait for the remote root */
    res = NBC_Sched_barrier (schedule);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      NBC_Return_handle (handle);
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* inform remote peers that all local peers have entered the barrier */
    for (int peer = 1; peer < rsize ; ++peer) {
      res = NBC_Sched_send (0, true, 1, MPI_BYTE, peer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        NBC_Return_handle (handle);
        return OMPI_ERR_OUT_OF_RESOURCE;
      }
    }
  }

  res = NBC_Sched_commit (schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    NBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  res = NBC_Start (handle, schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    NBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  *request = (ompi_request_t *) handle;

  return OMPI_SUCCESS;
}
