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

int ompi_coll_libpnbc_igather_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf,
                             int recvcount, MPI_Datatype recvtype, int root,
                             struct ompi_communicator_t *comm, ompi_request_t ** request,
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
      res = ompi_datatype_type_extent (recvtype, &rcvext);
      if (MPI_SUCCESS != res) {
        PNBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
        return res;
      }
  }

  if (inplace) {
    sendcount = recvcount;
    sendtype = recvtype;
  } else if (rank == root) {
    rbuf = ((char *)recvbuf) + (rank*recvcount*rcvext);
    /* if I am the root - just copy the message (only without MPI_IN_PLACE) */
    res = PNBC_Copy(sendbuf, sendcount, sendtype, rbuf, recvcount, recvtype, comm);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
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
      res = PNBC_Sched_send(sendbuf, false, sendcount, sendtype, root, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        OBJ_RELEASE(schedule);
        return res;
      }
    } else {
      for (int i = 0 ; i < p ; ++i) {
        rbuf = (char *)recvbuf + i * recvcount * rcvext;
        if (i != root) {
          /* root receives message to the right buffer */
          res = PNBC_Sched_recv (rbuf, false, recvcount, recvtype, i, schedule, false);
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

int ompi_coll_libpnbc_igather_inter (const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf,
                                    int recvcount, MPI_Datatype recvtype, int root,
                                    struct ompi_communicator_t *comm, ompi_request_t ** request,
                                    struct mca_coll_base_module_2_2_0_t *module) {
    int res, rsize;
    MPI_Aint rcvext = 0;
    PNBC_Schedule *schedule;
    char *rbuf;
    PNBC_Handle *handle;
    ompi_coll_libpnbc_module_t *libpnbc_module = (ompi_coll_libpnbc_module_t*) module;

    rsize = ompi_comm_remote_size (comm);

    if (root == MPI_ROOT) {
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
    if (root != MPI_ROOT && root != MPI_PROC_NULL) {
        /* send msg to root */
        res = PNBC_Sched_send (sendbuf, false, sendcount, sendtype, root, schedule, false);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          OBJ_RELEASE(schedule);
          return res;
        }
    } else if (MPI_ROOT == root) {
        for (int i = 0 ; i < rsize ; ++i) {
            rbuf = ((char *)recvbuf) + (i * recvcount * rcvext);
            /* root receives message to the right buffer */
            res = PNBC_Sched_recv (rbuf, false, recvcount, recvtype, i, schedule, false);
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

    res = PNBC_Start_internal(handle, schedule);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      PNBC_Return_handle (handle);
      return res;
    }

    *request = (ompi_request_t *) handle;

    return OMPI_SUCCESS;
}
