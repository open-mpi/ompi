/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2013      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "pnbc_internal.h"

/* simple linear MPI_Iscatter */
int ompi_coll_libpnbc_iscatter_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype,
                               void* recvbuf, int recvcount, MPI_Datatype recvtype, int root,
                               struct ompi_communicator_t *comm, ompi_request_t ** request,
                               struct mca_coll_base_module_2_2_0_t *module) {
  int rank, p, res;
  MPI_Aint sndext = 0;
  PNBC_Schedule *schedule;
  char *sbuf, inplace = 0;
  PNBC_Handle *handle;
  ompi_coll_libpnbc_module_t *libpnbc_module = (ompi_coll_libpnbc_module_t*) module;


  rank = ompi_comm_rank (comm);
  if (root == rank) {
    PNBC_IN_PLACE(sendbuf, recvbuf, inplace);
  }
  p = ompi_comm_size (comm);

  if (rank == root) {
    res = ompi_datatype_type_extent (sendtype, &sndext);
    if (MPI_SUCCESS != res) {
      PNBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
      return res;
    }
  }

  if ((rank == root) && (!inplace)) {
    sbuf = (char *) sendbuf + rank * sendcount * sndext;
    /* if I am the root - just copy the message (not for MPI_IN_PLACE) */
    res = PNBC_Copy (sbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
  }

    schedule = OBJ_NEW(PNBC_Schedule);
    if (OPAL_UNLIKELY(NULL == schedule)) {
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* receive from root */
    if (rank != root) {
      /* recv msg from root */
      res = PNBC_Sched_recv (recvbuf, false, recvcount, recvtype, root, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        OBJ_RELEASE(schedule);
        return res;
      }
    } else {
      for (int i = 0 ; i < p ; ++i) {
        sbuf = (char *) sendbuf + i * sendcount * sndext;
        if (i != root) {
          /* root sends the right buffer to the right receiver */
          res = PNBC_Sched_send (sbuf, false, sendcount, sendtype, i, schedule, false);
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

  res = PNBC_Init_handle(comm, &handle, libpnbc_module);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    return res;
  }

  *request = (ompi_request_t *) handle;

  return OMPI_SUCCESS;
}

int ompi_coll_libpnbc_iscatter_inter (const void* sendbuf, int sendcount, MPI_Datatype sendtype,
                                     void* recvbuf, int recvcount, MPI_Datatype recvtype, int root,
                                     struct ompi_communicator_t *comm, ompi_request_t ** request,
                                     struct mca_coll_base_module_2_2_0_t *module) {
    int res, rsize;
    MPI_Aint sndext;
    PNBC_Schedule *schedule;
    char *sbuf;
    PNBC_Handle *handle;
    ompi_coll_libpnbc_module_t *libpnbc_module = (ompi_coll_libpnbc_module_t*) module;

    rsize = ompi_comm_remote_size (comm);

    if (MPI_ROOT == root) {
        res = ompi_datatype_type_extent(sendtype, &sndext);
        if (MPI_SUCCESS != res) {
            PNBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
            return res;
        }
    }

    schedule = OBJ_NEW(PNBC_Schedule);
    if (OPAL_UNLIKELY(NULL == schedule)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* receive from root */
    if (MPI_ROOT != root && MPI_PROC_NULL != root) {
        /* recv msg from remote root */
        res = PNBC_Sched_recv(recvbuf, false, recvcount, recvtype, root, schedule, false);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
            OBJ_RELEASE(schedule);
            return res;
        }
    } else if (MPI_ROOT == root) {
        for (int i = 0 ; i < rsize ; ++i) {
            sbuf = ((char *)sendbuf) + (i * sendcount * sndext);
            /* root sends the right buffer to the right receiver */
            res = PNBC_Sched_send(sbuf, false, sendcount, sendtype, i, schedule, false);
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

    res = PNBC_Start_internal(handle, schedule);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        PNBC_Return_handle (handle);
        return res;
    }

    *request = (ompi_request_t *) handle;

    return OMPI_SUCCESS;
}
