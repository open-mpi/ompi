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
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "nbc_internal.h"

/* a scatterv schedule can not be cached easily because the contents
 * ot the recvcounts array may change, so a comparison of the address
 * would not be sufficient ... we simply do not cache it */

/* simple linear MPI_Iscatterv */
int ompi_coll_libnbc_iscatterv(const void* sendbuf, const int *sendcounts, const int *displs, MPI_Datatype sendtype,
                               void* recvbuf, int recvcount, MPI_Datatype recvtype, int root,
                               struct ompi_communicator_t *comm, ompi_request_t ** request,
                               struct mca_coll_base_module_2_1_0_t *module) {
  int rank, p, res;
  MPI_Aint sndext;
  NBC_Schedule *schedule;
  char *sbuf, inplace;
  NBC_Handle *handle;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

  NBC_IN_PLACE(sendbuf, recvbuf, inplace);

  rank = ompi_comm_rank (comm);
  p = ompi_comm_size (comm);

  schedule = OBJ_NEW(NBC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  /* receive from root */
  if (rank == root) {
    res = ompi_datatype_type_extent (sendtype, &sndext);
    if (MPI_SUCCESS != res) {
      NBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
      OBJ_RELEASE(schedule);
      return res;
    }

    for (int i = 0 ; i < p ; ++i) {
      sbuf = (char *) sendbuf + displs[i] * sndext;
      if (i == root) {
        if (!inplace) {
          /* if I am the root - just copy the message */
          res = NBC_Copy (sbuf, sendcounts[i], sendtype, recvbuf, recvcount, recvtype, comm);
        } else {
          res = OMPI_SUCCESS;
        }
      } else {
        /* root sends the right buffer to the right receiver */
        res = NBC_Sched_send (sbuf, false, sendcounts[i], sendtype, i, schedule, false);
      }

      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        OBJ_RELEASE(schedule);
        return res;
      }
    }
  } else {
    /* recv msg from root */
    res = NBC_Sched_recv (recvbuf, false, recvcount, recvtype, root, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      OBJ_RELEASE(schedule);
      return res;
    }
  }

  res = NBC_Sched_commit (schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    return res;
  }

  res = NBC_Init_handle (comm, &handle, libnbc_module);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    return res;
  }

  res = NBC_Start (handle, schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    NBC_Return_handle (handle);
    return res;
  }

  *request = (ompi_request_t *) handle;

  return OMPI_SUCCESS;
}

int ompi_coll_libnbc_iscatterv_inter (const void* sendbuf, const int *sendcounts, const int *displs, MPI_Datatype sendtype,
                                      void* recvbuf, int recvcount, MPI_Datatype recvtype, int root,
                                      struct ompi_communicator_t *comm, ompi_request_t ** request,
                                      struct mca_coll_base_module_2_1_0_t *module) {
    int res, rsize;
    MPI_Aint sndext;
    NBC_Schedule *schedule;
    char *sbuf;
    NBC_Handle *handle;
    ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

    rsize = ompi_comm_remote_size (comm);

    schedule = OBJ_NEW(NBC_Schedule);
    if (OPAL_UNLIKELY(NULL == schedule)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* receive from root */
    if (MPI_ROOT != root && MPI_PROC_NULL != root) {
        /* recv msg from root */
        res = NBC_Sched_recv(recvbuf, false, recvcount, recvtype, root, schedule, false);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
            OBJ_RELEASE(schedule);
            return res;
        }
    } else if (MPI_ROOT == root) {
        res = ompi_datatype_type_extent(sendtype, &sndext);
        if (MPI_SUCCESS != res) {
            NBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
            OBJ_RELEASE(schedule);
            return res;
        }

        for (int i = 0 ; i < rsize ; ++i) {
            sbuf = (char *)sendbuf + displs[i] * sndext;
            /* root sends the right buffer to the right receiver */
            res = NBC_Sched_send (sbuf, false, sendcounts[i], sendtype, i, schedule, false);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
                OBJ_RELEASE(schedule);
                return res;
            }
        }
    }

    res = NBC_Sched_commit(schedule);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        OBJ_RELEASE(schedule);
        return res;
    }

    res = NBC_Init_handle(comm, &handle, libnbc_module);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        OBJ_RELEASE(schedule);
        return res;
    }

    res = NBC_Start(handle, schedule);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        NBC_Return_handle (handle);
        return res;
    }

    *request = (ompi_request_t *) handle;

    return OMPI_SUCCESS;
}
