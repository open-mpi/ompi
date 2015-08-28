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
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "nbc_internal.h"

/* an alltoallw schedule can not be cached easily because the contents
 * ot the recvcounts array may change, so a comparison of the address
 * would not be sufficient ... we simply do not cache it */

/* simple linear Alltoallw */
int ompi_coll_libnbc_ialltoallw(const void* sendbuf, const int *sendcounts, const int *sdispls,
                                struct ompi_datatype_t * const *sendtypes, void* recvbuf, const int *recvcounts, const int *rdispls,
                                struct ompi_datatype_t * const *recvtypes, struct ompi_communicator_t *comm, ompi_request_t ** request,
				struct mca_coll_base_module_2_1_0_t *module)
{
  int rank, p, res;
  NBC_Schedule *schedule;
  char *rbuf, *sbuf, inplace;
  NBC_Handle *handle;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

  NBC_IN_PLACE(sendbuf, recvbuf, inplace);

  rank = ompi_comm_rank (comm);
  p = ompi_comm_size (comm);

  /* copy data to receivbuffer */
  if ((sendcounts[rank] != 0) && !inplace) {
    rbuf = (char *) recvbuf + rdispls[rank];
    sbuf = (char *) sendbuf + sdispls[rank];
    res = NBC_Copy(sbuf, sendcounts[rank], sendtypes[rank], rbuf, recvcounts[rank], recvtypes[rank], comm);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
  }

  schedule = OBJ_NEW(NBC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  for (int i = 0; i < p; i++) {
    if (i == rank) {
      continue;
    }

    /* post all sends */
    if (sendcounts[i] != 0) {
      sbuf = (char *) sendbuf + sdispls[i];
      res = NBC_Sched_send (sbuf, false, sendcounts[i], sendtypes[i], i, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        OBJ_RELEASE(schedule);
        return res;
      }
    }
    /* post all receives */
    if (recvcounts[i] != 0) {
      rbuf = (char *) recvbuf + rdispls[i];
      res = NBC_Sched_recv (rbuf, false, recvcounts[i], recvtypes[i], i, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        OBJ_RELEASE(schedule);
        return res;
      }
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

/* simple linear Alltoallw */
int ompi_coll_libnbc_ialltoallw_inter (const void* sendbuf, const int *sendcounts, const int *sdispls,
				       struct ompi_datatype_t * const *sendtypes, void* recvbuf, const int *recvcounts, const int *rdispls,
				       struct ompi_datatype_t * const *recvtypes, struct ompi_communicator_t *comm, ompi_request_t ** request,
				       struct mca_coll_base_module_2_1_0_t *module)
{
  int res, rsize;
  NBC_Schedule *schedule;
  char *rbuf, *sbuf;
  NBC_Handle *handle;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

  rsize = ompi_comm_remote_size (comm);

  schedule = OBJ_NEW(NBC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  for (int i = 0 ; i < rsize ; ++i) {
    /* post all sends */
    if (sendcounts[i] != 0) {
      sbuf = (char *) sendbuf + sdispls[i];
      res = NBC_Sched_send (sbuf, false, sendcounts[i], sendtypes[i], i, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        OBJ_RELEASE(schedule);
        return res;
      }
    }
    /* post all receives */
    if (recvcounts[i] != 0) {
      rbuf = (char *) recvbuf + rdispls[i];
      res = NBC_Sched_recv (rbuf, false, recvcounts[i], recvtypes[i], i, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        OBJ_RELEASE(schedule);
        return res;
      }
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
