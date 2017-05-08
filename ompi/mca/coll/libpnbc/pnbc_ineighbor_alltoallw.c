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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "pnbc_internal.h"

int ompi_coll_libpnbc_ineighbor_alltoallw_init(const void *sbuf, const int *scounts, const MPI_Aint *sdisps, struct ompi_datatype_t * const *stypes,
                                         void *rbuf, const int *rcounts, const MPI_Aint *rdisps, struct ompi_datatype_t * const *rtypes,
                                         struct ompi_communicator_t *comm, ompi_request_t ** request,
                                         struct mca_coll_base_module_2_2_0_t *module) {
  int res, indegree, outdegree, *srcs, *dsts;
  PNBC_Handle *handle;
  ompi_coll_libpnbc_module_t *libpnbc_module = (ompi_coll_libpnbc_module_t*) module;
  PNBC_Schedule *schedule;

    schedule = OBJ_NEW(PNBC_Schedule);
    if (OPAL_UNLIKELY(NULL == schedule)) {
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    res = PNBC_Comm_neighbors (comm, &srcs, &indegree, &dsts, &outdegree);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      OBJ_RELEASE(schedule);
      return res;
    }

    /* simply loop over neighbors and post send/recv operations */
    for (int i = 0 ; i < indegree ; ++i) {
      if (srcs[i] != MPI_PROC_NULL) {
        res = PNBC_Sched_recv ((char *) rbuf + rdisps[i], false, rcounts[i], rtypes[i], srcs[i], schedule, false);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          break;
        }
      }
    }

    free (srcs);

    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      free (dsts);
      OBJ_RELEASE(schedule);
      return res;
    }

    for (int i = 0 ; i < outdegree ; ++i) {
      if (dsts[i] != MPI_PROC_NULL) {
        res = PNBC_Sched_send ((char *) sbuf + sdisps[i], false, scounts[i], stypes[i], dsts[i], schedule, false);
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

  *request = (ompi_request_t *) handle;

  return OMPI_SUCCESS;
}
