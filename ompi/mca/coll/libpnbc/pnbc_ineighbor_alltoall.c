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

/* cannot cache schedules because one cannot check locally if the pattern is the same!! */
#undef PNBC_CACHE_SCHEDULE

#ifdef PNBC_CACHE_SCHEDULE
/* tree comparison function for schedule cache */
int PNBC_Ineighbor_alltoall_args_compare(PNBC_Ineighbor_alltoall_args *a, PNBC_Ineighbor_alltoall_args *b, void *param) {
  if( (a->sbuf == b->sbuf) &&
      (a->scount == b->scount) &&
      (a->stype == b->stype) &&
      (a->rbuf == b->rbuf) &&
      (a->rcount == b->rcount) &&
      (a->rtype == b->rtype) ) {
    return  0;
  }
  if( a->sbuf < b->sbuf ) {
    return -1;
  }
  return +1;
}
#endif

int ompi_coll_libpnbc_ineighbor_alltoall(const void *sbuf, int scount, MPI_Datatype stype, void *rbuf,
                                        int rcount, MPI_Datatype rtype, struct ompi_communicator_t *comm,
                                        ompi_request_t ** request, struct mca_coll_base_module_2_2_0_t *module) {
  int res, indegree, outdegree, *srcs, *dsts;
  MPI_Aint sndext, rcvext;
  PNBC_Handle *handle;
  ompi_coll_libpnbc_module_t *libpnbc_module = (ompi_coll_libpnbc_module_t*) module;
  PNBC_Schedule *schedule;

  res = ompi_datatype_type_extent(stype, &sndext);
  if (MPI_SUCCESS != res) {
    PNBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
    return res;
  }

  res = ompi_datatype_type_extent(rtype, &rcvext);
  if (MPI_SUCCESS != res) {
    PNBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
    return res;
  }

#ifdef PNBC_CACHE_SCHEDULE
  PNBC_Ineighbor_alltoall_args *args, *found, search;

  /* search schedule in communicator specific tree */
  search.sbuf = sbuf;
  search.scount = scount;
  search.stype = stype;
  search.rbuf = rbuf;
  search.rcount = rcount;
  search.rtype = rtype;
  found = (PNBC_Ineighbor_alltoall_args *) hb_tree_search ((hb_tree *) libpnbc_module->PNBC_Dict[PNBC_NEIGHBOR_ALLTOALL],
                                                          &search);
  if (NULL == found) {
#endif
    schedule = OBJ_NEW(PNBC_Schedule);
    if (OPAL_UNLIKELY(NULL == schedule)) {
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    res = PNBC_Comm_neighbors(comm, &srcs, &indegree, &dsts, &outdegree);
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
        res = PNBC_Sched_send ((char *) sbuf + i * scount * sndext, false, scount, stype, dsts[i], schedule, false);
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

#ifdef PNBC_CACHE_SCHEDULE
    /* save schedule to tree */
    args = (PNBC_Ineighbor_alltoall_args *) malloc (sizeof (args));
    if (NULL != args) {
      args->sbuf = sbuf;
      args->scount = scount;
      args->stype = stype;
      args->rbuf = rbuf;
      args->rcount = rcount;
      args->rtype = rtype;
      args->schedule = schedule;
      res = hb_tree_insert ((hb_tree *) libpnbc_module->PNBC_Dict[PNBC_NEIGHBOR_ALLTOALL], args, args, 0);
      if (0 == res) {
        OBJ_RETAIN(schedule);

        /* increase number of elements for A2A */
        if (++libpnbc_module->PNBC_Dict_size[PNBC_NEIGHBOR_ALLTOALL] > PNBC_SCHED_DICT_UPPER) {
          PNBC_SchedCache_dictwipe ((hb_tree *) libpnbc_module->PNBC_Dict[PNBC_NEIGHBOR_ALLTOALL],
                                   &libpnbc_module->PNBC_Dict_size[PNBC_NEIGHBOR_ALLTOALL]);
        }
      } else {
        PNBC_Error("error in dict_insert() (%i)", res);
        free (args);
      }
    }
  } else {
    /* found schedule */
    schedule = found->schedule;
    OBJ_RETAIN(schedule);
  }
#endif

  res = PNBC_Init_handle (comm, &handle, libpnbc_module);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    return res;
  }

  res = PNBC_Start (handle, schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    PNBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  *request = (ompi_request_t *) handle;

  return OMPI_SUCCESS;
}
