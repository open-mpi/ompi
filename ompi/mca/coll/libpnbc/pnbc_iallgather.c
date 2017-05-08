/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2014-2017 Research Organization for Information Science
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

#ifdef PNBC_CACHE_SCHEDULE
/* tree comparison function for schedule cache */
int PNBC_Allgather_args_compare(PNBC_Allgather_args *a, PNBC_Allgather_args *b, void *param) {
  if ((a->sendbuf == b->sendbuf) &&
      (a->sendcount == b->sendcount) &&
      (a->sendtype == b->sendtype) &&
      (a->recvbuf == b->recvbuf) &&
      (a->recvcount == b->recvcount) &&
      (a->recvtype == b->recvtype) ) {
    return 0;
  }

  if( a->sendbuf < b->sendbuf ) {
    return -1;
  }

  return 1;
}
#endif

/* simple linear MPI_Iallgather
 * the algorithm uses p-1 rounds
 * each node sends the packet it received last round (or has in round 0) to it's right neighbor (modulo p)
 * each node receives from it's left (modulo p) neighbor */
int ompi_coll_libpnbc_iallgather(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                                MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                struct mca_coll_base_module_2_2_0_t *module)
{
  int rank, p, res;
  MPI_Aint rcvext;
  PNBC_Schedule *schedule;
  char *rbuf, *sbuf, inplace;
#ifdef PNBC_CACHE_SCHEDULE
  PNBC_Allgather_args *args, *found, search;
#endif
  PNBC_Handle *handle;
  ompi_coll_libpnbc_module_t *libpnbc_module = (ompi_coll_libpnbc_module_t*) module;

  PNBC_IN_PLACE(sendbuf, recvbuf, inplace);

  rank = ompi_comm_rank (comm);
  p = ompi_comm_size (comm);

  res = ompi_datatype_type_extent(recvtype, &rcvext);
  if (MPI_SUCCESS != res) {
    return res;
  }

  if (inplace) {
    sendtype = recvtype;
    sendcount = recvcount;
  } else {
    /* copy my data to receive buffer */
    rbuf = (char *) recvbuf + rank * recvcount * rcvext;
    res = PNBC_Copy (sendbuf, sendcount, sendtype, rbuf, recvcount, recvtype, comm);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
  }
  if (1 == p) {
    *request = &ompi_request_empty;
    return OMPI_SUCCESS;
  }

#ifdef PNBC_CACHE_SCHEDULE
  /* search schedule in communicator specific tree */
  search.sendbuf = sendbuf;
  search.sendcount = sendcount;
  search.sendtype = sendtype;
  search.recvbuf = recvbuf;
  search.recvcount = recvcount;
  search.recvtype = recvtype;
  found = (PNBC_Allgather_args *) hb_tree_search ((hb_tree*)libpnbc_module->PNBC_Dict[PNBC_ALLGATHER], &search);
  if (NULL == found) {
#endif
    schedule = OBJ_NEW(PNBC_Schedule);
    if (OPAL_UNLIKELY(NULL == schedule)) {
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    sbuf = (char *)recvbuf + rank * recvcount * rcvext;
    /* do p-1 rounds */
    for(int r = 0 ; r < p ; ++r) {
      if(r != rank) {
        /* recv from rank r */
        rbuf = (char *)recvbuf + r * recvcount * rcvext;
        res = PNBC_Sched_recv (rbuf, false, recvcount, recvtype, r, schedule, false);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          OBJ_RELEASE(schedule);
          return res;
        }

        /* send to rank r - not from the sendbuf to optimize MPI_IN_PLACE */
        res = PNBC_Sched_send (sbuf, false, recvcount, recvtype, r, schedule, false);
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

#ifdef PNBC_CACHE_SCHEDULE
    /* save schedule to tree */
    args = (PNBC_Allgather_args *) malloc (sizeof (args));
    args->sendbuf = sendbuf;
    args->sendcount = sendcount;
    args->sendtype = sendtype;
    args->recvbuf = recvbuf;
    args->recvcount = recvcount;
    args->recvtype = recvtype;
    args->schedule = schedule;

    res = hb_tree_insert ((hb_tree *) libpnbc_module->PNBC_Dict[PNBC_ALLGATHER], args, args, 0);
    if (res != 0) {
      free (args);
    } else {
      OBJ_RETAIN(schedule);
    }

    /* increase number of elements for A2A */
    if (++libpnbc_module->PNBC_Dict_size[PNBC_ALLGATHER] > PNBC_SCHED_DICT_UPPER) {
      PNBC_SchedCache_dictwipe ((hb_tree *) libpnbc_module->PNBC_Dict[PNBC_ALLGATHER], &libpnbc_module->PNBC_Dict_size[PNBC_ALLGATHER]);
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
    OMPI_COLL_LIBPNBC_REQUEST_RETURN(handle);
    return res;
  }

  *request = (ompi_request_t *) handle;

  return OMPI_SUCCESS;
}

int ompi_coll_libpnbc_iallgather_inter(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
				      MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
				      struct mca_coll_base_module_2_2_0_t *module)
{
  int res, rsize;
  MPI_Aint rcvext;
  PNBC_Schedule *schedule;
  char *rbuf;
  PNBC_Handle *handle;
  ompi_coll_libpnbc_module_t *libpnbc_module = (ompi_coll_libpnbc_module_t*) module;

  res = ompi_datatype_type_extent(recvtype, &rcvext);
  if (MPI_SUCCESS != res) {
    PNBC_Error ("MPI Error in ompi_datatype_type_extent() (%i)", res);
    return res;
  }

  rsize = ompi_comm_remote_size (comm);

  /* set up schedule */
  schedule = OBJ_NEW(PNBC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  /* do rsize - 1 rounds */
  for (int r = 0 ; r < rsize ; ++r) {
    /* recv from rank r */
    rbuf = (char *) recvbuf + r * recvcount * rcvext;
    res = PNBC_Sched_recv (rbuf, false, recvcount, recvtype, r, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      OBJ_RELEASE(schedule);
      return res;
    }

    /* send to rank r */
    res = PNBC_Sched_send (sendbuf, false, sendcount, sendtype, r, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      OBJ_RELEASE(schedule);
      return res;
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

  res = PNBC_Start (handle, schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OMPI_COLL_LIBPNBC_REQUEST_RETURN(handle);
    return res;
  }

  *request = (ompi_request_t *) handle;

  return OMPI_SUCCESS;
}
