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

#ifdef PNBC_CACHE_SCHEDULE
/* tree comparison function for schedule cache */
int PNBC_Scatter_args_compare(PNBC_Scatter_args *a, PNBC_Scatter_args *b, void *param) {
    if ((a->sendbuf == b->sendbuf) &&
        (a->sendcount == b->sendcount) &&
        (a->sendtype == b->sendtype) &&
        (a->recvbuf == b->recvbuf) &&
        (a->recvcount == b->recvcount) &&
        (a->recvtype == b->recvtype) &&
        (a->root == b->root)) {
        return 0;
    }

    if (a->sendbuf < b->sendbuf) {
        return -1;
    }

    return 1;
}
#endif

/* simple linear MPI_Iscatter */
int ompi_coll_libpnbc_iscatter (const void* sendbuf, int sendcount, MPI_Datatype sendtype,
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

#ifdef PNBC_CACHE_SCHEDULE
  PNBC_Scatter_args *args, *found, search;

  /* search schedule in communicator specific tree */
  search.sendbuf=sendbuf;
  search.sendcount=sendcount;
  search.sendtype=sendtype;
  search.recvbuf=recvbuf;
  search.recvcount=recvcount;
  search.recvtype=recvtype;
  search.root=root;
  found = (PNBC_Scatter_args *) hb_tree_search ((hb_tree *) libpnbc_module->PNBC_Dict[PNBC_SCATTER], &search);
  if (NULL == found) {
#endif
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
#ifdef PNBC_CACHE_SCHEDULE
    /* save schedule to tree */
    args = (PNBC_Scatter_args *) malloc (sizeof (args));
    if (NULL != args) {
      args->sendbuf = sendbuf;
      args->sendcount = sendcount;
      args->sendtype = sendtype;
      args->recvbuf = recvbuf;
      args->recvcount = recvcount;
      args->recvtype = recvtype;
      args->root = root;
      args->schedule = schedule;
      res = hb_tree_insert ((hb_tree *) libpnbc_module->PNBC_Dict[PNBC_SCATTER], args, args, 0);
      if (0 == res) {
        OBJ_RETAIN(schedule);

        /* increase number of elements for A2A */
        if (++libpnbc_module->PNBC_Dict_size[PNBC_SCATTER] > PNBC_SCHED_DICT_UPPER) {
          PNBC_SchedCache_dictwipe ((hb_tree *) libpnbc_module->PNBC_Dict[PNBC_SCATTER],
                                   &libpnbc_module->PNBC_Dict_size[PNBC_SCATTER]);
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

  res = PNBC_Init_handle(comm, &handle, libpnbc_module);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    return res;
  }

  res = PNBC_Start(handle, schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    PNBC_Return_handle (handle);
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

    res = PNBC_Start(handle, schedule);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        PNBC_Return_handle (handle);
        return res;
    }

    *request = (ompi_request_t *) handle;

    return OMPI_SUCCESS;
}
