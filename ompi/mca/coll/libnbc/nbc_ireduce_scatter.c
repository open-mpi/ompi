/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      The University of Tennessee and The University
 *                         of Tennessee Research Foundation. All rights
 *                         reserved.
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "nbc_internal.h"

/* an reduce_csttare schedule can not be cached easily because the contents
 * ot the recvcounts array may change, so a comparison of the address
 * would not be sufficient ... we simply do not cache it */

/* binomial reduce to rank 0 followed by a linear scatter ...
 *
 * Algorithm:
 * pairwise exchange
 * round r:
 *  grp = rank % 2^r
 *  if grp == 0: receive from rank + 2^(r-1) if it exists and reduce value
 *  if grp == 1: send to rank - 2^(r-1) and exit function
 *
 * do this for R=log_2(p) rounds
 *
 */

int ompi_coll_libnbc_ireduce_scatter(const void* sendbuf, void* recvbuf, const int *recvcounts, MPI_Datatype datatype,
                                     MPI_Op op, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                     struct mca_coll_base_module_2_1_0_t *module) {
  int peer, rank, maxr, p, res, count;
  MPI_Aint ext;
  char *redbuf, *sbuf, inplace;
  NBC_Schedule *schedule;
  NBC_Handle *handle;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

  NBC_IN_PLACE(sendbuf, recvbuf, inplace);

  rank = ompi_comm_rank (comm);
  p = ompi_comm_size (comm);

  res = ompi_datatype_type_extent (datatype, &ext);
  if (MPI_SUCCESS != res) {
    NBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
    return res;
  }

  count = 0;
  for (int r = 0 ; r < p ; ++r) {
    count += recvcounts[r];
  }

  if (p == 1 || 0 == count) {
    if (!inplace) {
      /* single node not in_place: copy data to recvbuf */
      res = NBC_Copy(sendbuf, recvcounts[0], datatype, recvbuf, recvcounts[0], datatype, comm);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }

    *request = &ompi_request_empty;
    return OMPI_SUCCESS;
  }

  res = NBC_Init_handle (comm, &handle, libnbc_module);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    return res;
  }

  maxr = (int) ceil ((log((double) p) / LOG2));

  handle->tmpbuf = malloc (ext * count * 2);
  if (OPAL_UNLIKELY(NULL == handle->tmpbuf)) {
    NBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  redbuf = (char *) handle->tmpbuf + ext * count;

  schedule = OBJ_NEW(NBC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    NBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  /* make sure the schedule is released with the handle on error */
  handle->schedule = schedule;

  for (int r = 1, firstred = 1 ; r <= maxr ; ++r) {
    if ((rank % (1 << r)) == 0) {
      /* we have to receive this round */
      peer = rank + (1 << (r - 1));
      if (peer < p) {
        /* we have to wait until we have the data */
        res = NBC_Sched_recv(0, true, count, datatype, peer, schedule, true);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          NBC_Return_handle (handle);
          return res;
        }

        /* this cannot be done until handle->tmpbuf is unused :-( so barrier after the op */
        if (firstred) {
          /* take reduce data from the sendbuf in the first round -> save copy */
          res = NBC_Sched_op (redbuf - (intptr_t) handle->tmpbuf, true, sendbuf, false, 0, true, count, datatype,
                              op, schedule, true);
          firstred = 0;
        } else {
          /* perform the reduce in my local buffer */
          res = NBC_Sched_op (redbuf - (intptr_t) handle->tmpbuf, true, redbuf - (intptr_t) handle->tmpbuf, true,
                              0, true, count, datatype, op, schedule, true);
        }

        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          NBC_Return_handle (handle);
          return res;
        }
      }
    } else {
      /* we have to send this round */
      peer = rank - (1 << (r - 1));
      if (firstred) {
        /* we have to send the senbuf */
        res = NBC_Sched_send (sendbuf, false, count, datatype, peer, schedule, false);
      } else {
        /* we send an already reduced value from redbuf */
        res = NBC_Sched_send (redbuf - (intptr_t) handle->tmpbuf, true, count, datatype, peer, schedule, false);
      }
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        NBC_Return_handle (handle);
        return res;
      }

      /* leave the game */
      break;
    }
  }

  res = NBC_Sched_barrier(schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    NBC_Return_handle (handle);
    return res;
  }

  /* rank 0 is root and sends - all others receive */
  if (rank == 0) {
    for (long int r = 1, offset = 0 ; r < p ; ++r) {
      offset += recvcounts[r-1];
      sbuf = redbuf + offset * ext;
      /* root sends the right buffer to the right receiver */
      res = NBC_Sched_send (sbuf - (intptr_t) handle->tmpbuf, true, recvcounts[r], datatype, r, schedule,
                            false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        NBC_Return_handle (handle);
        return res;
      }
    }

    res = NBC_Sched_copy (redbuf - (intptr_t) handle->tmpbuf, true, recvcounts[0], datatype, recvbuf, false,
                          recvcounts[0], datatype, schedule, false);
  } else {
    res = NBC_Sched_recv (recvbuf, false, recvcounts[rank], datatype, 0, schedule, false);
  }

  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    NBC_Return_handle (handle);
    return res;
  }

  res = NBC_Sched_commit (schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    NBC_Return_handle (handle);
    return res;
  }

  res = NBC_Start (handle, schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    NBC_Return_handle (handle);
    return res;
  }

  *request = (ompi_request_t *) handle;

  /* tmpbuf is freed with the handle */
  return OMPI_SUCCESS;
}

int ompi_coll_libnbc_ireduce_scatter_inter (const void* sendbuf, void* recvbuf, const int *recvcounts, MPI_Datatype datatype,
                                            MPI_Op op, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                            struct mca_coll_base_module_2_1_0_t *module) {
  int rank, res, count, rsize;
  MPI_Aint ext;
  NBC_Schedule *schedule;
  NBC_Handle *handle;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

  rank = ompi_comm_rank (comm);
  rsize = ompi_comm_remote_size (comm);

  res = ompi_datatype_type_extent (datatype, &ext);
  if (MPI_SUCCESS != res) {
    NBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
    return res;
  }

  count = 0;
  for (int r = 0 ; r < rsize ; ++r) {
    count += recvcounts[r];
  }

  res = NBC_Init_handle(comm, &handle, libnbc_module);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    return res;
  }

  if (count > 0) {
    handle->tmpbuf = malloc (2 * ext * count);
    if (OPAL_UNLIKELY(NULL == handle->tmpbuf)) {
      NBC_Return_handle (handle);
      return OMPI_ERR_OUT_OF_RESOURCE;
    }
  }

  schedule = OBJ_NEW(NBC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    NBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  /* make sure the schedule is released with the handle on error */
  handle->schedule = schedule;

  /* send my data to the remote root */
  res = NBC_Sched_send(sendbuf, false, count, datatype, 0, schedule, false);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    NBC_Return_handle (handle);
    return res;
  }

  if (0 == rank) {
    res = NBC_Sched_recv ((void *) 0, true, count, datatype, 0, schedule, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      NBC_Return_handle (handle);
      return res;
    }

    for (int peer = 1 ; peer < rsize ; ++peer) {
      res = NBC_Sched_recv ((void *)(ext * count), true, count, datatype, peer, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        NBC_Return_handle (handle);
        return res;
      }

      res = NBC_Sched_op ((void *) 0, true, (void *)(ext * count), true, (void *) 0, true, count, datatype,
                          op, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        NBC_Return_handle (handle);
        return res;
      }
    }

    /* exchange data with remote root for scatter phase (we *could* use the local communicator to do the scatter) */
    res = NBC_Sched_recv ((void *)(ext * count), true, count, datatype, 0, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      NBC_Return_handle (handle);
      return res;
    }

    res = NBC_Sched_send ((void *) 0, true, count, datatype, 0, schedule, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      NBC_Return_handle (handle);
      return res;
    }

    /* scatter */
    for (int peer = 0, offset = ext * count ; peer < rsize ; ++peer) {
      res = NBC_Sched_send ((void *)(uintptr_t) offset, true, recvcounts[peer], datatype, peer, schedule,
                            false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        NBC_Return_handle (handle);
        return res;
      }

      offset += recvcounts[peer] * ext;
    }
  }

  /* receive my block */
  res = NBC_Sched_recv (recvbuf, false, recvcounts[rank], datatype, 0, schedule, false);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    NBC_Return_handle (handle);
    return res;
  }

  res = NBC_Sched_commit (schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    NBC_Return_handle (handle);
    return res;
  }

  res = NBC_Start (handle, schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    NBC_Return_handle (handle);
    return res;
  }

  *request = (ompi_request_t *) handle;

  /* tmpbuf is freed with the handle */
  return OMPI_SUCCESS;
}
