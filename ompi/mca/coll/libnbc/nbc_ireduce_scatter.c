/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      The University of Tennessee and The University
 *                         of Tennessee Research Foundation. All rights
 *                         reserved.
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "opal/include/opal/align.h"

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
  ptrdiff_t gap, span, span_align;
  char *sbuf, inplace;
  NBC_Schedule *schedule;
  NBC_Handle *handle;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;
  char *rbuf, *lbuf, *buf;

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

  span = opal_datatype_span(&datatype->super, count, &gap);
  span_align = OPAL_ALIGN(span, datatype->super.align, ptrdiff_t);
  handle->tmpbuf = malloc (span_align + span);
  if (OPAL_UNLIKELY(NULL == handle->tmpbuf)) {
    NBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  rbuf = (char *)(-gap);
  lbuf = (char *)(span_align - gap);

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
        res = NBC_Sched_recv(rbuf, true, count, datatype, peer, schedule, true);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          NBC_Return_handle (handle);
          return res;
        }

        /* this cannot be done until handle->tmpbuf is unused :-( so barrier after the op */
        if (firstred) {
          /* take reduce data from the sendbuf in the first round -> save copy */
          res = NBC_Sched_op (sendbuf, false, rbuf, true, count, datatype, op, schedule, true);
          firstred = 0;
        } else {
          /* perform the reduce in my local buffer */
          res = NBC_Sched_op (lbuf, true, rbuf, true, count, datatype, op, schedule, true);
        }

        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          NBC_Return_handle (handle);
          return res;
        }
        /* swap left and right buffers */
        buf = rbuf; rbuf = lbuf ; lbuf = buf;
      }
    } else {
      /* we have to send this round */
      peer = rank - (1 << (r - 1));
      if (firstred) {
        /* we have to send the senbuf */
        res = NBC_Sched_send (sendbuf, false, count, datatype, peer, schedule, false);
      } else {
        /* we send an already reduced value from lbuf */
        res = NBC_Sched_send (lbuf, true, count, datatype, peer, schedule, false);
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
      sbuf = lbuf + (offset*ext);
      /* root sends the right buffer to the right receiver */
      res = NBC_Sched_send (sbuf, true, recvcounts[r], datatype, r, schedule,
                            false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        NBC_Return_handle (handle);
        return res;
      }
    }

    res = NBC_Sched_copy (lbuf, true, recvcounts[0], datatype, recvbuf, false,
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
  int rank, res, count, lsize, rsize;
  MPI_Aint ext;
  ptrdiff_t gap, span, span_align;
  NBC_Schedule *schedule;
  NBC_Handle *handle;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

  rank = ompi_comm_rank (comm);
  lsize = ompi_comm_size(comm);
  rsize = ompi_comm_remote_size (comm);

  res = ompi_datatype_type_extent (datatype, &ext);
  if (MPI_SUCCESS != res) {
    NBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
    return res;
  }

  count = 0;
  for (int r = 0 ; r < lsize ; ++r) {
    count += recvcounts[r];
  }

  span = opal_datatype_span(&datatype->super, count, &gap);
  span_align = OPAL_ALIGN(span, datatype->super.align, ptrdiff_t);

  res = NBC_Init_handle(comm, &handle, libnbc_module);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    return res;
  }

  if (count > 0) {
    handle->tmpbuf = malloc (span_align + span);
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
    char *lbuf, *rbuf;
    lbuf = (char *)(-gap);
    rbuf = (char *)(span_align-gap);
    res = NBC_Sched_recv (lbuf, true, count, datatype, 0, schedule, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      NBC_Return_handle (handle);
      return res;
    }

    for (int peer = 1 ; peer < rsize ; ++peer) {
      char *tbuf;
      res = NBC_Sched_recv (rbuf, true, count, datatype, peer, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        NBC_Return_handle (handle);
        return res;
      }

      res = NBC_Sched_op (lbuf, true, rbuf, true, count, datatype,
                          op, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        NBC_Return_handle (handle);
        return res;
      }
      tbuf = lbuf; lbuf = rbuf; rbuf = tbuf;
    }

    /* do the local scatterv with the local communicator */
    res = NBC_Sched_copy (lbuf, true, recvcounts[0], datatype, recvbuf, false,
                          recvcounts[0], datatype, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      NBC_Return_handle (handle);
      return res;
    }
    for (int peer = 1, offset = recvcounts[0] * ext; peer < lsize ; ++peer) {
      res = NBC_Sched_local_send (lbuf + offset, true, recvcounts[peer], datatype, peer, schedule,
                                  false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        NBC_Return_handle (handle);
        return res;
      }

      offset += recvcounts[peer] * ext;
    }
  } else {
    /* receive my block */
    res = NBC_Sched_local_recv (recvbuf, false, recvcounts[rank], datatype, 0, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      NBC_Return_handle (handle);
      return res;
    }
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
