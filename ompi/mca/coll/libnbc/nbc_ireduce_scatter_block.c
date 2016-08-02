/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All 
 *                         rights reserved.
 * Copyright (c) 2012      Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "opal/include/opal/align.h"

#include "nbc_internal.h"

/* an reduce_csttare schedule can not be cached easily because the contents
 * ot the recvcount value may change, so a comparison of the address
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

int ompi_coll_libnbc_ireduce_scatter_block(void* sendbuf, void* recvbuf, int recvcount, MPI_Datatype datatype, 
                                     MPI_Op op, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                     struct mca_coll_base_module_2_0_0_t *module) {
  int peer, rank, maxr, p, r, res, count, offset, firstred;
  MPI_Aint ext;
  ptrdiff_t gap, span;
  char *redbuf, *sbuf, inplace;
  NBC_Schedule *schedule;
  NBC_Handle *handle;
  ompi_coll_libnbc_request_t **coll_req = (ompi_coll_libnbc_request_t**) request;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;
  
  NBC_IN_PLACE(sendbuf, recvbuf, inplace);

  rank = ompi_comm_rank (comm);
  p = ompi_comm_size (comm);
  res = MPI_Type_extent(datatype, &ext);
  if (MPI_SUCCESS != res || 0 == ext) { printf("MPI Error in MPI_Type_extent() (%i:%i)\n", res, (int)ext); return (MPI_SUCCESS == res) ? MPI_ERR_SIZE : res; }

  res = NBC_Init_handle(comm, coll_req, libnbc_module);
  if(res != NBC_OK) { printf("Error in NBC_Init_handle(%i)\n", res); return res; }
  handle = (*coll_req);
  
  schedule = (NBC_Schedule*)malloc(sizeof(NBC_Schedule));
  if (NULL == schedule) { printf("Error in malloc()\n"); return NBC_OOR; }

  res = NBC_Sched_create(schedule);
  if(res != NBC_OK) { printf("Error in NBC_Sched_create (%i)\n", res); return res; }

  maxr = (int)ceil((log(p)/LOG2));

  count = p * recvcount;
  
  if (0 < count) {
    char *rbuf, *lbuf, *buf;
    ptrdiff_t span_align;

    span = opal_datatype_span(&datatype->super, count, &gap);
    span_align = OPAL_ALIGN(span, datatype->super.align, ptrdiff_t);
    handle->tmpbuf = malloc (span_align + span);
    if (NULL == handle->tmpbuf) { printf("Error in malloc()\n"); return NBC_OOR; }

    rbuf = (void *)(-gap);
    lbuf = (char *)(span_align - gap);
    redbuf = (char *) handle->tmpbuf + span_align - gap;

    /* copy data to redbuf if we only have a single node */
    if((p==1) && !inplace) {
      res = NBC_Copy(sendbuf, count, datatype, redbuf, count, datatype, comm);
      if (NBC_OK != res) { printf("Error in NBC_Copy() (%i)\n", res); return res; }
    }
  
    firstred = 1;
    for(r=1; r<=maxr; r++) {
      if((rank % (1<<r)) == 0) {
        /* we have to receive this round */
        peer = rank + (1<<(r-1));
        if(peer<p) {
          res = NBC_Sched_recv(rbuf, true, count, datatype, peer, schedule);
          if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
          /* we have to wait until we have the data */
          res = NBC_Sched_barrier(schedule);
          if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }
          if(firstred) {
            /* take reduce data from the sendbuf in the first round -> save copy */
            res = NBC_Sched_op (sendbuf, false, rbuf, true, count, datatype, op, schedule);
            firstred = 0;
          } else {
            /* perform the reduce in my local buffer */
            res = NBC_Sched_op (lbuf, true, rbuf, true, count, datatype, op, schedule);
          }
          if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_op() (%i)\n", res); return res; }
          /* this cannot be done until handle->tmpbuf is unused :-( */
          res = NBC_Sched_barrier(schedule);
          if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }
          /* swap left and right buffers */
          buf = rbuf; rbuf = lbuf ; lbuf = buf;
        }
      } else {
        /* we have to send this round */
        peer = rank - (1<<(r-1));
        if(firstred) {
          /* we have to send the senbuf */
          res = NBC_Sched_send(sendbuf, false, count, datatype, peer, schedule);
        } else {
          /* we send an already reduced value from redbuf */
          res = NBC_Sched_send(lbuf, true, count, datatype, peer, schedule);
        }
        if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
        /* leave the game */
        break;
      }
    }
    
    res = NBC_Sched_barrier(schedule);
    if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }
  
    /* rank 0 is root and sends - all others receive */
    if(rank != 0) {
      res = NBC_Sched_recv(recvbuf, false, recvcount, datatype, 0, schedule);
     if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
    } else {
      offset = 0;
      for(r=1;r<p;r++) {
        offset += recvcount;
        sbuf = lbuf + (offset*ext);
        /* root sends the right buffer to the right receiver */
        res = NBC_Sched_send(sbuf, true, recvcount, datatype, r, schedule);
        if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
      }
      if ((p != 1) || !inplace) {
        res = NBC_Sched_copy (lbuf, true, recvcount, datatype, recvbuf, false, recvcount, datatype, schedule);
        if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_copy() (%i)\n", res); return res; }
      }
    }
  }

  /*NBC_PRINT_SCHED(*schedule);*/
  
  res = NBC_Sched_commit(schedule);
  if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_commit() (%i)\n", res); return res; }
  
  res = NBC_Start(handle, schedule);
  if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Start() (%i)\n", res); return res; }
  
  /* tmpbuf is freed with the handle */
  return NBC_OK;
}

int ompi_coll_libnbc_ireduce_scatter_block_inter(void *sendbuf, void *recvbuf, int rcount, struct ompi_datatype_t *dtype,
						 struct ompi_op_t *op, struct ompi_communicator_t *comm,
						 ompi_request_t **request, struct mca_coll_base_module_2_0_0_t *module) {
  int peer, rank, res, count, lsize, rsize;
  MPI_Aint ext;
  ptrdiff_t gap, span, span_align;
  NBC_Schedule *schedule;
  NBC_Handle *handle;
  ompi_coll_libnbc_request_t **coll_req = (ompi_coll_libnbc_request_t**) request;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

  res = NBC_Init_handle(comm, coll_req, libnbc_module);
  if(res != NBC_OK) { printf("Error in NBC_Init_handle(%i)\n", res); return res; }
  handle = (*coll_req);

  rank = ompi_comm_rank (comm);
  lsize = ompi_comm_size (comm);
  rsize = ompi_comm_remote_size (comm);

  res = ompi_datatype_type_extent (dtype, &ext);
  if (MPI_SUCCESS != res) { printf ("MPI Error in ompi_datatype_type_extent() (%i)", res); return res; }

  schedule = (NBC_Schedule*)malloc(sizeof(NBC_Schedule));
  if (NULL == schedule) { printf("Error in malloc()\n"); return NBC_OOR; }

  res = NBC_Sched_create(schedule);
  if(res != NBC_OK) { printf("Error in NBC_Sched_create (%i)\n", res); return res; }

  count = rcount * lsize;
  span = opal_datatype_span(&dtype->super, count, &gap);
  span_align = OPAL_ALIGN(span, dtype->super.align, ptrdiff_t);

  if (count > 0) {
    handle->tmpbuf = malloc (span_align + span);
    if(handle->tmpbuf == NULL) { printf("Error in malloc()\n"); return NBC_OOR; }
  }

  /* send my data to the remote root */
  res = NBC_Sched_send(sendbuf, false, count, dtype, 0, schedule);
  if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }

  if (0 == rank) {
    char *lbuf, *rbuf;
    lbuf = (char *)(-gap);
    rbuf = (char *)(span_align-gap);
    res = NBC_Sched_recv (lbuf, true, count, dtype, 0, schedule);
    if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }

    res = NBC_Sched_barrier(schedule);
    if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }

    for (peer = 1 ; peer < rsize ; ++peer) {
      char *tbuf;

      res = NBC_Sched_recv (rbuf, true, count, dtype, peer, schedule);
      if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }

      res = NBC_Sched_barrier(schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }

      res = NBC_Sched_op (lbuf, true, rbuf, true, count, dtype, op, schedule);
      if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_op() (%i)\n", res); return res; }

      res = NBC_Sched_barrier(schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }

      tbuf = lbuf; lbuf = rbuf; rbuf = tbuf;
    }

    /* do the scatter with the local communicator */
    res = NBC_Sched_copy (lbuf, true, rcount, dtype, recvbuf, false, rcount, dtype, schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_copy() (%i)\n", res); return res; }

    for (int peer = 1 ; peer < lsize ; ++peer) {
      res = NBC_Sched_local_send (lbuf + ext * rcount * peer, true, rcount, dtype, peer, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_local_send() (%i)\n", res); return res; }
    }
  } else {
    /* receive my block */
    res = NBC_Sched_local_recv(recvbuf, false, rcount, dtype, 0, schedule);
    if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_local_recv() (%i)\n", res); return res; }
  }

  /*NBC_PRINT_SCHED(*schedule);*/

  res = NBC_Sched_commit(schedule);
  if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_commit() (%i)\n", res); return res; }

  res = NBC_Start(handle, schedule);
  if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Start() (%i)\n", res); return res; }

  /* tmpbuf is freed with the handle */
  return NBC_OK;
}
