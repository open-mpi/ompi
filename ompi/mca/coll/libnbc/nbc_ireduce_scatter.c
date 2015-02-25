/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
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

int ompi_coll_libnbc_ireduce_scatter(void* sendbuf, void* recvbuf, int *recvcounts, MPI_Datatype datatype,
                                     MPI_Op op, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                     struct mca_coll_base_module_2_1_0_t *module) {
  int peer, rank, maxr, p, r, res, count, offset, firstred;
  MPI_Aint ext;
  char *redbuf, *sbuf, inplace;
  NBC_Schedule *schedule;
  NBC_Handle *handle;
  ompi_coll_libnbc_request_t **coll_req = (ompi_coll_libnbc_request_t**) request;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;
  
  NBC_IN_PLACE(sendbuf, recvbuf, inplace);

  res = NBC_Init_handle(comm, coll_req, libnbc_module);
  if(res != NBC_OK) { printf("Error in NBC_Init_handle(%i)\n", res); return res; }
  res = MPI_Comm_size(comm, &p);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_size() (%i)\n", res); return res; }

  if(p==1) {
    if(!inplace) {
      /* single node not in_place: copy data to recvbuf */
      res = NBC_Copy(sendbuf, recvcounts[0], datatype, recvbuf, recvcounts[0], datatype, comm);
      if (NBC_OK != res) { printf("Error in NBC_Copy() (%i)\n", res); return res; }
    }
    /* manually complete the request */
    (*request)->req_status.MPI_ERROR = OMPI_SUCCESS;
    OPAL_THREAD_LOCK(&ompi_request_lock);
    ompi_request_complete(*request, true);
    OPAL_THREAD_UNLOCK(&ompi_request_lock);
    return NBC_OK;
  }

  handle = (*coll_req);

  res = MPI_Comm_rank(comm, &rank);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_rank() (%i)\n", res); return res; }
  res = MPI_Type_extent(datatype, &ext);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Type_extent() (%i)\n", res); return res; }
  
  schedule = (NBC_Schedule*)malloc(sizeof(NBC_Schedule));
  if (NULL == schedule) { printf("Error in malloc()\n"); return NBC_OOR; }

  res = NBC_Sched_create(schedule);
  if(res != NBC_OK) { printf("Error in NBC_Sched_create (%i)\n", res); return res; }

  maxr = (int)ceil((log((double)p)/LOG2));

  count = 0;
  for(r=0;r<p;r++) count += recvcounts[r];
  
  handle->tmpbuf = malloc(ext*count*2);
  if(handle->tmpbuf == NULL) { printf("Error in malloc()\n"); return NBC_OOR; }

  redbuf = ((char*)handle->tmpbuf)+(ext*count);

  firstred = 1;
  for(r=1; r<=maxr; r++) {
    if((rank % (1<<r)) == 0) {
      /* we have to receive this round */
      peer = rank + (1<<(r-1));
      if(peer<p) {
        res = NBC_Sched_recv(0, true, count, datatype, peer, schedule);
        if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
        /* we have to wait until we have the data */
        res = NBC_Sched_barrier(schedule);
        if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }
        if(firstred) {
          /* take reduce data from the sendbuf in the first round -> save copy */
          res = NBC_Sched_op(redbuf-(unsigned long)handle->tmpbuf, true, sendbuf, false, 0, true, count, datatype, op, schedule);
          firstred = 0;
        } else {
          /* perform the reduce in my local buffer */
          res = NBC_Sched_op(redbuf-(unsigned long)handle->tmpbuf, true, redbuf-(unsigned long)handle->tmpbuf, true, 0, true, count, datatype, op, schedule);
        }
        if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_op() (%i)\n", res); return res; }
        /* this cannot be done until handle->tmpbuf is unused :-( */
        res = NBC_Sched_barrier(schedule);
        if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }
      }
    } else {
      /* we have to send this round */
      peer = rank - (1<<(r-1));
      if(firstred) {
        /* we have to send the senbuf */
        res = NBC_Sched_send(sendbuf, false, count, datatype, peer, schedule);
      } else {
        /* we send an already reduced value from redbuf */
        res = NBC_Sched_send(redbuf-(unsigned long)handle->tmpbuf, true, count, datatype, peer, schedule);
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
    res = NBC_Sched_recv(recvbuf, false, recvcounts[rank], datatype, 0, schedule);
   if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
  }

  if(rank == 0) {
    offset = 0;
    for(r=1;r<p;r++) {
      offset += recvcounts[r-1];
      sbuf = ((char *)redbuf) + (offset*ext);
      /* root sends the right buffer to the right receiver */
      res = NBC_Sched_send(sbuf-(unsigned long)handle->tmpbuf, true, recvcounts[r], datatype, r, schedule);
      if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
    }
    res = NBC_Sched_copy(redbuf-(unsigned long)handle->tmpbuf, true, recvcounts[0], datatype, recvbuf, false, recvcounts[0], datatype, schedule);
    if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_copy() (%i)\n", res); return res; }
  }

  /*NBC_PRINT_SCHED(*schedule);*/
  
  res = NBC_Sched_commit(schedule);
  if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_commit() (%i)\n", res); return res; }
  
  res = NBC_Start(handle, schedule);
  if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Start() (%i)\n", res); return res; }
  
  /* tmpbuf is freed with the handle */
  return NBC_OK;
}

int ompi_coll_libnbc_ireduce_scatter_inter(void* sendbuf, void* recvbuf, int *recvcounts, MPI_Datatype datatype,
					   MPI_Op op, struct ompi_communicator_t *comm, ompi_request_t ** request,
					   struct mca_coll_base_module_2_1_0_t *module) {
  int peer, rank, r, res, count, rsize, offset;
  MPI_Aint ext;
  NBC_Schedule *schedule;
  NBC_Handle *handle;
  ompi_coll_libnbc_request_t **coll_req = (ompi_coll_libnbc_request_t**) request;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

  res = NBC_Init_handle(comm, coll_req, libnbc_module);
  if(res != NBC_OK) { printf("Error in NBC_Init_handle(%i)\n", res); return res; }
  handle = (*coll_req);
  res = MPI_Comm_rank(comm, &rank);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_rank() (%i)\n", res); return res; }
  res = MPI_Comm_remote_size(comm, &rsize);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_remote_size() (%i)\n", res); return res; }
  res = MPI_Type_extent(datatype, &ext);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Type_extent() (%i)\n", res); return res; }

  schedule = (NBC_Schedule*)malloc(sizeof(NBC_Schedule));
  if (NULL == schedule) { printf("Error in malloc()\n"); return NBC_OOR; }

  res = NBC_Sched_create(schedule);
  if(res != NBC_OK) { printf("Error in NBC_Sched_create (%i)\n", res); return res; }

  count = 0;
  for (r = 0 ; r < rsize ; ++r) count += recvcounts[r];

  handle->tmpbuf = malloc(2 * ext * count);
  if(handle->tmpbuf == NULL) { printf("Error in malloc()\n"); return NBC_OOR; }

  /* send my data to the remote root */
  res = NBC_Sched_send(sendbuf, false, count, datatype, 0, schedule);
  if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }

  if (0 == rank) {
    res = NBC_Sched_recv((void *) 0, true, count, datatype, 0, schedule);
    if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }

    res = NBC_Sched_barrier(schedule);
    if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }

    for (peer = 1 ; peer < rsize ; ++peer) {
      res = NBC_Sched_recv((void *)(ext * count), true, count, datatype, peer, schedule);
      if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }

      res = NBC_Sched_barrier(schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }

      res = NBC_Sched_op((void *) 0, true, (void *)(ext * count), true, (void *) 0, true, count, datatype, op, schedule);
      if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_op() (%i)\n", res); return res; }

      res = NBC_Sched_barrier(schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }

    }

    /* exchange data with remote root for scatter phase (we *could* use the local communicator to do the scatter) */
    res = NBC_Sched_recv((void *)(ext * count), true, count, datatype, 0, schedule);
    if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }

    res = NBC_Sched_send((void *) 0, true, count, datatype, 0, schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }

    res = NBC_Sched_barrier(schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }

    /* scatter */
    for (peer = 0, offset = ext * count ; peer < rsize ; ++peer) {
      res = NBC_Sched_send((void *)(uintptr_t) offset, true, recvcounts[peer], datatype, peer, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
      offset += recvcounts[peer] * ext;
    }
  }

  /* receive my block */
  res = NBC_Sched_recv(recvbuf, false, recvcounts[rank], datatype, 0, schedule);
  if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }

  /*NBC_PRINT_SCHED(*schedule);*/

  res = NBC_Sched_commit(schedule);
  if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_commit() (%i)\n", res); return res; }

  res = NBC_Start(handle, schedule);
  if (NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Start() (%i)\n", res); return res; }

  /* tmpbuf is freed with the handle */
  return NBC_OK;
}
