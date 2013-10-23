/*
 * Copyright (c) 2006 The Trustees of Indiana University and Indiana
 *                    University Research and Technology
 *                    Corporation.  All rights reserved.
 * Copyright (c) 2006 The Technical University of Chemnitz. All 
 *                    rights reserved.
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 * Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 *
 */
#include "nbc_internal.h"

/* an allgatherv schedule can not be cached easily because the contents
 * ot the recvcounts array may change, so a comparison of the address
 * would not be sufficient ... we simply do not cache it */

/* simple linear MPI_Iallgatherv
 * the algorithm uses p-1 rounds
 * first round:
 *   each node sends to it's left node (rank+1)%p sendcount elements 
 *   each node begins with it's right node (rank-11)%p and receives from it recvcounts[(rank+1)%p] elements
 * second round: 
 *   each node sends to node (rank+2)%p sendcount elements 
 *   each node receives from node (rank-2)%p recvcounts[(rank+2)%p] elements */
int ompi_coll_libnbc_iallgatherv(void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int *recvcounts, int *displs, 
                                 MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                 struct mca_coll_base_module_2_0_0_t *module)
{
  int rank, p, res, r, speer, rpeer;
  MPI_Aint rcvext;
  NBC_Schedule *schedule;
  char *rbuf, *sbuf, inplace;
  NBC_Handle *handle;
  ompi_coll_libnbc_request_t **coll_req = (ompi_coll_libnbc_request_t**) request;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;
  
  NBC_IN_PLACE(sendbuf, recvbuf, inplace);
  
  res = NBC_Init_handle(comm, coll_req, libnbc_module);
  if(res != NBC_OK) { printf("Error in NBC_Init_handle(%i)\n", res); return res; }
  handle = (*coll_req);
  res = MPI_Comm_rank(comm, &rank);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_rank() (%i)\n", res); return res; }
  res = MPI_Comm_size(comm, &p);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_size() (%i)\n", res); return res; }
  res = MPI_Type_extent(recvtype, &rcvext);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Type_extent() (%i)\n", res); return res; }

  schedule = (NBC_Schedule*)malloc(sizeof(NBC_Schedule));
  if (NULL == schedule) { printf("Error in malloc() (%i)\n", res); return res; }

  handle->tmpbuf=NULL;
 
  res = NBC_Sched_create(schedule);
  if(res != NBC_OK) { printf("Error in NBC_Sched_create, (%i)\n", res); return res; }
  
  if (inplace) {
      sendtype = recvtype;
      sendcount = recvcounts[rank];
  } else {
    /* copy my data to receive buffer */
    rbuf = ((char *)recvbuf) + (displs[rank]*rcvext);
    NBC_Copy(sendbuf, sendcount, sendtype, rbuf, recvcounts[rank], recvtype, comm);
    if (NBC_OK != res) { printf("Error in NBC_Copy() (%i)\n", res); return res; }
  }
  sbuf = ((char*) recvbuf) + (displs[rank]*rcvext);

  /* do p-1 rounds */
  for(r=1;r<p;r++) {
    speer = (rank+r)%p;
    rpeer = (rank-r+p)%p;
    rbuf = ((char *)recvbuf) + (displs[rpeer]*rcvext);
    
    res = NBC_Sched_recv(rbuf, false, recvcounts[rpeer], recvtype, rpeer, schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
    res = NBC_Sched_send(sbuf, false, sendcount, sendtype, speer, schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
  }

  res = NBC_Sched_commit(schedule);
  if (NBC_OK != res) { printf("Error in NBC_Sched_commit() (%i)\n", res); return res; }
 
  res = NBC_Start(handle, schedule);
  if (NBC_OK != res) { printf("Error in NBC_Start() (%i)\n", res); return res; }
 
  return NBC_OK;
}

int ompi_coll_libnbc_iallgatherv_inter(void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int *recvcounts, int *displs,
				       MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
				       struct mca_coll_base_module_2_0_0_t *module)
{
  int rank, res, r, rsize;
  MPI_Aint rcvext;
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
  res = MPI_Type_extent(recvtype, &rcvext);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Type_extent() (%i)\n", res); return res; }

  schedule = (NBC_Schedule*)malloc(sizeof(NBC_Schedule));
  if (NULL == schedule) { printf("Error in malloc() (%i)\n", res); return res; }

  handle->tmpbuf=NULL;

  res = NBC_Sched_create(schedule);
  if(res != NBC_OK) { printf("Error in NBC_Sched_create, (%i)\n", res); return res; }

  /* do rsize  rounds */
  for (r = 0 ; r < rsize ; ++r) {
    char *rbuf = ((char *)recvbuf) + (displs[r]*rcvext);

    if (recvcounts[r]) {
      res = NBC_Sched_recv(rbuf, false, recvcounts[r], recvtype, r, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
    }
  }

  if (sendcount) {
    for (r = 0 ; r < rsize ; ++r) {
      res = NBC_Sched_send(sendbuf, false, sendcount, sendtype, r, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
    }
  }

  res = NBC_Sched_commit(schedule);
  if (NBC_OK != res) { printf("Error in NBC_Sched_commit() (%i)\n", res); return res; }

  res = NBC_Start(handle, schedule);
  if (NBC_OK != res) { printf("Error in NBC_Start() (%i)\n", res); return res; }

  return NBC_OK;
}
