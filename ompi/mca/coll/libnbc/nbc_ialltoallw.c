/*
 * Copyright (c) 2006 The Trustees of Indiana University and Indiana
 *                    University Research and Technology
 *                    Corporation.  All rights reserved.
 * Copyright (c) 2006 The Technical University of Chemnitz. All
 *                    rights reserved.
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "nbc_internal.h"

/* an alltoallw schedule can not be cached easily because the contents
 * ot the recvcounts array may change, so a comparison of the address
 * would not be sufficient ... we simply do not cache it */

/* simple linear Alltoallw */
int ompi_coll_libnbc_ialltoallw(void* sendbuf, int *sendcounts, int *sdispls,
                                MPI_Datatype sendtypes[], void* recvbuf, int *recvcounts, int *rdispls,
                                MPI_Datatype recvtypes[], struct ompi_communicator_t *comm, ompi_request_t ** request,
				struct mca_coll_base_module_2_0_0_t *module)
{
  int rank, p, res, i;
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
  res= MPI_Comm_size(comm, &p);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_size() (%i)\n", res); return res; }

  schedule = (NBC_Schedule*)malloc(sizeof(NBC_Schedule));
  if (NULL == schedule) { printf("Error in malloc() (%i)\n", res); return res; }

  handle->tmpbuf=NULL;

  res = NBC_Sched_create(schedule);
  if(res != NBC_OK) { printf("Error in NBC_Sched_create (%i)\n", res); return res; }

  /* copy data to receivbuffer */
  if((sendcounts[rank] != 0) && !inplace) {
    rbuf = ((char *) recvbuf) + rdispls[rank];
    sbuf = ((char *) sendbuf) + sdispls[rank];
    res = NBC_Copy(sbuf, sendcounts[rank], sendtypes[rank], rbuf, recvcounts[rank], recvtypes[rank], comm);
    if (NBC_OK != res) { printf("Error in NBC_Copy() (%i)\n", res); return res; }
  }

  for (i = 0; i < p; i++) {
    if (i == rank) { continue; }
    /* post all sends */
    if(sendcounts[i] != 0) {
      sbuf = ((char *) sendbuf) + sdispls[i];
      res = NBC_Sched_send(sbuf, false, sendcounts[i], sendtypes[i], i, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
    }
    /* post all receives */
    if(recvcounts[i] != 0) {
      rbuf = ((char *) recvbuf) + rdispls[i];
      res = NBC_Sched_recv(rbuf, false, recvcounts[i], recvtypes[i], i, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
    }
  }

  /*NBC_PRINT_SCHED(*schedule);*/

  res = NBC_Sched_commit(schedule);
  if (NBC_OK != res) { printf("Error in NBC_Sched_commit() (%i)\n", res); return res; }

  res = NBC_Start(handle, schedule);
  if (NBC_OK != res) { printf("Error in NBC_Start() (%i)\n", res); return res; }

  return NBC_OK;
}

/* simple linear Alltoallw */
int ompi_coll_libnbc_ialltoallw_inter (void* sendbuf, int *sendcounts, int *sdispls,
				       MPI_Datatype sendtypes[], void* recvbuf, int *recvcounts, int *rdispls,
				       MPI_Datatype recvtypes[], struct ompi_communicator_t *comm, ompi_request_t ** request,
				       struct mca_coll_base_module_2_0_0_t *module)
{
  int rank, res, i, rsize;
  NBC_Schedule *schedule;
  char *rbuf, *sbuf;
  NBC_Handle *handle;
  ompi_coll_libnbc_request_t **coll_req = (ompi_coll_libnbc_request_t**) request;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

  res = NBC_Init_handle(comm, coll_req, libnbc_module);
  if(res != NBC_OK) { printf("Error in NBC_Init_handle(%i)\n", res); return res; }
  handle = (*coll_req);
  res = MPI_Comm_rank(comm, &rank);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_rank() (%i)\n", res); return res; }

  MPI_Comm_remote_size (comm, &rsize);

  schedule = (NBC_Schedule*)malloc(sizeof(NBC_Schedule));
  if (NULL == schedule) { printf("Error in malloc() (%i)\n", res); return res; }

  handle->tmpbuf=NULL;

  res = NBC_Sched_create(schedule);
  if(res != NBC_OK) { printf("Error in NBC_Sched_create (%i)\n", res); return res; }

  for (i = 0; i < rsize; i++) {
    /* post all sends */
    if(sendcounts[i] != 0) {
      sbuf = ((char *) sendbuf) + sdispls[i];
      res = NBC_Sched_send(sbuf, false, sendcounts[i], sendtypes[i], i, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
    }
    /* post all receives */
    if(recvcounts[i] != 0) {
      rbuf = ((char *) recvbuf) + rdispls[i];
      res = NBC_Sched_recv(rbuf, false, recvcounts[i], recvtypes[i], i, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
    }
  }

  /*NBC_PRINT_SCHED(*schedule);*/

  res = NBC_Sched_commit(schedule);
  if (NBC_OK != res) { printf("Error in NBC_Sched_commit() (%i)\n", res); return res; }

  res = NBC_Start(handle, schedule);
  if (NBC_OK != res) { printf("Error in NBC_Start() (%i)\n", res); return res; }

  return NBC_OK;
}
