/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "nbc_internal.h"

/* Dissemination implementation of MPI_Ibarrier */
int ompi_coll_libnbc_ibarrier(struct ompi_communicator_t *comm, ompi_request_t ** request,
                              struct mca_coll_base_module_2_0_0_t *module)
{
  int round, rank, p, maxround, res, recvpeer, sendpeer;
  NBC_Schedule *schedule;
  NBC_Handle *handle;
  ompi_coll_libnbc_request_t **coll_req = (ompi_coll_libnbc_request_t**) request;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

  res = NBC_Init_handle(comm, coll_req, libnbc_module);
  if(res != NBC_OK) { printf("Error in NBC_Init_handle(%i)\n", res); return res; }
  handle = (*coll_req);

  res = MPI_Comm_rank(comm, &rank);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_rank() (%i)\n", res); return res; }
  res = MPI_Comm_size(comm, &p);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_size() (%i)\n", res); return res; }

  handle->tmpbuf=(void*)malloc(2*sizeof(char));

#ifdef NBC_CACHE_SCHEDULE
  /* there only one argument set per communicator -> hang it directly at
   * the tree-position, NBC_Dict_size[...] is 0 for not initialized and
   * 1 for initialized. NBC_Dict[...] is a pointer to the schedule in
   * this case */
  if(handle->comminfo->NBC_Dict_size[NBC_BARRIER] == 0) {
    /* we did not init it yet */
#endif
    schedule = (NBC_Schedule*)malloc(sizeof(NBC_Schedule));
    if (NULL == schedule) { printf("Error in malloc()\n"); return res; }

    round = -1;
    res = NBC_Sched_create(schedule);
    if(res != NBC_OK) { printf("Error in NBC_Sched_create (%i)\n", res); return res; }

    maxround = (int)ceil((log(p)/LOG2)-1);

    do {
      round++;
      sendpeer = (rank + (1<<round)) % p;
      /* add p because modulo does not work with negative values */
      recvpeer = ((rank - (1<<round))+p) % p;

      /* send msg to sendpeer */
      res = NBC_Sched_send((void*)0, true, 1, MPI_BYTE, sendpeer, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }

      /* recv msg from recvpeer */
      res = NBC_Sched_recv((void*)1, true, 1, MPI_BYTE, recvpeer, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }

      /* end communication round */
      if(round < maxround){
        res = NBC_Sched_barrier(schedule);
        if (NBC_OK != res) { printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }
      }
    } while (round < maxround);

    res = NBC_Sched_commit(schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_commit() (%i)\n", res); return res; }

#ifdef NBC_CACHE_SCHEDULE
    /* add it */
    handle->comminfo->NBC_Dict[NBC_BARRIER] = (hb_tree*)schedule;
    handle->comminfo->NBC_Dict_size[NBC_BARRIER] = 1;
  } else {
    /* we found it */
    schedule = (NBC_Schedule*)handle->comminfo->NBC_Dict[NBC_BARRIER];
  }
#endif

  res = NBC_Start(handle, schedule);
  if (NBC_OK != res) { printf("Error in NBC_Start() (%i)\n", res); return res; }

  return NBC_OK;
}

int ompi_coll_libnbc_ibarrier_inter(struct ompi_communicator_t *comm, ompi_request_t ** request,
                                    struct mca_coll_base_module_2_0_0_t *module)
{
  int rank, res, rsize, peer;
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

  handle->tmpbuf=(void*)malloc(2*sizeof(char));

  schedule = (NBC_Schedule*)malloc(sizeof(NBC_Schedule));
  if (NULL == schedule) { printf("Error in malloc()\n"); return res; }

  res = NBC_Sched_create(schedule);
  if(res != NBC_OK) { printf("Error in NBC_Sched_create (%i)\n", res); return res; }

  if (0 == rank) {
    for (peer = 1 ; peer < rsize ; ++peer) {
      res = NBC_Sched_recv (0, true, 1, MPI_BYTE, peer, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
    }
  }

  /* synchronize with the remote root */
  res = NBC_Sched_recv (0, true, 1, MPI_BYTE, 0, schedule);
  if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }

  res = NBC_Sched_send (0, true, 1, MPI_BYTE, 0, schedule);
  if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }

  if (0 == rank) {
    /* wait for the remote root */
    res = NBC_Sched_barrier(schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }

    /* inform remote peers that all local peers have entered the barrier */
    for (peer = 0 ; peer < rsize ; ++peer) {
      res = NBC_Sched_send (0, true, 1, MPI_BYTE, peer, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
    }
  }

  res = NBC_Sched_commit(schedule);
  if (NBC_OK != res) { printf("Error in NBC_Sched_commit() (%i)\n", res); return res; }

  res = NBC_Start(handle, schedule);
  if (NBC_OK != res) { printf("Error in NBC_Start() (%i)\n", res); return res; }

  return NBC_OK;
}
