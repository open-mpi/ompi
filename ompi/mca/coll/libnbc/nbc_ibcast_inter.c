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

int ompi_coll_libnbc_ibcast_inter(void *buffer, int count, MPI_Datatype datatype, int root,
                                  struct ompi_communicator_t *comm, ompi_request_t ** request,
                                  struct mca_coll_base_module_2_0_0_t *module) {
  int rank, p, res, size, peer;
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
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_rank() (%i)\n", res); return res; }
  res = MPI_Type_size(datatype, &size);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Type_size() (%i)\n", res); return res; }
  
  handle->tmpbuf=NULL;

  schedule = (NBC_Schedule*)malloc(sizeof(NBC_Schedule));
  
  res = NBC_Sched_create(schedule);
  if(res != NBC_OK) { printf("Error in NBC_Sched_create, res = %i\n", res); return res; }

  if(root != MPI_PROC_NULL) {
    /* send to all others */
    if(root == MPI_ROOT) {
      int remsize;

      res = MPI_Comm_remote_size(comm, &remsize);
      if(MPI_SUCCESS != res) { printf("MPI_Comm_remote_size() failed\n"); return res; }

      for (peer=0;peer<remsize;peer++) {
        /* send msg to peer */
        res = NBC_Sched_send(buffer, false, count, datatype, peer, schedule);
        if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
      }
    } else {
      /* recv msg from root */
      res = NBC_Sched_recv(buffer, false, count, datatype, root, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
    }
  }
  
  res = NBC_Sched_commit(schedule);
  if (NBC_OK != res) { printf("Error in NBC_Sched_commit() (%i)\n", res); return res; }
  
  res = NBC_Start(handle, schedule);
  if (NBC_OK != res) { printf("Error in NBC_Start() (%i)\n", res); return res; }
  
  return NBC_OK;
}
