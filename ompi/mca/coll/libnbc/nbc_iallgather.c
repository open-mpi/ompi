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

#ifdef NBC_CACHE_SCHEDULE
/* tree comparison function for schedule cache */
int NBC_Allgather_args_compare(NBC_Allgather_args *a, NBC_Allgather_args *b, void *param) {

	if( (a->sendbuf == b->sendbuf) && 
      (a->sendcount == b->sendcount) && 
      (a->sendtype == b->sendtype) &&
      (a->recvbuf == b->recvbuf) &&
      (a->recvcount == b->recvcount) &&
      (a->recvtype == b->recvtype) ) {
    return  0;
  }
	if( a->sendbuf < b->sendbuf ) {	
    return -1;
	}
	return +1;
}
#endif

/* simple linear MPI_Iallgather
 * the algorithm uses p-1 rounds
 * each node sends the packet it received last round (or has in round 0) to it's right neighbor (modulo p)
 * each node receives from it's left (modulo p) neighbor */
int ompi_coll_libnbc_iallgather(void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount, 
                                MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                struct mca_coll_base_module_2_0_0_t *module)
{
  int rank, p, res, r;
  MPI_Aint rcvext;
  NBC_Schedule *schedule;
  char *rbuf, *sbuf, inplace;
#ifdef NBC_CACHE_SCHEDULE
  NBC_Allgather_args *args, *found, search;
#endif
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

  handle->tmpbuf = NULL;

  if (inplace) {
      sendtype = recvtype;
      sendcount = recvcount;
  } else {
    /* copy my data to receive buffer */
    rbuf = ((char *)recvbuf) + (rank*recvcount*rcvext);
    res = NBC_Copy(sendbuf, sendcount, sendtype, rbuf, recvcount, recvtype, comm);
    if (NBC_OK != res) { printf("Error in NBC_Copy() (%i)\n", res); return res; }
  }

#ifdef NBC_CACHE_SCHEDULE
  /* search schedule in communicator specific tree */
  search.sendbuf=sendbuf;
  search.sendcount=sendcount;
  search.sendtype=sendtype;
  search.recvbuf=recvbuf;
  search.recvcount=recvcount;
  search.recvtype=recvtype;
  found = (NBC_Allgather_args *)hb_tree_search((hb_tree*)handle->comminfo->NBC_Dict[NBC_ALLGATHER], &search);
  if(found == NULL) {
#endif
    schedule = (NBC_Schedule*)malloc(sizeof(NBC_Schedule));
    if (NULL == schedule) { printf("Error in malloc()\n"); return res; }

    res = NBC_Sched_create(schedule);
    if(NBC_OK != res) { printf("Error in NBC_Sched_create, (%i)\n", res); return res; }
    
    sbuf = ((char *)recvbuf) + (rank*recvcount*rcvext);
    /* do p-1 rounds */
    for(r=0;r<p;r++) {
      if(r != rank) {
        /* recv from rank r */
        rbuf = ((char *)recvbuf) + r*(recvcount*rcvext);
        res = NBC_Sched_recv(rbuf, false, recvcount, recvtype, r, schedule);
        if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
        /* send to rank r - not from the sendbuf to optimize MPI_IN_PLACE */
        res = NBC_Sched_send(sbuf, false, sendcount, sendtype, r, schedule);
        if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
      }
    }

    res = NBC_Sched_commit(schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_commit() (%i)\n", res); return res; }

#ifdef NBC_CACHE_SCHEDULE
    /* save schedule to tree */
    args = (NBC_Allgather_args*)malloc(sizeof(NBC_Allgather_args));
    args->sendbuf=sendbuf;
    args->sendcount=sendcount;
    args->sendtype=sendtype;
    args->recvbuf=recvbuf;
    args->recvcount=recvcount;
    args->recvtype=recvtype;
    args->schedule=schedule;
	  res = hb_tree_insert ((hb_tree*)handle->comminfo->NBC_Dict[NBC_ALLGATHER], args, args, 0);
    if(res != 0) printf("error in dict_insert() (%i)\n", res);
    /* increase number of elements for A2A */
    if(++handle->comminfo->NBC_Dict_size[NBC_ALLGATHER] > NBC_SCHED_DICT_UPPER) {
      NBC_SchedCache_dictwipe((hb_tree*)handle->comminfo->NBC_Dict[NBC_ALLGATHER], &handle->comminfo->NBC_Dict_size[NBC_ALLGATHER]);
    }
  } else {
    /* found schedule */
    schedule=found->schedule;
  }
#endif

  /*NBC_PRINT_SCHED(*schedule);*/
 
  res = NBC_Start(handle, schedule);
  if (NBC_OK != res) { printf("Error in NBC_Start() (%i)\n", res); return res; }
 
  return NBC_OK;
}

int ompi_coll_libnbc_iallgather_inter(void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
				      MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
				      struct mca_coll_base_module_2_0_0_t *module)
{
  int rank, res, r, rsize;
  MPI_Aint rcvext;
  NBC_Schedule *schedule;
  char *rbuf;
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

  handle->tmpbuf = NULL;

  schedule = (NBC_Schedule*)malloc(sizeof(NBC_Schedule));
  if (NULL == schedule) { printf("Error in malloc()\n"); return res; }

  res = NBC_Sched_create(schedule);
  if(NBC_OK != res) { printf("Error in NBC_Sched_create, (%i)\n", res); return res; }

  /* do rsize - 1 rounds */
  for(r = 0 ; r < rsize ; ++r) {
    /* recv from rank r */
    rbuf = ((char *)recvbuf) + r*(recvcount*rcvext);
    res = NBC_Sched_recv(rbuf, false, recvcount, recvtype, r, schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }

    /* send to rank r */
    res = NBC_Sched_send(sendbuf, false, sendcount, sendtype, r, schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
  }

  res = NBC_Sched_commit(schedule);
  if (NBC_OK != res) { printf("Error in NBC_Sched_commit() (%i)\n", res); return res; }

  /*NBC_PRINT_SCHED(*schedule);*/

  res = NBC_Start(handle, schedule);
  if (NBC_OK != res) { printf("Error in NBC_Start() (%i)\n", res); return res; }

  return NBC_OK;
}
