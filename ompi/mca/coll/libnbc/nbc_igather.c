#include "nbc.h"

#ifdef NBC_CACHE_SCHEDULE
/* tree comparison function for schedule cache */
int NBC_Gather_args_compare(NBC_Gather_args *a, NBC_Gather_args *b, void *param) {

	if( (a->sendbuf == b->sendbuf) && 
      (a->sendcount == b->sendcount) && 
      (a->sendtype == b->sendtype) &&
      (a->recvbuf == b->recvbuf) &&
      (a->recvcount == b->recvcount) &&
      (a->recvtype == b->recvtype) && 
      (a->root == b->root) ) {
    return  0;
  }
	if( a->sendbuf < b->sendbuf ) {	
    return -1;
	}
	return +1;
}
#endif

/* simple linear MPI_Igather */
int NBC_Igather(void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm, NBC_Handle* handle) {
  int rank, p, res, i;
  MPI_Aint rcvext;
  NBC_Schedule *schedule;
  char *rbuf, inplace;
#ifdef NBC_CACHE_SCHEDULE
  NBC_Gather_args *args, *found, search;
#endif
  
  NBC_IN_PLACE(sendbuf, recvbuf, inplace);
  
  res = NBC_Init_handle(handle, comm);
  if(res != NBC_OK) { printf("Error in NBC_Init_handle(%i)\n", res); return res; }
  res = MPI_Comm_rank(comm, &rank);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_rank() (%i)\n", res); return res; }
  res = MPI_Comm_size(comm, &p);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_rank() (%i)\n", res); return res; }
  res = MPI_Type_extent(recvtype, &rcvext);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Type_extent() (%i)\n", res); return res; }

  handle->tmpbuf = NULL;
  
  if((rank == root) && (!inplace)) {
    rbuf = ((char *)recvbuf) + (rank*recvcount*rcvext);
    /* if I am the root - just copy the message (only without MPI_IN_PLACE) */
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
  search.root=root;
  found = hb_tree_search(handle->comminfo->NBC_Dict[NBC_GATHER], &search);
  if(found == NULL) {
#endif
    schedule = malloc(sizeof(NBC_Schedule));
    if (NULL == schedule) { printf("Error in malloc() (%i)\n", res); return res; }

    res = NBC_Sched_create(schedule);
    if(res != NBC_OK) { printf("Error in NBC_Sched_create (%i)\n", res); return res; }

    /* send to root */
    if(rank != root) {
      /* send msg to root */
      res = NBC_Sched_send(sendbuf, false, sendcount, sendtype, root, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
    } else {
      for(i=0;i<p;i++) {
        rbuf = ((char *)recvbuf) + (i*recvcount*rcvext);
        if(i != root) {
          /* root receives message to the right buffer */
          res = NBC_Sched_recv(rbuf, false, recvcount, recvtype, i, schedule);
          if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
        }
      }
    }
   
    res = NBC_Sched_commit(schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_commit() (%i)\n", res); return res; }
 
#ifdef NBC_CACHE_SCHEDULE
    /* save schedule to tree */
    args = malloc(sizeof(NBC_Gather_args));
    args->sendbuf=sendbuf;
    args->sendcount=sendcount;
    args->sendtype=sendtype;
    args->recvbuf=recvbuf;
    args->recvcount=recvcount;
    args->recvtype=recvtype;
    args->root=root;
    args->schedule=schedule;
	  res = hb_tree_insert (handle->comminfo->NBC_Dict[NBC_GATHER], args, args, 0);
    if(res != 0) printf("error in dict_insert() (%i)\n", res);
    /* increase number of elements for A2A */
    if(++handle->comminfo->NBC_Dict_size[NBC_GATHER] > SCHED_DICT_UPPER) {
      NBC_SchedCache_dictwipe(handle->comminfo->NBC_Dict[NBC_GATHER], &handle->comminfo->NBC_Dict_size[NBC_GATHER]);
    }
  } else {
    /* found schedule */
    schedule=found->schedule;
  }
#endif

  res = NBC_Start(handle, schedule);
  if (NBC_OK != res) { printf("Error in NBC_Start() (%i)\n", res); return res; }
 
  return NBC_OK;
}
