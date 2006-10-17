#include "nbc.h"

/* an gatherv schedule can not be cached easily because the contents
 * ot the recvcounts array may change, so a comparison of the address
 * would not be sufficient ... we simply do not cache it */

/* simple linear MPI_Igatherv */
int NBC_Igatherv(void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int *recvcounts, int *displs, MPI_Datatype recvtype, int root, MPI_Comm comm, NBC_Handle* handle) {
  int rank, p, res, i;
  MPI_Aint rcvext;
  NBC_Schedule *schedule;
  char *rbuf, inplace;
  
  NBC_IN_PLACE(sendbuf, recvbuf, inplace);
  
  res = NBC_Init_handle(handle, comm);
  if(res != NBC_OK) { printf("Error in NBC_Init_handle(%i)\n", res); return res; }
  res = MPI_Comm_rank(comm, &rank);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_rank() (%i)\n", res); return res; }
  res = MPI_Comm_size(comm, &p);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_rank() (%i)\n", res); return res; }
  res = MPI_Type_extent(recvtype, &rcvext);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Type_extent() (%i)\n", res); return res; }

  schedule = malloc(sizeof(NBC_Schedule));
  if (NULL == schedule) { printf("Error in malloc() (%i)\n", res); return res; }

  handle->tmpbuf=NULL;
 
  res = NBC_Sched_create(schedule);
  if(res != NBC_OK) { printf("Error in NBC_Sched_create (%i)\n", res); return res; }

  /* send to root */
  if(rank != root) {
    /* send msg to root */
    res = NBC_Sched_send(sendbuf, false, sendcount, sendtype, root, schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
  } else {
    for(i=0;i<p;i++) {
      rbuf = ((char *)recvbuf) + (displs[i]*rcvext);
      if(i == root) {
        if(!inplace) {
          /* if I am the root - just copy the message */
          res = NBC_Copy(sendbuf, sendcount, sendtype, rbuf, recvcounts[i], recvtype, comm);
          if (NBC_OK != res) { printf("Error in NBC_Copy() (%i)\n", res); return res; }
        }
      } else {
        /* root receives message to the right buffer */
        res = NBC_Sched_recv(rbuf, false, recvcounts[i], recvtype, i, schedule);
        if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
      }
    }
  }
 
  res = NBC_Sched_commit(schedule);
  if (NBC_OK != res) { printf("Error in NBC_Sched_commit() (%i)\n", res); return res; }
 
  res = NBC_Start(handle, schedule);
  if (NBC_OK != res) { printf("Error in NBC_Start() (%i)\n", res); return res; }
 
  return NBC_OK;
}
