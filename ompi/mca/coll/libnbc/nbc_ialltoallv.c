#include "nbc.h"

/* an alltoallv schedule can not be cached easily because the contents
 * ot the recvcounts array may change, so a comparison of the address
 * would not be sufficient ... we simply do not cache it */

/* simple linear Alltoallv */
int NBC_Ialltoallv(void* sendbuf, int *sendcounts, int *sdispls,
    MPI_Datatype sendtype, void* recvbuf, int *recvcounts, int *rdispls,
    MPI_Datatype recvtype, MPI_Comm comm, NBC_Handle* handle) {
  
  int rank, p, res, i;
  MPI_Aint sndext, rcvext;
  NBC_Schedule *schedule;
  char *rbuf, *sbuf, inplace;
  
  NBC_IN_PLACE(sendbuf, recvbuf, inplace);
  
  res = NBC_Init_handle(handle, comm);
  if(res != NBC_OK) { printf("Error in NBC_Init_handle(%i)\n", res); return res; }
  res = MPI_Comm_rank(comm, &rank);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_rank() (%i)\n", res); return res; }
  res= MPI_Comm_size(comm, &p);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_size() (%i)\n", res); return res; }
  res = MPI_Type_extent(sendtype, &sndext);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Type_extent() (%i)\n", res); return res; }
  res = MPI_Type_extent(recvtype, &rcvext);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Type_extent() (%i)\n", res); return res; }

  schedule = malloc(sizeof(NBC_Schedule));
  if (NULL == schedule) { printf("Error in malloc() (%i)\n", res); return res; }

  handle->tmpbuf=NULL;
  
  res = NBC_Sched_create(schedule);
  if(res != NBC_OK) { printf("Error in NBC_Sched_create (%i)\n", res); return res; }

  /* copy data to receivbuffer */
  if((sendcounts[rank] != 0) && !inplace) {
    rbuf = ((char *) recvbuf) + (rdispls[rank] * rcvext);
    sbuf = ((char *) sendbuf) + (sdispls[rank] * sndext);
    res = NBC_Copy(sbuf, sendcounts[rank], sendtype, rbuf, recvcounts[rank], recvtype, comm);
    if (NBC_OK != res) { printf("Error in NBC_Copy() (%i)\n", res); return res; }
  }

  for (i = 0; i < p; i++) {
    if (i == rank) { continue; }
    /* post all sends */
    sbuf = ((char *) sendbuf) + (sdispls[i] * sndext);
    res = NBC_Sched_send(sbuf, false, sendcounts[i], sendtype, i, schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
    /* post all receives */
    rbuf = ((char *) recvbuf) + (rdispls[i] * rcvext);
    res = NBC_Sched_recv(rbuf, false, recvcounts[i], recvtype, i, schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
  }

  /*NBC_PRINT_SCHED(*schedule);*/

  res = NBC_Sched_commit(schedule);
  if (NBC_OK != res) { printf("Error in NBC_Sched_commit() (%i)\n", res); return res; }

  res = NBC_Start(handle, schedule);
  if (NBC_OK != res) { printf("Error in NBC_Start() (%i)\n", res); return res; }
  
  return NBC_OK;
}
