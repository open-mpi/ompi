/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "nbc_internal.h"

static inline int a2aw_sched_linear(int rank, int p, NBC_Schedule *schedule,
                                    const void *sendbuf, const int *sendcounts, const int *sdispls,
                                    struct ompi_datatype_t * const * sendtypes,
                                    void *recvbuf, const int *recvcounts, const int *rdispls,
                                    struct ompi_datatype_t * const * recvtypes);

static inline int a2aw_sched_pairwise(int rank, int p, NBC_Schedule *schedule,
                                      const void *sendbuf, const int *sendcounts, const int *sdispls,
                                      struct ompi_datatype_t * const * sendtypes,
                                      void *recvbuf, const int *recvcounts, const int *rdispls,
                                      struct ompi_datatype_t * const * recvtypes);

static inline int a2aw_sched_inplace(int rank, int p, NBC_Schedule *schedule,
                                    void *buf, const int *counts, const int *displs,
                                    struct ompi_datatype_t * const * types);

/* an alltoallw schedule can not be cached easily because the contents
 * ot the recvcounts array may change, so a comparison of the address
 * would not be sufficient ... we simply do not cache it */

/* simple linear Alltoallw */
int ompi_coll_libnbc_ialltoallw(void* sendbuf, int *sendcounts, int *sdispls,
                                MPI_Datatype sendtypes[], void* recvbuf, int *recvcounts, int *rdispls,
                                MPI_Datatype recvtypes[], struct ompi_communicator_t *comm, ompi_request_t ** request,
				struct mca_coll_base_module_2_0_0_t *module)
{
  int rank, p, res;
  NBC_Schedule *schedule;
  char *rbuf, *sbuf, inplace;
  ptrdiff_t span=0;
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
  if (inplace) {
    ptrdiff_t lgap, lspan;
    for (int i = 0; i < p; i++) {
      lspan = opal_datatype_span(&recvtypes[i]->super, recvcounts[i], &lgap);
      if (lspan > span) {
        span = lspan;
      }
    }
    handle->tmpbuf = malloc(span);
    if (OPAL_UNLIKELY(NULL == handle->tmpbuf)) { printf("Error in malloc()\n"); return NBC_OOR; }
    sendcounts = recvcounts;
    sdispls = rdispls;
    sendtypes = recvtypes;
  } else if (sendcounts[rank] != 0) {
    rbuf = (char *) recvbuf + rdispls[rank];
    sbuf = (char *) sendbuf + sdispls[rank];
    res = NBC_Copy(sbuf, sendcounts[rank], sendtypes[rank], rbuf, recvcounts[rank], recvtypes[rank], comm);
    if (NBC_OK != res) { printf("Error in NBC_Copy() (%i)\n", res); return res; }
  }

  if (inplace) {
    res = a2aw_sched_inplace(rank, p, schedule, recvbuf,
                                 recvcounts, rdispls, recvtypes);
  } else {
    res = a2aw_sched_linear(rank, p, schedule,
                            sendbuf, sendcounts, sdispls, sendtypes,
                            recvbuf, recvcounts, rdispls, recvtypes);
  }
  if (OPAL_UNLIKELY(NBC_OK != res)) { return res; }

  res = NBC_Sched_commit (schedule);
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

static inline int a2aw_sched_linear(int rank, int p, NBC_Schedule *schedule,
                                    const void *sendbuf, const int *sendcounts, const int *sdispls,
                                    struct ompi_datatype_t * const * sendtypes,
                                    void *recvbuf, const int *recvcounts, const int *rdispls,
                                    struct ompi_datatype_t * const * recvtypes) {
  int res;

  for (int i = 0; i < p; i++) {
    if (i == rank) {
      continue;
    }

    /* post send */
    if (sendcounts[i] != 0) {
      char *sbuf = (char *) sendbuf + sdispls[i];
      res = NBC_Sched_send (sbuf, false, sendcounts[i], sendtypes[i], i, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
    }
    /* post receive */
    if (recvcounts[i] != 0) {
      char *rbuf = (char *) recvbuf + rdispls[i];
      res = NBC_Sched_recv (rbuf, false, recvcounts[i], recvtypes[i], i, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
    }
  }

  return NBC_OK;
}

static inline int a2aw_sched_pairwise(int rank, int p, NBC_Schedule *schedule,
                                      const void *sendbuf, const int *sendcounts, const int *sdispls,
                                      struct ompi_datatype_t * const * sendtypes,
                                      void *recvbuf, const int *recvcounts, const int *rdispls,
                                      struct ompi_datatype_t * const * recvtypes) {
  int res;

  for (int i = 1; i < p; i++) {
    int sndpeer = (rank + i) % p;
    int rcvpeer = (rank + p - i) % p;

    /* post send */
    if (sendcounts[sndpeer] != 0) {
      char *sbuf = (char *) sendbuf + sdispls[sndpeer];
      res = NBC_Sched_send (sbuf, false, sendcounts[sndpeer], sendtypes[sndpeer], sndpeer, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
    }
    /* post receive */
    if (recvcounts[rcvpeer] != 0) {
      char *rbuf = (char *) recvbuf + rdispls[rcvpeer];
      res = NBC_Sched_recv (rbuf, false, recvcounts[rcvpeer], recvtypes[rcvpeer], rcvpeer, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
      res = NBC_Sched_barrier(schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_barr() (%i)\n", res); return res; }
    }
  }

  return NBC_OK;
}

static inline int a2aw_sched_inplace(int rank, int p, NBC_Schedule *schedule,
                                     void *buf, const int *counts, const int *displs,
                                     struct ompi_datatype_t * const * types) {
  ptrdiff_t gap;
  int res;

  for (int i = 1; i < (p+1)/2; i++) {
    int speer = (rank + i) % p;
    int rpeer = (rank + p - i) % p;
    char *sbuf = (char *) buf + displs[speer];
    char *rbuf = (char *) buf + displs[rpeer];

    if (0 != counts[rpeer]) {
      (void)opal_datatype_span(&types[rpeer]->super, counts[rpeer], &gap);
      res = NBC_Sched_copy (rbuf, false, counts[rpeer], types[rpeer], (void *)(-gap), true, counts[rpeer], types[rpeer], schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_copy() (%i)\n", res); return res; }
      res = NBC_Sched_barrier(schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_barr() (%i)\n", res); return res; }
    }
    if (0 != counts[speer]) {
      res = NBC_Sched_send (sbuf, false , counts[speer], types[speer], speer, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
    }
    if (0 != counts[rpeer]) {
      res = NBC_Sched_recv (rbuf, false , counts[rpeer], types[rpeer], rpeer, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
      res = NBC_Sched_barrier(schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_barr() (%i)\n", res); return res; }
    }

    if (0 != counts[rpeer]) {
      res = NBC_Sched_send ((void *)(-gap), true, counts[rpeer], types[rpeer], rpeer, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
    }
    if (0 != counts[speer]) {
      res = NBC_Sched_recv (sbuf, false, counts[speer], types[speer], speer, schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
      res = NBC_Sched_barrier(schedule);
      if (NBC_OK != res) { printf("Error in NBC_Sched_barr() (%i)\n", res); return res; }
    }
  }
  if (0 == (p%2)) {
    int peer = (rank + p/2) % p;

    char *tbuf = (char *) buf + displs[peer];
    (void)opal_datatype_span(&types[peer]->super, counts[peer], &gap);
    res = NBC_Sched_copy (tbuf, false, counts[peer], types[peer], (void *)(-gap), true, counts[peer], types[peer], schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_copy() (%i)\n", res); return res; }
    res = NBC_Sched_barrier(schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_barr() (%i)\n", res); return res; }
    res = NBC_Sched_send ((void *)(-gap), true , counts[peer], types[peer], peer, schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
    res = NBC_Sched_recv (tbuf, false , counts[peer], types[peer], peer, schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
    res = NBC_Sched_barrier(schedule);
    if (NBC_OK != res) { printf("Error in NBC_Sched_barr() (%i)\n", res); return res; }
  }

  return NBC_OK;
}
