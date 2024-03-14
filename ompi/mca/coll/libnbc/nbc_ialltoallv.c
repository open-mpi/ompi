/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2014-2018 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2015-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
 * Copyright (c) 2021      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "nbc_internal.h"

static inline int a2av_sched_linear(int rank, int p, NBC_Schedule *schedule,
                                    const void *sendbuf, const int *sendcounts,
                                    const int *sdispls, MPI_Aint sndext, MPI_Datatype sendtype, const size_t sdtype_size,
                                    void *recvbuf, const int *recvcounts,
                                    const int *rdispls, MPI_Aint rcvext, MPI_Datatype recvtype, const size_t rdtype_size);

static inline int a2av_sched_pairwise(int rank, int p, NBC_Schedule *schedule,
                                      const void *sendbuf, const int *sendcounts, const int *sdispls,
                                      MPI_Aint sndext, MPI_Datatype sendtype, const size_t sdtype_size,
                                      void *recvbuf, const int *recvcounts, const int *rdispls,
                                      MPI_Aint rcvext, MPI_Datatype recvtype, const size_t rdtype_size);

static inline int a2av_sched_inplace(int rank, int p, NBC_Schedule *schedule,
                                    void *buf, const int *counts, const int *displs,
                                    MPI_Aint ext, MPI_Datatype type, const size_t dtype_size, ptrdiff_t gap);

/* an alltoallv schedule can not be cached easily because the contents
 * of the recvcounts array may change, so a comparison of the address
 * would not be sufficient ... we simply do not cache it */

/* simple linear Alltoallv */
static int nbc_alltoallv_init(const void* sendbuf, const int *sendcounts, const int *sdispls,
                              MPI_Datatype sendtype, void* recvbuf, const int *recvcounts, const int *rdispls,
                              MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                              mca_coll_base_module_t *module, bool persistent)
{
  int rank, p, res;
  size_t sdtype_size, rdtype_size;
  MPI_Aint sndext, rcvext;
  NBC_Schedule *schedule;
  char *rbuf, *sbuf, inplace;
  ptrdiff_t gap = 0, span;
  void * tmpbuf = NULL;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

  NBC_IN_PLACE(sendbuf, recvbuf, inplace);

  rank = ompi_comm_rank (comm);
  p = ompi_comm_size (comm);

  ompi_datatype_type_size(recvtype, &rdtype_size);
  res = ompi_datatype_type_extent (recvtype, &rcvext);
  if (MPI_SUCCESS != res) {
    NBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
    return res;
  }

  /* copy data to receivbuffer */
  if (inplace) {
    int count = 0;
    for (int i = 0; i < p; i++) {
      if (recvcounts[i] > count) {
        count = recvcounts[i];
      }
    }
    span = opal_datatype_span(&recvtype->super, count, &gap);
    /**
     * If this process has no data to send or receive it can bail out early,
     * but it needs to increase the nonblocking tag to stay in sync with the
     * rest of the processes.
     */
    if (OPAL_UNLIKELY(0 == span)) {
      ompi_coll_base_nbc_reserve_tags(comm, 1);
      return nbc_get_noop_request(persistent, request);
    }
    tmpbuf = malloc(span);
    if (OPAL_UNLIKELY(NULL == tmpbuf)) {
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    sendcounts = recvcounts;
    sdispls = rdispls;
    sndext = rcvext;
    sdtype_size = rdtype_size;
  } else {
    ompi_datatype_type_size(sendtype, &sdtype_size);
    res = ompi_datatype_type_extent (sendtype, &sndext);
    if (MPI_SUCCESS != res) {
      NBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
      return res;
    }
  }

  schedule = OBJ_NEW(NBC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    free(tmpbuf);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  if (!inplace && 0 < sendcounts[rank] && 0 < sdtype_size) {
    rbuf = (char *) recvbuf + rdispls[rank] * rcvext;
    sbuf = (char *) sendbuf + sdispls[rank] * sndext;
    res = NBC_Sched_copy (sbuf, false, sendcounts[rank], sendtype,
                          rbuf, false, recvcounts[rank], recvtype, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      OBJ_RELEASE(schedule);
      return res;
    }
  }

  if (inplace) {
    res = a2av_sched_inplace(rank, p, schedule, recvbuf, recvcounts, rdispls, rcvext, recvtype,
                             rdtype_size, gap);
  } else {
    res = a2av_sched_linear(rank, p, schedule,
                            sendbuf, sendcounts, sdispls, sndext, sendtype, sdtype_size,
                            recvbuf, recvcounts, rdispls, rcvext, recvtype, rdtype_size);
  }
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    free(tmpbuf);
    return res;
  }

  res = NBC_Sched_commit (schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    free(tmpbuf);
    return res;
  }

  res = NBC_Schedule_request(schedule, comm, libnbc_module, persistent, request, tmpbuf);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    free(tmpbuf);
    return res;
  }

  return OMPI_SUCCESS;
}

int ompi_coll_libnbc_ialltoallv(const void* sendbuf, const int *sendcounts, const int *sdispls,
                                MPI_Datatype sendtype, void* recvbuf, const int *recvcounts, const int *rdispls,
                                MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                mca_coll_base_module_t *module) {
    int res = nbc_alltoallv_init(sendbuf, sendcounts, sdispls, sendtype,
                                 recvbuf, recvcounts, rdispls, recvtype,
                                 comm, request, module, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
    }
  
    res = NBC_Start(*(ompi_coll_libnbc_request_t **)request);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        NBC_Return_handle (*(ompi_coll_libnbc_request_t **)request);
        *request = &ompi_request_null.request;
        return res;
    }

    return OMPI_SUCCESS;
}

/* simple linear Alltoallv */
static int nbc_alltoallv_inter_init (const void* sendbuf, const int *sendcounts, const int *sdispls,
                                     MPI_Datatype sendtype, void* recvbuf, const int *recvcounts, const int *rdispls,
                                     MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                     mca_coll_base_module_t *module, bool persistent)
{
  int res, rsize;
  size_t sdtype_size, rdtype_size;
  MPI_Aint sndext, rcvext;
  NBC_Schedule *schedule;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

  ompi_datatype_type_size(sendtype, &sdtype_size);
  ompi_datatype_type_size(recvtype, &rdtype_size);

  res = ompi_datatype_type_extent(sendtype, &sndext);
  if (MPI_SUCCESS != res) {
    NBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
    return res;
  }

  res = ompi_datatype_type_extent(recvtype, &rcvext);
  if (MPI_SUCCESS != res) {
    NBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
    return res;
  }

  rsize = ompi_comm_remote_size (comm);

  schedule = OBJ_NEW(NBC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  for (int i = 0; i < rsize; i++) {
    /* post all sends */
    if (0 < sendcounts[i] && 0 < sdtype_size) {
      char *sbuf = (char *) sendbuf + sdispls[i] * sndext;
      res = NBC_Sched_send (sbuf, false, sendcounts[i], sendtype, i, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        OBJ_RELEASE(schedule);
        return res;
      }
    }
    /* post all receives */
    if (0 < recvcounts[i] && 0 < rdtype_size) {
      char *rbuf = (char *) recvbuf + rdispls[i] * rcvext;
      res = NBC_Sched_recv (rbuf, false, recvcounts[i], recvtype, i, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        OBJ_RELEASE(schedule);
        return res;
      }
    }
  }

  res = NBC_Sched_commit(schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    return res;
  }

  res = NBC_Schedule_request(schedule, comm, libnbc_module, persistent, request, NULL);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    return res;
  }

  return OMPI_SUCCESS;
}

int ompi_coll_libnbc_ialltoallv_inter (const void* sendbuf, const int *sendcounts, const int *sdispls,
				       MPI_Datatype sendtype, void* recvbuf, const int *recvcounts, const int *rdispls,
				       MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
				       mca_coll_base_module_t *module) {
    int res = nbc_alltoallv_inter_init(sendbuf, sendcounts, sdispls, sendtype,
                                       recvbuf, recvcounts, rdispls, recvtype,
                                       comm, request, module, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
    }
  
    res = NBC_Start(*(ompi_coll_libnbc_request_t **)request);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        NBC_Return_handle (*(ompi_coll_libnbc_request_t **)request);
        *request = &ompi_request_null.request;
        return res;
    }

    return OMPI_SUCCESS;
}

__opal_attribute_unused__
static inline int a2av_sched_linear(int rank, int p, NBC_Schedule *schedule,
                                    const void *sendbuf, const int *sendcounts, const int *sdispls,
                                    MPI_Aint sndext, MPI_Datatype sendtype, const size_t sdtype_size,
                                    void *recvbuf, const int *recvcounts, const int *rdispls,
                                    MPI_Aint rcvext, MPI_Datatype recvtype, const size_t rdtype_size) {
  int res;

  for (int i = 0 ; i < p ; ++i) {
    if (i == rank) {
      continue;
    }

    /* post send */
    if (0 < sendcounts[i] && 0 < sdtype_size) {
      char *sbuf = ((char *) sendbuf) + (sdispls[i] * sndext);
      res = NBC_Sched_send(sbuf, false, sendcounts[i], sendtype, i, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }

    /* post receive */
    if (0 < recvcounts[i] && 0 < rdtype_size) {
      char *rbuf = ((char *) recvbuf) + (rdispls[i] * rcvext);
      res = NBC_Sched_recv(rbuf, false, recvcounts[i], recvtype, i, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
  }

  return OMPI_SUCCESS;
}

__opal_attribute_unused__
static inline int a2av_sched_pairwise(int rank, int p, NBC_Schedule *schedule,
                                      const void *sendbuf, const int *sendcounts, const int *sdispls,
                                      MPI_Aint sndext, MPI_Datatype sendtype, const size_t sdtype_size,
                                      void *recvbuf, const int *recvcounts, const int *rdispls,
                                      MPI_Aint rcvext, MPI_Datatype recvtype, const size_t rdtype_size) {
  int res;

  for (int i = 1 ; i < p ; ++i) {
    int sndpeer = (rank + i) % p;
    int rcvpeer = (rank + p - i) %p;

    /* post send */
    if (0 < sendcounts[sndpeer] && 0 < sdtype_size) {
      char *sbuf = ((char *) sendbuf) + (sdispls[sndpeer] * sndext);
      res = NBC_Sched_send(sbuf, false, sendcounts[sndpeer], sendtype, sndpeer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }

    /* post receive */
    if (0 < recvcounts[rcvpeer] && 0 < rdtype_size) {
      char *rbuf = ((char *) recvbuf) + (rdispls[rcvpeer] * rcvext);
      res = NBC_Sched_recv(rbuf, false, recvcounts[rcvpeer], recvtype, rcvpeer, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
  }

  return OMPI_SUCCESS;
}

static inline int a2av_sched_inplace(int rank, int p, NBC_Schedule *schedule,
                                    void *buf, const int *counts, const int *displs,
                                    MPI_Aint ext, MPI_Datatype type, const size_t dtype_size, ptrdiff_t gap) {
  int res;

  for (int i = 1; i < (p+1)/2; i++) {
    int speer = (rank + i) % p;
    int rpeer = (rank + p - i) % p;
    char *sbuf = (char *) buf + displs[speer] * ext;
    char *rbuf = (char *) buf + displs[rpeer] * ext;

    if (0 == dtype_size) {
      /* Nothing to exchange */
      return OMPI_SUCCESS;
    }

    if (0 < counts[rpeer]) {
      res = NBC_Sched_copy (rbuf, false, counts[rpeer], type,
                            (void *)(-gap), true, counts[rpeer], type,
                            schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
    if (0 < counts[speer]) {
      res = NBC_Sched_send (sbuf, false , counts[speer], type, speer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
    if (0 < counts[rpeer]) {
      res = NBC_Sched_recv (rbuf, false , counts[rpeer], type, rpeer, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }

    if (0 < counts[rpeer]) {
      res = NBC_Sched_send ((void *)(-gap), true, counts[rpeer], type, rpeer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
    if (0 < counts[speer]) {
      res = NBC_Sched_recv (sbuf, false, counts[speer], type, speer, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
  }
  if (0 == (p%2)) {
    int peer = (rank + p/2) % p;
    char *tbuf = (char *) buf + displs[peer] * ext;

    if (0 < counts[peer]) {
        res = NBC_Sched_copy(tbuf, false, counts[peer], type, (void *) (-gap), true, counts[peer],
                             type, schedule, true);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
            return res;
        }
    }

    if (0 < counts[peer]) {
      res = NBC_Sched_send ((void *)(-gap), true , counts[peer], type, peer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
      res = NBC_Sched_recv (tbuf, false , counts[peer], type, peer, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
  }

  return OMPI_SUCCESS;
}

int ompi_coll_libnbc_alltoallv_init(const void* sendbuf, const int *sendcounts, const int *sdispls,
                                    MPI_Datatype sendtype, void* recvbuf, const int *recvcounts, const int *rdispls,
                                    MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                    mca_coll_base_module_t *module) {
    int res = nbc_alltoallv_init(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype,
                                 comm, request, module, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
    }

    return OMPI_SUCCESS;
}

int ompi_coll_libnbc_alltoallv_inter_init(const void* sendbuf, const int *sendcounts, const int *sdispls,
                                          MPI_Datatype sendtype, void* recvbuf, const int *recvcounts, const int *rdispls,
                                          MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                          mca_coll_base_module_t *module) {
    int res = nbc_alltoallv_inter_init(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype,
                                       comm, request, module, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
    }

    return OMPI_SUCCESS;
}
