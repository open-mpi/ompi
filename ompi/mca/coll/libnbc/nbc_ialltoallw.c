/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
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
int ompi_coll_libnbc_ialltoallw(const void* sendbuf, const int *sendcounts, const int *sdispls,
                                struct ompi_datatype_t * const *sendtypes, void* recvbuf, const int *recvcounts, const int *rdispls,
                                struct ompi_datatype_t * const *recvtypes, struct ompi_communicator_t *comm, ompi_request_t ** request,
				struct mca_coll_base_module_2_1_0_t *module)
{
  int rank, p, res;
  NBC_Schedule *schedule;
  char *rbuf, *sbuf, inplace;
  ptrdiff_t span=0;
  NBC_Handle *handle;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

  NBC_IN_PLACE(sendbuf, recvbuf, inplace);

  rank = ompi_comm_rank (comm);
  p = ompi_comm_size (comm);

  res = NBC_Init_handle (comm, &handle, libnbc_module);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    return res;
  }

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
    if (OPAL_UNLIKELY(NULL == handle->tmpbuf)) {
      NBC_Return_handle (handle);
      return OMPI_ERR_OUT_OF_RESOURCE;
    }
    sendcounts = recvcounts;
    sdispls = rdispls;
    sendtypes = recvtypes;
  } else if (sendcounts[rank] != 0) {
    rbuf = (char *) recvbuf + rdispls[rank];
    sbuf = (char *) sendbuf + sdispls[rank];
    res = NBC_Copy(sbuf, sendcounts[rank], sendtypes[rank], rbuf, recvcounts[rank], recvtypes[rank], comm);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
  }

  schedule = OBJ_NEW(NBC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    NBC_Return_handle (handle);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  if (inplace) {
    res = a2aw_sched_inplace(rank, p, schedule, recvbuf,
                                 recvcounts, rdispls, recvtypes);
  } else {
    res = a2aw_sched_linear(rank, p, schedule,
                            sendbuf, sendcounts, sdispls, sendtypes,
                            recvbuf, recvcounts, rdispls, recvtypes);
  }
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    NBC_Return_handle (handle);
    OBJ_RELEASE(schedule);
    return res;
  }

  res = NBC_Sched_commit (schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    NBC_Return_handle (handle);
    OBJ_RELEASE(schedule);
    return res;
  }

  res = NBC_Start (handle, schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    NBC_Return_handle (handle);
    return res;
  }

  *request = (ompi_request_t *) handle;

  return OMPI_SUCCESS;
} 

/* simple linear Alltoallw */
int ompi_coll_libnbc_ialltoallw_inter (const void* sendbuf, const int *sendcounts, const int *sdispls,
				       struct ompi_datatype_t * const *sendtypes, void* recvbuf, const int *recvcounts, const int *rdispls,
				       struct ompi_datatype_t * const *recvtypes, struct ompi_communicator_t *comm, ompi_request_t ** request,
				       struct mca_coll_base_module_2_1_0_t *module)
{
  int res, rsize;
  NBC_Schedule *schedule;
  char *rbuf, *sbuf;
  NBC_Handle *handle;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

  rsize = ompi_comm_remote_size (comm);

  schedule = OBJ_NEW(NBC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  for (int i = 0 ; i < rsize ; ++i) {
    /* post all sends */
    if (sendcounts[i] != 0) {
      sbuf = (char *) sendbuf + sdispls[i];
      res = NBC_Sched_send (sbuf, false, sendcounts[i], sendtypes[i], i, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        OBJ_RELEASE(schedule);
        return res;
      }
    }
    /* post all receives */
    if (recvcounts[i] != 0) {
      rbuf = (char *) recvbuf + rdispls[i];
      res = NBC_Sched_recv (rbuf, false, recvcounts[i], recvtypes[i], i, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        OBJ_RELEASE(schedule);
        return res;
      }
    }
  }

  res = NBC_Sched_commit (schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    return res;
  }

  res = NBC_Init_handle (comm, &handle, libnbc_module);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    return res;
  }

  res = NBC_Start (handle, schedule);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    NBC_Return_handle (handle);
    return res;
  }

  *request = (ompi_request_t *) handle;

  return OMPI_SUCCESS;
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
      res = NBC_Sched_send (sbuf, false, sendcounts[i], sendtypes[i], i, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
    /* post receive */
    if (recvcounts[i] != 0) {
      char *rbuf = (char *) recvbuf + rdispls[i];
      res = NBC_Sched_recv (rbuf, false, recvcounts[i], recvtypes[i], i, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
  }

  return OMPI_SUCCESS;
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
      res = NBC_Sched_send (sbuf, false, sendcounts[sndpeer], sendtypes[sndpeer], sndpeer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
    /* post receive */
    if (recvcounts[rcvpeer] != 0) {
      char *rbuf = (char *) recvbuf + rdispls[rcvpeer];
      res = NBC_Sched_recv (rbuf, false, recvcounts[rcvpeer], recvtypes[rcvpeer], rcvpeer, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
  }

  return OMPI_SUCCESS;
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
      res = NBC_Sched_copy (rbuf, false, counts[rpeer], types[rpeer],
                            (void *)(-gap), true, counts[rpeer], types[rpeer],
                            schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
    if (0 != counts[speer]) {
      res = NBC_Sched_send (sbuf, false , counts[speer], types[speer], speer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
    if (0 != counts[rpeer]) {
      res = NBC_Sched_recv (rbuf, false , counts[rpeer], types[rpeer], rpeer, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }

    if (0 != counts[rpeer]) {
      res = NBC_Sched_send ((void *)(-gap), true, counts[rpeer], types[rpeer], rpeer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
    if (0 != counts[speer]) {
      res = NBC_Sched_recv (sbuf, false, counts[speer], types[speer], speer, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
  }
  if (0 == (p%2)) {
    int peer = (rank + p/2) % p;

    char *tbuf = (char *) buf + displs[peer];
    (void)opal_datatype_span(&types[peer]->super, counts[peer], &gap);
    res = NBC_Sched_copy (tbuf, false, counts[peer], types[peer],
                          (void *)(-gap), true, counts[peer], types[peer],
                          schedule, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
    res = NBC_Sched_send ((void *)(-gap), true , counts[peer], types[peer], peer, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
    res = NBC_Sched_recv (tbuf, false , counts[peer], types[peer], peer, schedule, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
  }

  return OMPI_SUCCESS;
}
