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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "nbc_internal.h"

static inline int a2aw_sched_linear(int rank, int p, NBC_Schedule *schedule,
                                    const void *sendbuf, ompi_count_array_t sendcounts, ompi_disp_array_t sdispls,
                                    struct ompi_datatype_t * const * sendtypes,
                                    void *recvbuf, ompi_count_array_t recvcounts, ompi_disp_array_t rdispls,
                                    struct ompi_datatype_t * const * recvtypes);

static inline int a2aw_sched_pairwise(int rank, int p, NBC_Schedule *schedule,
                                      const void *sendbuf, ompi_count_array_t sendcounts, ompi_disp_array_t sdispls,
                                      struct ompi_datatype_t * const * sendtypes,
                                      void *recvbuf, ompi_count_array_t recvcounts, ompi_disp_array_t rdispls,
                                      struct ompi_datatype_t * const * recvtypes);

static inline int a2aw_sched_inplace(int rank, int p, NBC_Schedule *schedule,
                                    void *buf, ompi_count_array_t counts, ompi_disp_array_t displs,
                                    struct ompi_datatype_t * const * types);

/* an alltoallw schedule can not be cached easily because the contents
 * of the recvcounts array may change, so a comparison of the address
 * would not be sufficient ... we simply do not cache it */

/* simple linear Alltoallw */
static int nbc_alltoallw_init(const void* sendbuf, ompi_count_array_t sendcounts, ompi_disp_array_t sdispls,
                              struct ompi_datatype_t * const *sendtypes, void* recvbuf, ompi_count_array_t recvcounts, ompi_disp_array_t rdispls,
                              struct ompi_datatype_t * const *recvtypes, struct ompi_communicator_t *comm, ompi_request_t ** request,
                              mca_coll_base_module_t *module, bool persistent)
{
  int rank, p, res;
  NBC_Schedule *schedule;
  char *rbuf, *sbuf, inplace;
  ptrdiff_t span=0;
  void *tmpbuf = NULL;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

  NBC_IN_PLACE(sendbuf, recvbuf, inplace);

  rank = ompi_comm_rank (comm);
  p = ompi_comm_size (comm);

  /* copy data to receivbuffer */
  if (inplace) {
    ptrdiff_t lgap, lspan;
    for (int i = 0; i < p; i++) {
      lspan = opal_datatype_span(&recvtypes[i]->super, ompi_count_array_get(recvcounts, i), &lgap);
      if (lspan > span) {
        span = lspan;
      }
    }
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
    sendtypes = recvtypes;
  }

  schedule = OBJ_NEW(NBC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    free(tmpbuf);
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  if (!inplace && ompi_count_array_get(sendcounts, rank) != 0) {
    rbuf = (char *) recvbuf + ompi_disp_array_get(rdispls, rank);
    sbuf = (char *) sendbuf + ompi_disp_array_get(sdispls, rank);
    res = NBC_Sched_copy(sbuf, false, ompi_count_array_get(sendcounts, rank), sendtypes[rank],
                         rbuf, false, ompi_count_array_get(recvcounts, rank), recvtypes[rank], schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
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

int ompi_coll_libnbc_ialltoallw(const void* sendbuf, ompi_count_array_t sendcounts, ompi_disp_array_t sdispls,
                                struct ompi_datatype_t * const *sendtypes, void* recvbuf, ompi_count_array_t recvcounts, ompi_disp_array_t rdispls,
                                struct ompi_datatype_t * const *recvtypes, struct ompi_communicator_t *comm, ompi_request_t ** request,
				mca_coll_base_module_t *module) {
    int res = nbc_alltoallw_init(sendbuf, sendcounts, sdispls, sendtypes,
                                 recvbuf, recvcounts, rdispls, recvtypes,
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

/* simple linear Alltoallw */
static int nbc_alltoallw_inter_init (const void* sendbuf, ompi_count_array_t sendcounts, ompi_disp_array_t sdispls,
                                     struct ompi_datatype_t * const *sendtypes, void* recvbuf, ompi_count_array_t recvcounts, ompi_disp_array_t rdispls,
                                     struct ompi_datatype_t * const *recvtypes, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                     mca_coll_base_module_t *module, bool persistent)
{
  int res, rsize;
  NBC_Schedule *schedule;
  char *rbuf, *sbuf;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

  rsize = ompi_comm_remote_size (comm);

  schedule = OBJ_NEW(NBC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  for (int i = 0 ; i < rsize ; ++i) {
    /* post all sends */
    if (ompi_count_array_get(sendcounts, i) != 0) {
      sbuf = (char *) sendbuf + ompi_disp_array_get(sdispls, i);
      res = NBC_Sched_send (sbuf, false, ompi_count_array_get(sendcounts, i), sendtypes[i], i, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        OBJ_RELEASE(schedule);
        return res;
      }
    }
    /* post all receives */
    if (ompi_count_array_get(recvcounts, i) != 0) {
      rbuf = (char *) recvbuf + ompi_disp_array_get(rdispls, i);
      res = NBC_Sched_recv (rbuf, false, ompi_count_array_get(recvcounts, i), recvtypes[i], i, schedule, false);
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

  res = NBC_Schedule_request(schedule, comm, libnbc_module, persistent, request, NULL);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    return res;
  }

  return OMPI_SUCCESS;
}

int ompi_coll_libnbc_ialltoallw_inter(const void* sendbuf, ompi_count_array_t sendcounts, ompi_disp_array_t sdispls,
                                      struct ompi_datatype_t * const *sendtypes, void* recvbuf, ompi_count_array_t recvcounts, ompi_disp_array_t rdispls,
                                      struct ompi_datatype_t * const *recvtypes, struct ompi_communicator_t *comm, ompi_request_t ** request,
				      mca_coll_base_module_t *module) {
    int res = nbc_alltoallw_inter_init(sendbuf, sendcounts, sdispls, sendtypes,
                                       recvbuf, recvcounts, rdispls, recvtypes,
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

static inline int a2aw_sched_linear(int rank, int p, NBC_Schedule *schedule,
                                    const void *sendbuf, ompi_count_array_t sendcounts, ompi_disp_array_t sdispls,
                                    struct ompi_datatype_t * const * sendtypes,
                                    void *recvbuf, ompi_count_array_t recvcounts, ompi_disp_array_t rdispls,
                                    struct ompi_datatype_t * const * recvtypes) {
  int res;

  for (int i = 0; i < p; i++) {
    ptrdiff_t gap, span;
    if (i == rank) {
      continue;
    }

    /* post send */
    span = opal_datatype_span(&sendtypes[i]->super, ompi_count_array_get(sendcounts, i), &gap);
    if (OPAL_LIKELY(0 < span)) {
      char *sbuf = (char *) sendbuf + ompi_disp_array_get(sdispls, i);
      res = NBC_Sched_send (sbuf, false, ompi_count_array_get(sendcounts, i), sendtypes[i], i, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
    /* post receive */
    span = opal_datatype_span(&recvtypes[i]->super, ompi_count_array_get(recvcounts, i), &gap);
    if (OPAL_LIKELY(0 < span)) {
      char *rbuf = (char *) recvbuf + ompi_disp_array_get(rdispls, i);
      res = NBC_Sched_recv (rbuf, false, ompi_count_array_get(recvcounts, i), recvtypes[i], i, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
  }

  return OMPI_SUCCESS;
}

__opal_attribute_unused__
static inline int a2aw_sched_pairwise(int rank, int p, NBC_Schedule *schedule,
                                      const void *sendbuf, ompi_count_array_t sendcounts, ompi_disp_array_t sdispls,
                                      struct ompi_datatype_t * const * sendtypes,
                                      void *recvbuf, ompi_count_array_t recvcounts, ompi_disp_array_t rdispls,
                                      struct ompi_datatype_t * const * recvtypes) {
  int res;

  for (int i = 1; i < p; i++) {
    int sndpeer = (rank + i) % p;
    int rcvpeer = (rank + p - i) % p;

    /* post send */
    if (ompi_count_array_get(sendcounts, sndpeer) != 0) {
      char *sbuf = (char *) sendbuf + ompi_disp_array_get(sdispls, sndpeer);
      res = NBC_Sched_send (sbuf, false, ompi_count_array_get(sendcounts, sndpeer), sendtypes[sndpeer], sndpeer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
    /* post receive */
    if (ompi_count_array_get(recvcounts, rcvpeer) != 0) {
      char *rbuf = (char *) recvbuf + ompi_disp_array_get(rdispls, rcvpeer);
      res = NBC_Sched_recv (rbuf, false, ompi_count_array_get(recvcounts, rcvpeer), recvtypes[rcvpeer], rcvpeer, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
  }

  return OMPI_SUCCESS;
}

static inline int a2aw_sched_inplace(int rank, int p, NBC_Schedule *schedule,
                                     void *buf, ompi_count_array_t counts, ompi_disp_array_t displs,
                                     struct ompi_datatype_t * const * types) {
  ptrdiff_t gap = 0;
  int res;

  for (int i = 1; i < (p+1)/2; i++) {
    int speer = (rank + i) % p;
    int rpeer = (rank + p - i) % p;
    char *sbuf = (char *) buf + ompi_disp_array_get(displs, speer);
    char *rbuf = (char *) buf + ompi_disp_array_get(displs, rpeer);

    if (0 != ompi_count_array_get(counts, rpeer)) {
      (void)opal_datatype_span(&types[rpeer]->super, ompi_count_array_get(counts, rpeer), &gap);
      res = NBC_Sched_copy (rbuf, false, ompi_count_array_get(counts, rpeer), types[rpeer],
                            (void *)(-gap), true, ompi_count_array_get(counts, rpeer), types[rpeer],
                            schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
    if (0 != ompi_count_array_get(counts, speer)) {
      res = NBC_Sched_send (sbuf, false, ompi_count_array_get(counts, speer), types[speer], speer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
    if (0 != ompi_count_array_get(counts, rpeer)) {
      res = NBC_Sched_recv (rbuf, false, ompi_count_array_get(counts, rpeer), types[rpeer], rpeer, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }

    if (0 != ompi_count_array_get(counts, rpeer)) {
      res = NBC_Sched_send ((void *)(-gap), true, ompi_count_array_get(counts, rpeer), types[rpeer], rpeer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
    if (0 != ompi_count_array_get(counts, speer)) {
      res = NBC_Sched_recv (sbuf, false, ompi_count_array_get(counts, speer), types[speer], speer, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
  }
  if (0 == (p%2)) {
    int peer = (rank + p/2) % p;

    char *tbuf = (char *) buf + ompi_disp_array_get(displs, peer);
    (void)opal_datatype_span(&types[peer]->super, ompi_count_array_get(counts, peer), &gap);
    res = NBC_Sched_copy (tbuf, false, ompi_count_array_get(counts, peer), types[peer],
                          (void *)(-gap), true, ompi_count_array_get(counts, peer), types[peer],
                          schedule, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
    res = NBC_Sched_send ((void *)(-gap), true, ompi_count_array_get(counts, peer), types[peer], peer, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
    res = NBC_Sched_recv (tbuf, false, ompi_count_array_get(counts, peer), types[peer], peer, schedule, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
  }

  return OMPI_SUCCESS;
}

int ompi_coll_libnbc_alltoallw_init(const void* sendbuf, ompi_count_array_t sendcounts, ompi_disp_array_t sdispls,
                                    struct ompi_datatype_t * const *sendtypes, void* recvbuf, ompi_count_array_t recvcounts, ompi_disp_array_t rdispls,
                                    struct ompi_datatype_t * const *recvtypes, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                    mca_coll_base_module_t *module) {
    int res = nbc_alltoallw_init(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes,
                                 comm, request, module, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
    }

    return OMPI_SUCCESS;
}

int ompi_coll_libnbc_alltoallw_inter_init(const void* sendbuf, ompi_count_array_t sendcounts, ompi_disp_array_t sdispls,
                                          struct ompi_datatype_t * const *sendtypes, void* recvbuf,
                                          ompi_count_array_t recvcounts, ompi_disp_array_t rdispls,
                                          struct ompi_datatype_t * const *recvtypes, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                          mca_coll_base_module_t *module) {
    int res = nbc_alltoallw_inter_init(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes,
                                       comm, request, module, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
    }

    return OMPI_SUCCESS;
}
