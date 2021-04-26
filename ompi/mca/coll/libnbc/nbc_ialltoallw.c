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
 * Copyright (c) 2020      Bull SAS. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
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

static mca_base_var_enum_value_t ialltoallw_algorithms[] = {
    {0, "ignore"},
    {1, "linear"},
    {2, "pairwise"},
    {0, NULL}
};

/* The following are used by dynamic and forced rules */
/* this routine is called by the component only */

int ompi_coll_libnbc_alltoallw_check_forced_init (void)
{
  mca_base_var_enum_t *new_enum;

  mca_coll_libnbc_component.forced_params[ALLTOALLW].algorithm = 0;
  (void) mca_base_var_enum_create("coll_libnbc_alltoallw_algorithms", ialltoallw_algorithms, &new_enum);
  (void) mca_base_component_var_register(&mca_coll_libnbc_component.super.collm_version,
                                         "ialltoallw_algorithm",
                                         "Which ialltoallw algorithm is used unless MPI_IN_PLACE flag has been specified: 0 ignore, 1 linear, 2 pairwise",
                                         MCA_BASE_VAR_TYPE_INT, new_enum, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                         OPAL_INFO_LVL_5,
                                         MCA_BASE_VAR_SCOPE_ALL,
                                         &mca_coll_libnbc_component.forced_params[ALLTOALLW].algorithm);

  OBJ_RELEASE(new_enum);
  return OMPI_SUCCESS;
}

/* an alltoallw schedule can not be cached easily because the contents
 * ot the recvcounts array may change, so a comparison of the address
 * would not be sufficient ... we simply do not cache it */

/* simple linear Alltoallw */
static int nbc_alltoallw_init(const void* sendbuf, const int *sendcounts, const int *sdispls,
                              struct ompi_datatype_t * const *sendtypes, void* recvbuf, const int *recvcounts, const int *rdispls,
                              struct ompi_datatype_t * const *recvtypes, struct ompi_communicator_t *comm, ompi_request_t ** request,
                              struct mca_coll_base_module_2_3_0_t *module, bool persistent)
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

  enum {NBC_A2AW_LINEAR, NBC_A2AW_PAIRWISE, NBC_A2AW_INPLACE} alg;
  if (inplace) {
    alg = NBC_A2AW_INPLACE;
  } else if (libnbc_module->com_rules[ALLTOALLW]) {
    int algorithm,dummy1,dummy2,dummy3;
    /**
     * check to see if we have some filebased rules. As we don't have global
     * knowledge about the total amount of data, use the first available rule.
     * This allow the users to specify the alltoallw algorithm to be used only
     * based on the communicator size.
    */
    algorithm = ompi_coll_base_get_target_method_params (libnbc_module->com_rules[ALLTOALLW],
                                                         0, &dummy1, &dummy2, &dummy3);
    if (algorithm) {
      alg = algorithm - 1; /* -1 is to shift from algorithm ID to enum */
    } else {
      alg = NBC_A2AW_LINEAR; /* default if not inplace */
    }
  } else if (0 != mca_coll_libnbc_component.forced_params[ALLTOALLW].algorithm) {
    alg = mca_coll_libnbc_component.forced_params[ALLTOALLW].algorithm - 1; /* -1 is to shift from algorithm ID to enum */
  } else {
    alg = NBC_A2AW_LINEAR; /* default if not inplace */
  }

  opal_output_verbose(10, mca_coll_libnbc_component.stream,
                      "Libnbc ialltoallw : algorithm %d (no segmentation supported)",
                      alg + 1);
  /* copy data to receivbuffer */
  if (inplace) {
    ptrdiff_t lgap, lspan;
    for (int i = 0; i < p; i++) {
      lspan = opal_datatype_span(&recvtypes[i]->super, recvcounts[i], &lgap);
      if (lspan > span) {
        span = lspan;
      }
    }
    if (OPAL_UNLIKELY(0 == span)) {
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

  if (!inplace && sendcounts[rank] != 0) {
    rbuf = (char *) recvbuf + rdispls[rank];
    sbuf = (char *) sendbuf + sdispls[rank];
    res = NBC_Sched_copy(sbuf, false, sendcounts[rank], sendtypes[rank],
                         rbuf, false, recvcounts[rank], recvtypes[rank], schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
  }

  switch(alg) {
    case NBC_A2AW_INPLACE:
      res = a2aw_sched_inplace(rank, p, schedule, recvbuf,
                               recvcounts, rdispls, recvtypes);
      break;
    case NBC_A2AW_LINEAR:
      res = a2aw_sched_linear(rank, p, schedule,
                              sendbuf, sendcounts, sdispls, sendtypes,
                              recvbuf, recvcounts, rdispls, recvtypes);
      break;
    case NBC_A2AW_PAIRWISE:
      res = a2aw_sched_pairwise(rank, p, schedule,
                                sendbuf, sendcounts, sdispls, sendtypes,
                                recvbuf, recvcounts, rdispls, recvtypes);
      break;
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

int ompi_coll_libnbc_ialltoallw(const void* sendbuf, const int *sendcounts, const int *sdispls,
                                struct ompi_datatype_t * const *sendtypes, void* recvbuf, const int *recvcounts, const int *rdispls,
                                struct ompi_datatype_t * const *recvtypes, struct ompi_communicator_t *comm, ompi_request_t ** request,
				struct mca_coll_base_module_2_3_0_t *module) {
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
static int nbc_alltoallw_inter_init (const void* sendbuf, const int *sendcounts, const int *sdispls,
                                     struct ompi_datatype_t * const *sendtypes, void* recvbuf, const int *recvcounts, const int *rdispls,
                                     struct ompi_datatype_t * const *recvtypes, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                     struct mca_coll_base_module_2_3_0_t *module, bool persistent)
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

  res = NBC_Schedule_request(schedule, comm, libnbc_module, persistent, request, NULL);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    return res;
  }

  return OMPI_SUCCESS;
}

int ompi_coll_libnbc_ialltoallw_inter(const void* sendbuf, const int *sendcounts, const int *sdispls,
                                      struct ompi_datatype_t * const *sendtypes, void* recvbuf, const int *recvcounts, const int *rdispls,
                                      struct ompi_datatype_t * const *recvtypes, struct ompi_communicator_t *comm, ompi_request_t ** request,
				      struct mca_coll_base_module_2_3_0_t *module) {
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
                                    const void *sendbuf, const int *sendcounts, const int *sdispls,
                                    struct ompi_datatype_t * const * sendtypes,
                                    void *recvbuf, const int *recvcounts, const int *rdispls,
                                    struct ompi_datatype_t * const * recvtypes) {
  int res;

  for (int i = 0; i < p; i++) {
    ptrdiff_t gap, span;
    if (i == rank) {
      continue;
    }

    /* post send */
    span = opal_datatype_span(&sendtypes[i]->super, sendcounts[i], &gap);
    if (OPAL_LIKELY(0 < span)) {
      char *sbuf = (char *) sendbuf + sdispls[i];
      res = NBC_Sched_send (sbuf, false, sendcounts[i], sendtypes[i], i, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
    /* post receive */
    span = opal_datatype_span(&recvtypes[i]->super, recvcounts[i], &gap);
    if (OPAL_LIKELY(0 < span)) {
      char *rbuf = (char *) recvbuf + rdispls[i];
      res = NBC_Sched_recv (rbuf, false, recvcounts[i], recvtypes[i], i, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
  }

  return OMPI_SUCCESS;
}

__opal_attribute_unused__
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
  ptrdiff_t gap = 0;
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

int ompi_coll_libnbc_alltoallw_init(const void* sendbuf, const int *sendcounts, const int *sdispls,
                                    struct ompi_datatype_t * const *sendtypes, void* recvbuf, const int *recvcounts, const int *rdispls,
                                    struct ompi_datatype_t * const *recvtypes, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                    struct mca_coll_base_module_2_3_0_t *module) {
    int res = nbc_alltoallw_init(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes,
                                 comm, request, module, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
    }

    return OMPI_SUCCESS;
}

int ompi_coll_libnbc_alltoallw_inter_init(const void* sendbuf, const int *sendcounts, const int *sdispls,
                                          struct ompi_datatype_t * const *sendtypes, void* recvbuf, const int *recvcounts, const int *rdispls,
                                          struct ompi_datatype_t * const *recvtypes, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                          struct mca_coll_base_module_2_3_0_t *module) {
    int res = nbc_alltoallw_inter_init(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes,
                                       comm, request, module, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
    }

    return OMPI_SUCCESS;
}
