/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2014-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2017-2022 IBM Corporation.  All rights reserved.
 * Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "nbc_internal.h"

static inline int allgather_sched_linear(
    int rank, int comm_size, NBC_Schedule *schedule, const void *sendbuf,
    int scount, struct ompi_datatype_t *sdtype, void *recvbuf, int rcount,
    struct ompi_datatype_t *rdtype);
static inline int allgather_sched_recursivedoubling(
    int rank, int comm_size, NBC_Schedule *schedule, const void *sbuf,
    int scount, struct ompi_datatype_t *sdtype, void *rbuf, int rcount,
    struct ompi_datatype_t *rdtype);

#ifdef NBC_CACHE_SCHEDULE
/* tree comparison function for schedule cache */
int NBC_Allgather_args_compare(NBC_Allgather_args *a, NBC_Allgather_args *b, void *param) {
  if ((a->sendbuf == b->sendbuf) &&
      (a->sendcount == b->sendcount) &&
      (a->sendtype == b->sendtype) &&
      (a->recvbuf == b->recvbuf) &&
      (a->recvcount == b->recvcount) &&
      (a->recvtype == b->recvtype) ) {
    return 0;
  }

  if( a->sendbuf < b->sendbuf ) {
    return -1;
  }

  return 1;
}
#endif

static int nbc_allgather_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                              MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                              mca_coll_base_module_t *module, bool persistent)
{
  int rank, p, res;
  MPI_Aint rcvext;
  NBC_Schedule *schedule;
  char *rbuf, inplace;
#ifdef NBC_CACHE_SCHEDULE
  NBC_Allgather_args *args, *found, search;
#endif
  enum { NBC_ALLGATHER_LINEAR, NBC_ALLGATHER_RDBL} alg;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

  NBC_IN_PLACE(sendbuf, recvbuf, inplace);

  rank = ompi_comm_rank (comm);
  p = ompi_comm_size (comm);
  int is_commsize_pow2 = !(p & (p - 1));

  if (libnbc_iallgather_algorithm == 0) {
    alg = NBC_ALLGATHER_LINEAR;
  } else {
    /* user forced dynamic decision */
    if (libnbc_iallgather_algorithm == 1) {
      alg = NBC_ALLGATHER_LINEAR;
    } else if (libnbc_iallgather_algorithm == 2 && is_commsize_pow2) {
      alg = NBC_ALLGATHER_RDBL;
    } else {
      alg = NBC_ALLGATHER_LINEAR;
    }
  }

  res = ompi_datatype_type_extent(recvtype, &rcvext);
  if (MPI_SUCCESS != res) {
    return res;
  }

  if (inplace) {
    sendtype = recvtype;
    sendcount = recvcount;
  } else if (!persistent) { /* for persistent, the copy must be scheduled */
    /* copy my data to receive buffer */
    rbuf = (char *) recvbuf + (MPI_Aint)rcvext * rank * recvcount;
    res = NBC_Copy (sendbuf, sendcount, sendtype, rbuf, recvcount, recvtype, comm);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
  }
  if (1 == p && (!persistent || inplace)) {
    return nbc_get_noop_request(persistent, request);
  }

#ifdef NBC_CACHE_SCHEDULE
  /* search schedule in communicator specific tree */
  search.sendbuf = sendbuf;
  search.sendcount = sendcount;
  search.sendtype = sendtype;
  search.recvbuf = recvbuf;
  search.recvcount = recvcount;
  search.recvtype = recvtype;
  found = (NBC_Allgather_args *) hb_tree_search ((hb_tree*)libnbc_module->NBC_Dict[NBC_ALLGATHER], &search);
  if (NULL == found) {
#endif
    schedule = OBJ_NEW(NBC_Schedule);
    if (OPAL_UNLIKELY(NULL == schedule)) {
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    if (persistent && !inplace) {
      /* for nonblocking, data has been copied already */
      /* copy my data to receive buffer (= send buffer of NBC_Sched_send) */
      rbuf = (char *)recvbuf + (MPI_Aint) rcvext * rank * recvcount;
      res = NBC_Sched_copy((void *)sendbuf, false, sendcount, sendtype,
                            rbuf, false, recvcount, recvtype, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        OBJ_RELEASE(schedule);
        return res;
      }
    }

    switch (alg) {
      case NBC_ALLGATHER_LINEAR:
        res = allgather_sched_linear(rank, p, schedule, sendbuf, sendcount, sendtype,
                                     recvbuf, recvcount, recvtype);
        break;
      case NBC_ALLGATHER_RDBL:
        res = allgather_sched_recursivedoubling(rank, p, schedule, sendbuf, sendcount,
                                                sendtype, recvbuf, recvcount, recvtype);
        break;
    }

    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      OBJ_RELEASE(schedule);
      return res;
    }

    res = NBC_Sched_commit(schedule);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      OBJ_RELEASE(schedule);
      return res;
    }

#ifdef NBC_CACHE_SCHEDULE
    /* save schedule to tree */
    args = (NBC_Allgather_args *) malloc (sizeof (args));
    args->sendbuf = sendbuf;
    args->sendcount = sendcount;
    args->sendtype = sendtype;
    args->recvbuf = recvbuf;
    args->recvcount = recvcount;
    args->recvtype = recvtype;
    args->schedule = schedule;

    res = hb_tree_insert ((hb_tree *) libnbc_module->NBC_Dict[NBC_ALLGATHER], args, args, 0);
    if (res != 0) {
      free (args);
    } else {
      OBJ_RETAIN(schedule);
    }

    /* increase number of elements for A2A */
    if (++libnbc_module->NBC_Dict_size[NBC_ALLGATHER] > NBC_SCHED_DICT_UPPER) {
      NBC_SchedCache_dictwipe ((hb_tree *) libnbc_module->NBC_Dict[NBC_ALLGATHER], &libnbc_module->NBC_Dict_size[NBC_ALLGATHER]);
    }
  } else {
    /* found schedule */
    schedule = found->schedule;
    OBJ_RETAIN(schedule);
  }
#endif

  res = NBC_Schedule_request(schedule, comm, libnbc_module, persistent, request, NULL);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    return res;
  }

  return OMPI_SUCCESS;
}

int ompi_coll_libnbc_iallgather(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                                MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                mca_coll_base_module_t *module)
{
    int res = nbc_allgather_init(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype,
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

static int nbc_allgather_inter_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                                    MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                    mca_coll_base_module_t *module, bool persistent)
{
  int res, rsize;
  MPI_Aint rcvext;
  NBC_Schedule *schedule;
  char *rbuf;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

  res = ompi_datatype_type_extent(recvtype, &rcvext);
  if (MPI_SUCCESS != res) {
    NBC_Error ("MPI Error in ompi_datatype_type_extent() (%i)", res);
    return res;
  }

  rsize = ompi_comm_remote_size (comm);

  /* set up schedule */
  schedule = OBJ_NEW(NBC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  /* do rsize - 1 rounds */
  for (int r = 0 ; r < rsize ; ++r) {
    /* recv from rank r */
    rbuf = (char *) recvbuf + (MPI_Aint) rcvext * r * recvcount;
    res = NBC_Sched_recv (rbuf, false, recvcount, recvtype, r, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      OBJ_RELEASE(schedule);
      return res;
    }

    /* send to rank r */
    res = NBC_Sched_send (sendbuf, false, sendcount, sendtype, r, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      OBJ_RELEASE(schedule);
      return res;
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

int ompi_coll_libnbc_iallgather_inter(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
				      MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
				      mca_coll_base_module_t *module) {
    int res = nbc_allgather_inter_init(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype,
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

/*
 * allgather_sched_linear
 *
 * Description: an implementation of Iallgather using linear algorithm
 *
 * Time: O(comm_size)
 * Schedule length (rounds): O(comm_size)
 */
static inline int allgather_sched_linear(
    int rank, int comm_size, NBC_Schedule *schedule, const void *sendbuf,
    int scount, struct ompi_datatype_t *sdtype, void *recvbuf, int rcount,
    struct ompi_datatype_t *rdtype)
{
    int res = OMPI_SUCCESS;
    ptrdiff_t rlb, rext;

    res = ompi_datatype_get_extent(rdtype, &rlb, &rext);
    char *sbuf = (char *)recvbuf + (MPI_Aint) rext * rank * rcount;

    for (int remote = 0; remote < comm_size ; ++remote) {
        if (remote != rank) {
            /* Recv from rank remote */
            char *rbuf = (char *)recvbuf + (MPI_Aint) rext * remote * rcount;
            res = NBC_Sched_recv(rbuf, false, rcount, rdtype, remote, schedule, false);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }

            /* Send to rank remote - not from the sendbuf to optimize MPI_IN_PLACE */
            res = NBC_Sched_send(sbuf, false, rcount, rdtype, remote, schedule, false);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
        }
    }

cleanup_and_return:
    return res;
}

/*
 * allgather_sched_recursivedoubling
 *
 * Description: an implementation of Iallgather using recursive doubling algorithm
 * Limitation: power-of-two number of processes only
 * Time: O(log(comm_size))
 * Schedule length (rounds): O(log(comm_size))
 * Memory: no additional memory requirements beyond user-supplied buffers.
 *
 * Example on 4 nodes:
 *   Initialization: everyone has its own buffer at location rank in rbuf
 *    #     0      1      2      3
 *         [0]    [ ]    [ ]    [ ]
 *         [ ]    [1]    [ ]    [ ]
 *         [ ]    [ ]    [2]    [ ]
 *         [ ]    [ ]    [ ]    [3]
 *   Step 0: exchange data with (rank ^ 2^0)
 *    #     0      1      2      3
 *         [0]    [0]    [ ]    [ ]
 *         [1]    [1]    [ ]    [ ]
 *         [ ]    [ ]    [2]    [2]
 *         [ ]    [ ]    [3]    [3]
 *   Step 1: exchange data with (rank ^ 2^1) (if you can)
 *    #     0      1      2      3
 *         [0]    [0]    [0]    [0]
 *         [1]    [1]    [1]    [1]
 *         [2]    [2]    [2]    [2]
 *         [3]    [3]    [3]    [3]
 *
 */
static inline int allgather_sched_recursivedoubling(
    int rank, int comm_size, NBC_Schedule *schedule, const void *sbuf,
    int scount, struct ompi_datatype_t *sdtype, void *rbuf, int rcount,
    struct ompi_datatype_t *rdtype)
{
    int res = OMPI_SUCCESS;
    ptrdiff_t rlb, rext;
    char *tmpsend = NULL, *tmprecv = NULL;

    res = ompi_datatype_get_extent(rdtype, &rlb, &rext);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }

    int sendblocklocation = rank;
    for (int distance = 1; distance < comm_size; distance <<= 1) {
        int remote = rank ^ distance;

        tmpsend = (char *)rbuf + (ptrdiff_t)sendblocklocation * (ptrdiff_t)rcount * rext;
        if (rank < remote) {
            tmprecv = (char *)rbuf + (ptrdiff_t)(sendblocklocation + distance) * (ptrdiff_t)rcount * rext;
        } else {
            tmprecv = (char *)rbuf + (ptrdiff_t)(sendblocklocation - distance) * (ptrdiff_t)rcount * rext;
            sendblocklocation -= distance;
        }

        res = NBC_Sched_send(tmpsend, false, (ptrdiff_t)distance * (ptrdiff_t)rcount,
                             rdtype, remote, schedule, false);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }

        res = NBC_Sched_recv(tmprecv, false, (ptrdiff_t)distance * (ptrdiff_t)rcount,
                             rdtype, remote, schedule, true);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
    }

cleanup_and_return:
    return res;
}

int ompi_coll_libnbc_allgather_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                                    MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                    mca_coll_base_module_t *module) {
    int res = nbc_allgather_init(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype,
                                 comm, request, module, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
    }

    return OMPI_SUCCESS;
}

int ompi_coll_libnbc_allgather_inter_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                                          MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                          mca_coll_base_module_t *module) {
    int res = nbc_allgather_inter_init(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype,
                                       comm, request, module, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
    }

    return OMPI_SUCCESS;
}
