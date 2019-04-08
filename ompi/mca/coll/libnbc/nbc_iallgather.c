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

static inline int allgather_sched_linear(
    int rank, int comm_size, NBC_Schedule *schedule, const void *sendbuf,
    int scount, struct ompi_datatype_t *sdtype, void *recvbuf, int rcount,
    struct ompi_datatype_t *rdtype);
static inline int allgather_sched_recursivedoubling(
    int rank, int comm_size, NBC_Schedule *schedule, const void *sbuf,
    int scount, struct ompi_datatype_t *sdtype, void *rbuf, int rcount,
    struct ompi_datatype_t *rdtype);
static inline int allgather_sched_bruck(
    int rank, int comm_size, NBC_Schedule *schedule, const void *sbuf,
    int scount, struct ompi_datatype_t *sdtype, void *rbuf, int rcount,
    struct ompi_datatype_t *rdtype, void *tmpbuf, char inplace);

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
                              struct mca_coll_base_module_2_3_0_t *module, bool persistent)
{
  int rank, p, res;
  MPI_Aint rcvext;
  NBC_Schedule *schedule;
  void *tmpbuf = NULL;
  char *rbuf, inplace;
#ifdef NBC_CACHE_SCHEDULE
  NBC_Allgather_args *args, *found, search;
#endif
  enum { NBC_ALLGATHER_LINEAR, NBC_ALLGATHER_RDBL, NBC_ALLGATHER_BRUCK } alg;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;
  ptrdiff_t span, gap;

  NBC_IN_PLACE(sendbuf, recvbuf, inplace);

  rank = ompi_comm_rank (comm);
  p = ompi_comm_size (comm);
  int is_commsize_pow2 = !(p & (p - 1));

/* algorithm selection */
  if (libnbc_iallgather_algorithm == 0) {
    alg = NBC_ALLGATHER_LINEAR;
  } else {
    /* user forced dynamic decision */
    if (libnbc_iallgather_algorithm == 1) {
      alg = NBC_ALLGATHER_LINEAR;
    } else if (libnbc_iallgather_algorithm == 2 && is_commsize_pow2) {
      alg = NBC_ALLGATHER_RDBL;
    } else if (libnbc_iallgather_algorithm == 3) {
      alg = NBC_ALLGATHER_BRUCK;
    } else {
      alg = NBC_ALLGATHER_LINEAR;
    }
  }

  res = ompi_datatype_type_extent(recvtype, &rcvext);
  if (MPI_SUCCESS != res) {
    return res;
  }

  /* allocate temporary buffers */
  if (alg == NBC_ALLGATHER_BRUCK) {
    span = opal_datatype_span(&recvtype->super, (int64_t)(p - rank) * recvcount, &gap);
    tmpbuf = (char *)calloc(span, sizeof(char));
    if (OPAL_UNLIKELY(NULL == tmpbuf)) {
      return OMPI_ERR_OUT_OF_RESOURCE;
    }
  }

  if (inplace) {
    sendtype = recvtype;
    sendcount = recvcount;

    if ((alg == NBC_ALLGATHER_BRUCK) && (0 != rank)) {  /* non root with MPI_IN_PLACE */
        /* copy r^th block from receive buffer to block 0 */
        rbuf = (char *) recvbuf + rank * recvcount * rcvext;
        res = NBC_Copy (rbuf, recvcount, recvtype, recvbuf, recvcount, recvtype, comm);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          free(tmpbuf);
          return res;
        }
    }
  } else if (!persistent) { /* for persistent, the copy must be scheduled */
    if (alg == NBC_ALLGATHER_BRUCK) {
      /* copy send buffer to block 0 of receive buffer */
      res = NBC_Copy (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm);
    } else {
      /* copy send buffer to r^th block of receive buffer */
      rbuf = (char *) recvbuf + rank * recvcount * rcvext;
      res = NBC_Copy (sendbuf, sendcount, sendtype, rbuf, recvcount, recvtype, comm);
    }
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      free(tmpbuf);
      return res;
    }
  }
  if (1 == p && (!persistent || inplace)) {
    free(tmpbuf);
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
      free(tmpbuf);
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    if (persistent && !inplace) {
      /* for nonblocking, data has been copied already */
      /* copy my data to receive buffer (= send buffer of NBC_Sched_send) */
      if (alg == NBC_ALLGATHER_BRUCK) {
        res = NBC_Sched_copy((void *)sendbuf, false, sendcount, sendtype,
                              recvbuf, false, recvcount, recvtype, schedule, false);
      } else {
        rbuf = (char *)recvbuf + rank * recvcount * rcvext;
        res = NBC_Sched_copy((void *)sendbuf, false, sendcount, sendtype,
                              rbuf, false, recvcount, recvtype, schedule, true);
      }
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        OBJ_RELEASE(schedule);
        free(tmpbuf);
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
      case NBC_ALLGATHER_BRUCK:
        res = allgather_sched_bruck(rank, p, schedule, sendbuf, sendcount,
                                                sendtype, recvbuf, recvcount, recvtype, tmpbuf, inplace);
        break;
    }

    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      OBJ_RELEASE(schedule);
      free(tmpbuf);
      return res;
    }

    res = NBC_Sched_commit(schedule);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      OBJ_RELEASE(schedule);
      free(tmpbuf);
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

  res = NBC_Schedule_request(schedule, comm, libnbc_module, persistent, request, tmpbuf);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    OBJ_RELEASE(schedule);
    free(tmpbuf);
    return res;
  }

  return OMPI_SUCCESS;
}

int ompi_coll_libnbc_iallgather(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                                MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                struct mca_coll_base_module_2_3_0_t *module)
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
                                    struct mca_coll_base_module_2_3_0_t *module, bool persistent)
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
    rbuf = (char *) recvbuf + r * recvcount * rcvext;
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
				      struct mca_coll_base_module_2_3_0_t *module) {
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
    char *sbuf = (char *)recvbuf + rank * rcount * rext;

    for (int remote = 0; remote < comm_size ; ++remote) {
        if (remote != rank) {
            /* Recv from rank remote */
            char *rbuf = (char *)recvbuf + remote * rcount * rext;
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

/*
 * allgather_sched_bruck
 * 
 * Description:     Variation to All-to-all algorithm described by Bruck et al.in
 *                  "Efficient Algorithms for All-to-all Communications
 *                  in Multiport Message-Passing Systems"
 * Time:            O(log(comm_size))
 * Schedule length: O(log(comm_size))
 * Memory:          non-zero ranks require temp buffer to perform final step
 *                  in the algorithm.
 *
 * Example on 6 nodes:
 *   Initialization: everyone has its own buffer at location 0 in rbuf
 *    #     0      1      2      3      4      5
 *         [0]    [1]    [2]    [3]    [4]    [5]
 *   Step 0: send message to (rank - 2^0), receive message from (rank + 2^0)
 *    #     0      1      2      3      4      5
 *         [0]    [1]    [2]    [3]    [4]    [5]
 *         [1]    [2]    [3]    [4]    [5]    [0]
 *   Step 1: send message to (rank - 2^1), receive message from (rank + 2^1)
 *           message contains all blocks from location 0 to 2^1*block size
 *    #     0      1      2      3      4      5
 *         [0]    [1]    [2]    [3]    [4]    [5]
 *         [1]    [2]    [3]    [4]    [5]    [0]
 *         [2]    [3]    [4]    [5]    [0]    [1]
 *         [3]    [4]    [5]    [0]    [1]    [2]
 *   Step 2: send message to (rank - 2^2), receive message from (rank + 2^2)
 *           message size is "all remaining blocks"
 *    #     0      1      2      3      4      5
 *         [0]    [1]    [2]    [3]    [4]    [5]
 *         [1]    [2]    [3]    [4]    [5]    [0]
 *         [2]    [3]    [4]    [5]    [0]    [1]
 *         [3]    [4]    [5]    [0]    [1]    [2]
 *         [4]    [5]    [0]    [1]    [2]    [3]
 *         [5]    [0]    [1]    [2]    [3]    [4]
 *    Finalization: Do a local shift to get data in correct place
 *    #     0      1      2      3      4      5
 *         [0]    [0]    [0]    [0]    [0]    [0]
 *         [1]    [1]    [1]    [1]    [1]    [1]
 *         [2]    [2]    [2]    [2]    [2]    [2]
 *         [3]    [3]    [3]    [3]    [3]    [3]
 *         [4]    [4]    [4]    [4]    [4]    [4]
 *         [5]    [5]    [5]    [5]    [5]    [5]
 */
static inline int allgather_sched_bruck(
    int rank, int comm_size, NBC_Schedule *schedule, const void *sbuf,
    int scount, struct ompi_datatype_t *sdtype, void *rbuf, int rcount,
    struct ompi_datatype_t *rdtype, void *tmpbuf, char inplace)
{
    int res = OMPI_SUCCESS;
    ptrdiff_t rlb, rext;
    char *tmpsend = NULL, *tmprecv = NULL;

    res = ompi_datatype_get_extent(rdtype, &rlb, &rext);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }

    /* Communication step:
       At every step i, rank r:
       - doubles the distance
       - sends message which starts at begining of rbuf and has size
       (blockcount * rcount) to rank (r - distance)
       - receives message of size blockcount * rcount from rank (r + distance)
       at location (rbuf + distance * rcount * rext)
       - blockcount doubles until last step when only the remaining data is
       exchanged.
    */
    int blockcount = 1;
    for(int distance = 1; distance < comm_size; distance <<= 1) {
        int sendto = (rank - distance + comm_size) % comm_size;
        int recvfrom = (rank + distance) % comm_size;

        if (distance <= (comm_size >> 1)) {
            blockcount = distance;
        } else {
            blockcount = comm_size - distance;
        }

        tmpsend = (char *)rbuf;
        tmprecv = tmpsend + (ptrdiff_t)distance * (ptrdiff_t)rcount * rext;

        res = NBC_Sched_send(tmpsend, false, (ptrdiff_t)blockcount * (ptrdiff_t)rcount,
                             rdtype, sendto, schedule, false);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }

        res = NBC_Sched_recv(tmprecv, false, (ptrdiff_t)blockcount * (ptrdiff_t)rcount,
                             rdtype, recvfrom, schedule, true);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
    }

    /* Finalization step:
       On all nodes except 0, data needs to be shifted locally:
       - copy blocks [0 .. (comm_size - rank - 1)] from rbuf to shiftbuf buffer
       - move blocks [(comm_size - rank) .. size] from rbuf to begining of rbuf
       - copy blocks from shift buffer starting at block [rank] in rbuf.
    */
    if (0 != rank) {
        ptrdiff_t gap;

        (void)opal_datatype_span(&rdtype->super, (int64_t)(comm_size - rank) * rcount, &gap);
        char * shiftbuf = (char *)tmpbuf - gap;

        /* 1. copy blocks [0 .. (comm_size - rank - 1)] from rbuf to shift buffer */
        res = NBC_Sched_copy((char *)rbuf, false, (ptrdiff_t)(comm_size - rank) * (ptrdiff_t)rcount, rdtype,
                             shiftbuf, false, (ptrdiff_t)(comm_size - rank) * (ptrdiff_t)rcount, rdtype, schedule, false);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }

        /* 2. move blocks [(comm_size - rank) .. size] from rbuf to the begining of rbuf */
        tmpsend = (char *)rbuf + (ptrdiff_t)(comm_size - rank) * (ptrdiff_t)rcount * rext;
        res = NBC_Sched_copy(tmpsend, false, (ptrdiff_t)rank * (ptrdiff_t)rcount, rdtype,
                             (char *)rbuf, false, (ptrdiff_t)rank * (ptrdiff_t)rcount, rdtype, schedule, false);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
        
        /* 3. copy blocks from shift buffer back to rbuf starting at block [rank]. */
        tmprecv = (char *)rbuf + (ptrdiff_t)rank * (ptrdiff_t)rcount * rext;
        res = NBC_Sched_copy(shiftbuf, false, (ptrdiff_t)(comm_size - rank) * (ptrdiff_t)rcount, rdtype,
                             tmprecv, false, (ptrdiff_t)(comm_size - rank) * (ptrdiff_t)rcount, rdtype, schedule, false);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
    }


cleanup_and_return:
    return res;
}

int ompi_coll_libnbc_allgather_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                                    MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                    struct mca_coll_base_module_2_3_0_t *module) {
    int res = nbc_allgather_init(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype,
                                 comm, request, module, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
    }

    return OMPI_SUCCESS;
}

int ompi_coll_libnbc_allgather_inter_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                                          MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                          struct mca_coll_base_module_2_3_0_t *module) {
    int res = nbc_allgather_inter_init(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype,
                                       comm, request, module, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
    }

    return OMPI_SUCCESS;
}
