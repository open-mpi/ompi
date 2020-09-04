/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2014-2018 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016-2017 IBM Corporation.  All rights reserved.
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

#define IBCAST_DEFAULT_RADIX 4
#define IBCAST_DEFAULT_SEGSIZE 16384

static inline int bcast_sched_binomial(int rank, int p, int root, NBC_Schedule *schedule, void *buffer, int count,
                                       MPI_Datatype datatype);
static inline int bcast_sched_linear(int rank, int p, int root, NBC_Schedule *schedule, void *buffer, int count,
                                     MPI_Datatype datatype);
static inline int bcast_sched_chain(int rank, int p, int root, NBC_Schedule *schedule, void *buffer, int count,
                                    MPI_Datatype datatype, int fragsize, size_t size);
static inline int bcast_sched_knomial(int rank, int comm_size, int root, NBC_Schedule *schedule, void *buf,
                                      int count, MPI_Datatype datatype, int knomial_radix);

static int libnbc_ibcast_knomial_radix;
static bool libnbc_ibcast_skip_dt_decision;

static mca_base_var_enum_value_t ibcast_algorithms[] = {
    {0, "ignore"},
    {1, "linear"},
    {2, "binomial"},
    {3, "chain"},
    {4, "knomial"},
    {0, NULL}
};

typedef enum { NBC_BCAST_LINEAR, NBC_BCAST_BINOMIAL, NBC_BCAST_CHAIN, NBC_BCAST_KNOMIAL } bcast_algorithm_t;

/* The following are used by dynamic and forced rules */

/* this routine is called by the component only */
/* module does not call this it calls the forced_getvalues routine instead */

int ompi_coll_libnbc_bcast_check_forced_init (void)
{
  mca_base_var_enum_t *new_enum;

  mca_coll_libnbc_component.forced_params[BCAST].algorithm = 0;
  (void) mca_base_var_enum_create("coll_libnbc_ibcast_algorithms", ibcast_algorithms, &new_enum);
  (void) mca_base_component_var_register(&mca_coll_libnbc_component.super.collm_version,
                                         "ibcast_algorithm",
                                         "Which ibcast algorithm is used: 0 ignore, 1 linear, 2 binomial, 3 chain, 4 knomial",
                                         MCA_BASE_VAR_TYPE_INT, new_enum, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                         OPAL_INFO_LVL_5,
                                         MCA_BASE_VAR_SCOPE_ALL,
                                         &mca_coll_libnbc_component.forced_params[BCAST].algorithm);

  mca_coll_libnbc_component.forced_params[BCAST].segsize = IBCAST_DEFAULT_SEGSIZE;
  mca_base_component_var_register(&mca_coll_libnbc_component.super.collm_version,
                                  "ibcast_algorithm_segmentsize",
                                  "Segment size in bytes used by default for ibcast algorithms. Only has meaning if algorithm is forced and supports segmenting. 0 bytes means no segmentation.",
                                  MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                  OPAL_INFO_LVL_5,
                                  MCA_BASE_VAR_SCOPE_ALL,
                                  &mca_coll_libnbc_component.forced_params[BCAST].segsize);

  libnbc_ibcast_knomial_radix = IBCAST_DEFAULT_RADIX;

  (void) mca_base_component_var_register(&mca_coll_libnbc_component.super.collm_version,
                                         "ibcast_knomial_radix", "k-nomial tree radix for the ibcast algorithm (radix > 1)",
                                         MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                         OPAL_INFO_LVL_9,
                                         MCA_BASE_VAR_SCOPE_READONLY,
                                         &libnbc_ibcast_knomial_radix);
  /* ibcast decision function can make the wrong decision if a legal
   * non-uniform data type signature is used. This has resulted in the
   * collective operation failing, and possibly producing wrong answers.
   * We are investigating a fix for this problem, but it is taking a while.
   *   https://github.com/open-mpi/ompi/issues/2256
   *   https://github.com/open-mpi/ompi/issues/1763
   * As a result we are adding an MCA parameter to make a conservative
   * decision to avoid this issue. If the user knows that their application
   * does not use data types in this way, then they can set this parameter
   * to get the old behavior. Once the issue is truely fixed, then this
   * parameter can be removed.
   */
  libnbc_ibcast_skip_dt_decision = true;
  (void) mca_base_component_var_register(&mca_coll_libnbc_component.super.collm_version,
                                         "ibcast_skip_dt_decision",
                                         "In ibcast only use size of communicator to choose algorithm, exclude data type signature. Set to 'false' to use data type signature in decision. WARNING: If you set this to 'false' then your application should not use non-uniform data type signatures in calls to ibcast.",
                                         MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                         OPAL_INFO_LVL_9,
                                         MCA_BASE_VAR_SCOPE_READONLY,
                                         &libnbc_ibcast_skip_dt_decision);


  OBJ_RELEASE(new_enum);
  return OMPI_SUCCESS;
}

#ifdef NBC_CACHE_SCHEDULE
/* tree comparison function for schedule cache */
int NBC_Bcast_args_compare(NBC_Bcast_args *a, NBC_Bcast_args *b, void *param) {
  if ((a->buffer == b->buffer) &&
      (a->count == b->count) &&
      (a->datatype == b->datatype) &&
      (a->root == b->root) ) {
    return 0;
  }

  if( a->buffer < b->buffer ) {
    return -1;
  }

  return 1;
}
#endif

static bcast_algorithm_t nbc_bcast_default_algorithm(int p, size_t size, int count,
                                                     int* segsize)
{
  bcast_algorithm_t alg;
  *segsize = IBCAST_DEFAULT_SEGSIZE;

  if( libnbc_ibcast_skip_dt_decision ) {
    if (p <= 4) {
      alg = NBC_BCAST_LINEAR;
    }
    else {
      alg = NBC_BCAST_BINOMIAL;
    }
  }
  else {
    if (p <= 4) {
      alg = NBC_BCAST_LINEAR;
    } else if (size * count < 65536) {
      alg = NBC_BCAST_BINOMIAL;
    } else if (size * count < 524288) {
      alg = NBC_BCAST_CHAIN;
      *segsize = 8192;
    } else {
      alg = NBC_BCAST_CHAIN;
      *segsize = 32768;
    }
  }
  return alg;
}

static int nbc_bcast_init(void *buffer, int count, MPI_Datatype datatype, int root,
                          struct ompi_communicator_t *comm, ompi_request_t ** request,
                          struct mca_coll_base_module_2_3_0_t *module, bool persistent)
{
  int rank, p, res, segsize, radix;
  size_t size;
  NBC_Schedule *schedule;
#ifdef NBC_CACHE_SCHEDULE
  NBC_Bcast_args *args, *found, search;
#endif
  bcast_algorithm_t alg;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

  rank = ompi_comm_rank (comm);
  p = ompi_comm_size (comm);

  if (1 == p) {
    return nbc_get_noop_request(persistent, request);
  }

  res = ompi_datatype_type_size(datatype, &size);
  if (MPI_SUCCESS != res) {
    NBC_Error("MPI Error in ompi_datatype_type_size() (%i)", res);
    return res;
  }

  if (libnbc_module->com_rules[BCAST]) {
    int algorithm, dummy;
    algorithm = ompi_coll_base_get_target_method_params (libnbc_module->com_rules[BCAST],
                                                         size * count, &radix, &segsize, &dummy);
    if (algorithm) {
      alg = algorithm - 1; /* -1 is to shift from algorithm ID to enum */
    } else {
      /* get default algorithm ID but keep our segsize value */
      alg = nbc_bcast_default_algorithm(p, size, count, &dummy);
    }
  } else if (0 != mca_coll_libnbc_component.forced_params[BCAST].algorithm) {
    alg = mca_coll_libnbc_component.forced_params[BCAST].algorithm - 1; /* -1 is to shift from algorithm ID to enum */
    segsize = mca_coll_libnbc_component.forced_params[BCAST].segsize;
    radix = libnbc_ibcast_knomial_radix;
  } else {
    alg = nbc_bcast_default_algorithm(p, size, count, &segsize);
    radix = libnbc_ibcast_knomial_radix;
  }

  if (NBC_BCAST_KNOMIAL == alg && radix <= 1) {
    alg = NBC_BCAST_LINEAR;
  }

  opal_output_verbose(10, mca_coll_libnbc_component.stream,
                      "Libnbc ibcast : algorithm %d segmentsize %d radix %d",
                      alg + 1, segsize, radix);

  if(0 == segsize) {
    segsize = count * size; /* only one frag */
  }

#ifdef NBC_CACHE_SCHEDULE
  /* search schedule in communicator specific tree */
  search.buffer = buffer;
  search.count = count;
  search.datatype = datatype;
  search.root = root;
  found = (NBC_Bcast_args *) hb_tree_search ((hb_tree *) libnbc_module->NBC_Dict[NBC_BCAST], &search);
  if (NULL == found) {
#endif
    schedule = OBJ_NEW(NBC_Schedule);
    if (OPAL_UNLIKELY(NULL == schedule)) {
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    switch(alg) {
      case NBC_BCAST_LINEAR:
        res = bcast_sched_linear(rank, p, root, schedule, buffer, count, datatype);
        break;
      case NBC_BCAST_BINOMIAL:
        res = bcast_sched_binomial(rank, p, root, schedule, buffer, count, datatype);
        break;
      case NBC_BCAST_CHAIN:
        res = bcast_sched_chain(rank, p, root, schedule, buffer, count, datatype, segsize, size);
        break;
      case NBC_BCAST_KNOMIAL:
        res = bcast_sched_knomial(rank, p, root, schedule, buffer, count, datatype, radix);
        break;
    }

    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      OBJ_RELEASE(schedule);
      return res;
    }

    res = NBC_Sched_commit (schedule);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      OBJ_RELEASE(schedule);
      return res;
    }

#ifdef NBC_CACHE_SCHEDULE
    /* save schedule to tree */
    args = (NBC_Bcast_args *) malloc (sizeof (args));
    if (NULL != args) {
      args->buffer = buffer;
      args->count = count;
      args->datatype = datatype;
      args->root = root;
      args->schedule = schedule;
      res = hb_tree_insert ((hb_tree *) libnbc_module->NBC_Dict[NBC_BCAST], args, args, 0);
      if (0 == res) {
        OBJ_RETAIN (schedule);

        /* increase number of elements for A2A */
        if (++libnbc_module->NBC_Dict_size[NBC_BCAST] > NBC_SCHED_DICT_UPPER) {
          NBC_SchedCache_dictwipe ((hb_tree *) libnbc_module->NBC_Dict[NBC_BCAST],
                                   &libnbc_module->NBC_Dict_size[NBC_BCAST]);
        }
      } else {
        NBC_Error("error in dict_insert() (%i)", res);
        free (args);
      }
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

int ompi_coll_libnbc_ibcast(void *buffer, int count, MPI_Datatype datatype, int root,
                            struct ompi_communicator_t *comm, ompi_request_t ** request,
                            struct mca_coll_base_module_2_3_0_t *module)
{
    int res = nbc_bcast_init(buffer, count, datatype, root,
                             comm, request, module, false);
    if (OPAL_LIKELY(OMPI_SUCCESS != res)) {
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

/* better binomial bcast
 * working principle:
 * - each node gets a virtual rank vrank
 * - the 'root' node get vrank 0
 * - node 0 gets the vrank of the 'root'
 * - all other ranks stay identical (they do not matter)
 *
 * Algorithm:
 * - each node with vrank > 2^r and vrank < 2^r+1 receives from node
 *   vrank - 2^r (vrank=1 receives from 0, vrank 0 receives never)
 * - each node sends each round r to node vrank + 2^r
 * - a node stops to send if 2^r > commsize
 */
#define RANK2VRANK(rank, vrank, root) \
{ \
  vrank = rank; \
  if (rank == 0) vrank = root; \
  if (rank == root) vrank = 0; \
}
#define VRANK2RANK(rank, vrank, root) \
{ \
  rank = vrank; \
  if (vrank == 0) rank = root; \
  if (vrank == root) rank = 0; \
}
static inline int bcast_sched_binomial(int rank, int p, int root, NBC_Schedule *schedule, void *buffer, int count, MPI_Datatype datatype) {
  int maxr, vrank, peer, res;

  maxr = (int)ceil((log((double)p)/LOG2));

  RANK2VRANK(rank, vrank, root);

  /* receive from the right hosts  */
  if (vrank != 0) {
    for (int r = 0 ; r < maxr ; ++r) {
      if ((vrank >= (1 << r)) && (vrank < (1 << (r + 1)))) {
        VRANK2RANK(peer, vrank - (1 << r), root);
        res = NBC_Sched_recv (buffer, false, count, datatype, peer, schedule, false);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          return res;
        }
      }
    }

    res = NBC_Sched_barrier (schedule);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
  }

  /* now send to the right hosts */
  for (int r = 0 ; r < maxr ; ++r) {
    if (((vrank + (1 << r) < p) && (vrank < (1 << r))) || (vrank == 0)) {
      VRANK2RANK(peer, vrank + (1 << r), root);
      res = NBC_Sched_send (buffer, false, count, datatype, peer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }
  }

  return OMPI_SUCCESS;
}

/* simple linear MPI_Ibcast */
static inline int bcast_sched_linear(int rank, int p, int root, NBC_Schedule *schedule, void *buffer, int count, MPI_Datatype datatype) {
  int res;

  /* send to all others */
  if(rank == root) {
    for (int peer = 0 ; peer < p ; ++peer) {
      if (peer != root) {
        /* send msg to peer */
        res = NBC_Sched_send (buffer, false, count, datatype, peer, schedule, false);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          return res;
        }
      }
    }
  } else {
    /* recv msg from root */
    res = NBC_Sched_recv (buffer, false, count, datatype, root, schedule, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      return res;
    }
  }

  return OMPI_SUCCESS;
}

/* simple chained MPI_Ibcast */
static inline int bcast_sched_chain(int rank, int p, int root, NBC_Schedule *schedule, void *buffer, int count, MPI_Datatype datatype, int fragsize, size_t size) {
  int res, vrank, rpeer, speer, numfrag, fragcount, thiscount;
  MPI_Aint ext;
  char *buf;

  RANK2VRANK(rank, vrank, root);
  VRANK2RANK(rpeer, vrank-1, root);
  VRANK2RANK(speer, vrank+1, root);
  res = ompi_datatype_type_extent(datatype, &ext);
  if (MPI_SUCCESS != res) {
    NBC_Error("MPI Error in ompi_datatype_type_extent() (%i)", res);
    return res;
  }

  if (count == 0) {
    return OMPI_SUCCESS;
  }

  numfrag = count * size/fragsize;
  if ((count * size) % fragsize != 0) {
    numfrag++;
  }

  fragcount = count/numfrag;

  for (int fragnum = 0 ; fragnum < numfrag ; ++fragnum) {
    buf = (char *) buffer + fragnum * fragcount * ext;
    thiscount = fragcount;
    if (fragnum == numfrag-1) {
      /* last fragment may not be full */
      thiscount = count - fragcount * fragnum;
    }

    /* root does not receive */
    if (vrank != 0) {
      res = NBC_Sched_recv (buf, false, thiscount, datatype, rpeer, schedule, true);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
    }

    /* last rank does not send */
    if (vrank != p-1) {
      res = NBC_Sched_send (buf, false, thiscount, datatype, speer, schedule, false);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }

      /* this barrier here seems awaward but isn't!!!! */
      if (vrank == 0)  {
        res = NBC_Sched_barrier (schedule);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          return res;
        }
      }
    }
  }

  return OMPI_SUCCESS;
}

/*
 * bcast_sched_knomial:
 *
 * Description: an implementation of Ibcast using k-nomial tree algorithm
 *
 * Time: (radix - 1)O(log_{radix}(comm_size))
 * Schedule length (rounds): O(log(comm_size))
 */
static inline int bcast_sched_knomial(
    int rank, int comm_size, int root, NBC_Schedule *schedule, void *buf,
    int count, MPI_Datatype datatype, int knomial_radix)
{
    int res = OMPI_SUCCESS;

    /* Receive from parent */
    int vrank = (rank - root + comm_size) % comm_size;
    int mask = 0x1;
    while (mask < comm_size) {
        if (vrank % (knomial_radix * mask)) {
            int parent = vrank / (knomial_radix * mask) * (knomial_radix * mask);
            parent = (parent + root) % comm_size;
            res = NBC_Sched_recv(buf, false, count, datatype, parent, schedule, true);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
            break;
        }
        mask *= knomial_radix;
    }
    mask /= knomial_radix;

    /* Send data to all children */
    while (mask > 0) {
        for (int r = 1; r < knomial_radix; r++) {
            int child = vrank + mask * r;
            if (child < comm_size) {
                child = (child + root) % comm_size;
                res = NBC_Sched_send(buf, false, count, datatype, child, schedule, false);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) { goto cleanup_and_return; }
            }
        }
        mask /= knomial_radix;
    }

cleanup_and_return:
    return res;
}

static int nbc_bcast_inter_init(void *buffer, int count, MPI_Datatype datatype, int root,
                                struct ompi_communicator_t *comm, ompi_request_t ** request,
                                struct mca_coll_base_module_2_3_0_t *module, bool persistent) {
  int res;
  NBC_Schedule *schedule;
  ompi_coll_libnbc_module_t *libnbc_module = (ompi_coll_libnbc_module_t*) module;

  schedule = OBJ_NEW(NBC_Schedule);
  if (OPAL_UNLIKELY(NULL == schedule)) {
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  if (root != MPI_PROC_NULL) {
    /* send to all others */
    if (root == MPI_ROOT) {
      int remsize;

      remsize = ompi_comm_remote_size (comm);

      for (int peer = 0 ; peer < remsize ; ++peer) {
        /* send msg to peer */
        res = NBC_Sched_send (buffer, false, count, datatype, peer, schedule, false);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          OBJ_RELEASE(schedule);
          return res;
        }
      }
    } else {
      /* recv msg from root */
      res = NBC_Sched_recv (buffer, false, count, datatype, root, schedule, false);
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

int ompi_coll_libnbc_ibcast_inter(void *buffer, int count, MPI_Datatype datatype, int root,
                                  struct ompi_communicator_t *comm, ompi_request_t ** request,
                                  struct mca_coll_base_module_2_3_0_t *module) {
    int res = nbc_bcast_inter_init(buffer, count, datatype, root,
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

int ompi_coll_libnbc_bcast_init(void *buffer, int count, MPI_Datatype datatype, int root,
                                struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                struct mca_coll_base_module_2_3_0_t *module) {
    int res = nbc_bcast_init(buffer, count, datatype, root,
                             comm, request, module, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
    }

    return OMPI_SUCCESS;
}

int ompi_coll_libnbc_bcast_inter_init(void *buffer, int count, MPI_Datatype datatype, int root,
                                      struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                      struct mca_coll_base_module_2_3_0_t *module) {
    int res = nbc_bcast_inter_init(buffer, count, datatype, root,
                                   comm, request, module, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
    }

    return OMPI_SUCCESS;
}
