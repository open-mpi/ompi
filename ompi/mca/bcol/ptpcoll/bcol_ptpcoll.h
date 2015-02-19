/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BCOL_PTPCOLL_EXPORT_H
#define MCA_BCOL_PTPCOLL_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"
#include "opal/mca/mca.h"
#include "ompi/mca/bcol/bcol.h"
#include "ompi/mca/bcol/base/base.h"
#include "opal/mca/mpool/mpool.h"
#include "ompi/request/request.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/patterns/net/netpatterns.h"

BEGIN_C_DECLS

#ifdef HAVE_SCHED_YIELD
#  include <sched.h>
#  define SPIN sched_yield()
#else  /* no switch available */
#  define SPIN
#endif

/** 
 * Structure to hold the basic shared memory coll component.  First it holds the
 * base coll component, and then holds a bunch of
 * sm-coll-component-specific stuff (e.g., current MCA param
 * values). 
 */
struct mca_bcol_ptpcoll_component_t {
    /** Base coll component */
    mca_bcol_base_component_2_0_0_t super;
    /** Verbosity level, used only in debug enabled builds */
    int verbose;
    /** The radix of K-nomial tree, initilized by mca parameter */
    int k_nomial_radix;
    /** The radix of narray tree, initilized by mca parameter */
    int narray_radix;
    /** The radix is used for narray scatther and knomail gather for 
      large message bcast **/
    int narray_knomial_radix;
    /** Number of times to poll for specific tag/src */
    int num_to_probe;
    /*
     * bcast small messages algorithm
     * 1 - Knomial bcast
     * 2 - Narray bcast
     */
    int bcast_small_messages_known_root_alg;
    /*
     * bcast large messages algorithm
     * 1 - binomial scatter-gather
     * 2 - Narray scatther, knomial gather
     */
    int bcast_large_messages_known_root_alg;
    /*
     * barrier algorithm
     * 1 - recursive doubling
     * 2 - recursive K-ing
     */
    int barrier_alg;

    int use_brucks_smsg_alltoall_rdma;
};

struct mca_bcol_ptpcoll_collreq_t {
    opal_free_list_item_t super;

    int tag;
    int num_reqs;
    int exchange;

    int need_toserv_extra;
    int extra_partner_rank;

    ompi_request_t **requests; 
};
typedef struct mca_bcol_ptpcoll_collreq_t mca_bcol_ptpcoll_collreq_t;
OBJ_CLASS_DECLARATION(mca_bcol_ptpcoll_collreq_t);

/**
 * Convenience typedef
 */
typedef struct mca_bcol_ptpcoll_component_t mca_bcol_ptpcoll_component_t;

/* Bcast small messages, 
   known root algorithm */
enum {
    PTPCOLL_KNOMIAL = 1,
    PTPCOLL_NARRAY
};

/* Bcast large messages, 
   known root algorithm */
enum {
    PTPCOLL_BINOMIAL_SG = 1,  /* Binomila scatter-gather */
    PTPCOLL_NARRAY_KNOMIAL_SG /* Narray-Knomial scatter-gather */
};

/* 
 * Implemented function index list 
 */

/* barrier */
enum{
    FANIN_FAN_OUT_BARRIER_FN,
    RECURSIVE_DOUBLING_BARRIER_FN,
    N_BARRIER_FNS
};

/* reduce */
enum{
    FANIN_REDUCE_FN,
    REDUCE_SCATTER_GATHER_FN,
    N_REDUCE_FNS
};
enum{
    SHORT_DATA_FN_REDUCE,
    LONG_DATA_FN_REDUCE,
    N_REDUCE_FNS_USED
};

/* all-reduce */
enum{
    FANIN_FANOUT_ALLREDUCE_FN,
    REDUCE_SCATTER_ALLGATHER_FN,
    N_ALLREDUCE_FNS
};
enum{
    SHORT_DATA_FN_ALLREDUCE,
    LONG_DATA_FN_ALLREDUCE,
    N_ALLREDUCE_FNS_USED
};


/*
 * N-order tree node description
 */
struct tree_node_t {
    /* my rank within the group */
    int my_rank;
    /* my node type - root, leaf, or interior */
    int my_node_type;
    /* number of nodes in the tree */
    int tree_size;
    /* number of parents (0/1) */
    int n_parents;
    /* number of children */
    int n_children;
    /* parent rank within the group */
    int parent_rank;
    /* chidren ranks within the group */
    int *children_ranks;
};
typedef struct tree_node_t tree_node_t;

struct pair_exchange_node_t {

    /* number of nodes this node will exchange data with */
    int n_exchanges;

    /* ranks of nodes involved in data exchnge */
    int *rank_exchanges;

    /* number of extra sources of data - outside largest power of 2 in
     *  this group */
    int n_extra_sources;

    /* rank of the extra source */
    int rank_extra_source;

    /* number of tags needed per stripe */
    int n_tags;

    /* log 2 of largest full power of 2 for this node set */
    int log_2;

    /* largest power of 2 that fits in this group */
    int n_largest_pow_2;

    /* node type */
    int node_type;

};
typedef struct pair_exchange_node_t pair_exchange_node_t;

/*
 * Barrier request objects
 */

/* enum for phase at which the nb barrier is in */
enum{
    NB_BARRIER_INACTIVE,
    NB_BARRIER_FAN_IN,
    NB_BARRIER_FAN_OUT,
    /* done and not started are the same for all practicle
     * purposes, as the init funtion always sets this flag
     */
    NB_BARRIER_DONE
};

typedef enum {
    PTPCOLL_NOT_STARTED         = 1,
    PTPCOLL_WAITING_FOR_DATA    = 1 << 1,
    PTPCOLL_SCATTER_STARTED     = 1 << 2,
    PTPCOLL_GATHER_STARTED      = 1 << 3,
    PTPCOLL_EXTRA_SEND_STARTED  = 1 << 4,
    PTPCOLL_ROOT_SEND_STARTED   = 1 << 5
} ptpcoll_op_status;

struct mca_bcol_ptpcoll_ml_buffer_desc_t {
    void     *data_addr;            /* buffer address */
    uint64_t     bank_index;        /* my bank */
    uint64_t     buffer_index;      /* my buff index */
    int       active_requests;   /* keep number of active requests */
    ompi_request_t **requests;      /* caching pointers to requests */
    int          data_src;          /* used for bcast to cache internal data */ 
    int          radix_mask;        /* used for bcast to cache internal data */ 
    int          radix_mask_pow;    /* used for bcast to cache internal data */ 
    int          iteration;         /* buffer iteration in knomial, binomail, etc. algorithms */
    int          tag;               /* tag number that is attached to this operation */
    int          status;       /* operation status */
    /* Fixme: Probably we can get rid of these fields by redesigning
     * the reduce implementation
     */
    int          reduction_status; /* used for reduction to cache internal
                                      reduction status */
    bool          reduce_init_called;
};
typedef struct mca_bcol_ptpcoll_ml_buffer_desc_t mca_bcol_ptpcoll_ml_buffer_desc_t;

/* 
 * Information that we need to keep in order to access and
 * track local ML memory that is used as source and destinatination
 * for collectives operations
 */
struct mca_bcol_ptpcoll_local_mlmem_desc_t {
    /* Bank index to release */
    uint32_t bank_index_for_release;
    /* number of memory banks */
    uint32_t     num_banks;
    /* number of buffers per bank */
    uint32_t     num_buffers_per_bank;
    /* size of a payload buffer */
    uint32_t     size_buffer;
    /* pointer to buffer descriptors initialized */
    mca_bcol_ptpcoll_ml_buffer_desc_t *ml_buf_desc;
};
typedef struct mca_bcol_ptpcoll_local_mlmem_desc_t mca_bcol_ptpcoll_local_mlmem_desc_t;

typedef enum {
    PTPCOLL_PROXY       = 1,
    PTPCOLL_IN_GROUP    = 1 << 1,
    PTPCOLL_EXTRA       = 1 << 2,
    PTPCOLL_KN_PROXY    = 1 << 3,
    PTPCOLL_KN_IN_GROUP = 1 << 4,
    PTPCOLL_KN_EXTRA    = 1 << 5
} node_type_pow2;

struct mca_bcol_ptpcoll_module_t {
    /* base structure */
    mca_bcol_base_module_t super;

    /* size */
    int group_size;

    /* size of each memory segment */
    size_t segment_size;

    /* k_nomial radix */
    int k_nomial_radix;
    /* caching power of K, for K-nomial operations */
    int pow_k;
    /* caching power of K number that is smaller or equal to size of group */
    int pow_knum;
    /* caching power of 2, it is special case for some algorithms */
    int pow_2;
    /* caching power of 2 number that is closet to size of group */
    int pow_2num;
    /* type of this node in group of power 2 */
    int pow_2type;
    /* type of this node in group of K-nomaial tree */
    int pow_ktype;
    /* type of this node in group of narray tree */
    int narray_type;
    /* size of full narray tree */
    int full_narray_tree_size;
    /* num leafs on last level */
    int full_narray_tree_num_leafs;

    /* Nary tree info */
    netpatterns_tree_node_t *narray_node;

    /* if the rank in group, it keeps the extra peer. 
       if the rank is extra, it keeps the proxy peer.
     */
    int proxy_extra_index;    /* pow2 algorithm */
    int *kn_proxy_extra_index; /* K nomaila algorithm */
    int kn_proxy_extra_num; /* number of extra peers , maximum k - 1*/

    /* collective tag */
    long long collective_tag;

    /* tag mask - the pml has a limit on tag size, so need
     * to wrap around
     */
    uint64_t tag_mask;

    /* Caching information about local ml memory.
     * Since ptpcoll does not support RDMA operations over pml,
     * we don't need to keep any information about remote buffers
     */
    mca_bcol_ptpcoll_local_mlmem_desc_t ml_mem;


    /* Narray-Knomial scatther gather */

    /* list of extra indexes */
    int *narray_knomial_proxy_extra_index;
    /* number of extra peers , maximum k - 1*/
    int narray_knomial_proxy_num; 
    /* Narray-Knomial node information array */
    netpatterns_narray_knomial_tree_node_t *narray_knomial_node;
    /* Knomial exchange tree */ 
    netpatterns_k_exchange_node_t knomial_exchange_tree;
    /* knomial allgather tree --- Do not disable, we need both 
       different algorithms define recursive k - ing differently
     */
    netpatterns_k_exchange_node_t knomial_allgather_tree;

	/* Knomial allgather offsets */
	int **allgather_offsets;

    /* Free lists of outstanding collective operations */
    opal_free_list_t collreqs_free;

    int log_group_size;
    struct iovec *alltoall_iovec;
};

typedef struct mca_bcol_ptpcoll_module_t mca_bcol_ptpcoll_module_t;
OBJ_CLASS_DECLARATION(mca_bcol_ptpcoll_module_t);


/**
 * Global component instance
 */
OMPI_MODULE_DECLSPEC extern mca_bcol_ptpcoll_component_t 
mca_bcol_ptpcoll_component;


/*
 * coll module functions
 */

/* query to see if the component is available for use, and can
 * satisfy the thread and progress requirements
 */
int mca_bcol_ptpcoll_init_query(bool enable_progress_threads,
        bool enable_mpi_threads);

/* query to see if the module is available for use on the given
 * communicator, and if so, what it's priority is.
 */
mca_bcol_base_module_t **
mca_bcol_ptpcoll_comm_query(mca_sbgp_base_module_t *sbgp, int *num_modules);

/* interface function to setup recursive k-ing tree */
int mca_bcol_ptpcoll_setup_knomial_tree(mca_bcol_base_module_t *super);

/* barrier routines */
int bcol_ptpcoll_barrier_recurs_dbl(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args);
int bcol_ptpcoll_barrier_recurs_knomial(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args);
int bcol_ptpcoll_barrier_init(mca_bcol_base_module_t *super);
int mca_bcol_ptpcoll_memsync_init(mca_bcol_base_module_t *super);
void * bcol_ptpcoll_allocate_memory(size_t length, size_t alignment, 
        struct mca_bcol_base_module_t *bcol_module);
int bcol_ptpcoll_register_memory(void * in_ptr, size_t length, size_t alignment,
        struct mca_bcol_base_module_t *bcol_module);
int bcol_ptpcoll_deregister_memory( void * in_ptr,
        struct mca_bcol_base_module_t *bcol_module);
int bcol_ptpcoll_free_memory(void *ptr,
        struct mca_bcol_base_module_t *bcol_module);
int bcol_ptpcoll_fanin( bcol_function_args_t *input_args,
        struct mca_bcol_base_module_t *module);
int bcol_ptpcoll_fanout( bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args);


/* allgather routine */
int bcol_ptpcoll_k_nomial_allgather_init(bcol_function_args_t *input_args,
                        struct mca_bcol_base_function_t *const_args);

/* allgather progress */
int bcol_ptpcoll_k_nomial_allgather_progress(bcol_function_args_t *input_args,
                        struct mca_bcol_base_function_t *const_args);
/* allgather register */
int bcol_ptpcoll_allgather_init(mca_bcol_base_module_t *super);

static inline __opal_attribute_always_inline__ 
        int mca_bcol_ptpcoll_test_for_match(ompi_request_t **request , int *rc)
{
    int matched = 0;
    int i;
    mca_bcol_ptpcoll_component_t *cm = &mca_bcol_ptpcoll_component;
    *rc = OMPI_SUCCESS;

    for (i = 0; i < cm->num_to_probe &&
             0 == matched && OMPI_SUCCESS == *rc ; i++) {
        *rc = ompi_request_test(request, &matched, MPI_STATUS_IGNORE);
    }

    return matched;
}

static inline __opal_attribute_always_inline__ 
        int mca_bcol_ptpcoll_test_all_for_match(int *n_requests, ompi_request_t **requests , int *rc)
{
    int matched = 0;
    int i;
    mca_bcol_ptpcoll_component_t *cm = &mca_bcol_ptpcoll_component;
    *rc = OMPI_SUCCESS;

    assert(*n_requests >= 0);

    if (0 == *n_requests) {
        return 1;
    }

    for (i = 0; i < cm->num_to_probe &&
            0 == matched && OMPI_SUCCESS == *rc; i++) {
        *rc = ompi_request_test_all
            (*n_requests, requests, &matched, MPI_STATUS_IGNORE);
    }

    if (matched) {
        *n_requests = 0;
    }

    return matched;
}

/* Some negative tags already used by OMPI, making sure that we take safe offset */
#define PTPCOLL_TAG_OFFSET 100
#define PTPCOLL_TAG_FACTOR 2

static inline int lognum(int n){
	int count = 1, lognum = 0;

	while (count < n) {
		count = count << 1;
		lognum++;
	}
	return lognum;
}

END_C_DECLS

#endif /* MCA_BCOL_PTPCOLL_EXPORT_H */
