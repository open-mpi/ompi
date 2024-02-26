/*
 * Copyright (c) 2018-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2022      IBM Corporation. All rights reserved
 * Copyright (c) 2020-2022 Bull S.A.S. All rights reserved.
 * Copyright (c)           Amazon.com, Inc. or its affiliates.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * This component provides hierarchical implementations of MPI collectives.
 * Hierarchical approach is efficient in case of too many process wanting a remote
 * access to the same local or remote resource (high message rate).
 * Some components are also better at local scale (for example with shared memory)
 * where others provide scalable implementations. Hierarchical implementation
 * enable a fallback on other components for intermediary operation.
 * For example a MPI_Bcast will be divided into a sequence of bcasts from the
 * highest to the lowest topological level.
 * Some algorithms introduce more advanced feature (such as noise resiliency)
 * some just link topological levels. The last ones are called 'simple'.
 * To perform sub-communications, extra communicators are initialised for
 * each topological level.
 */


#ifndef MCA_COLL_HAN_EXPORT_H
#define MCA_COLL_HAN_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/mca/mca.h"
#include "opal/util/output.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "coll_han_trigger.h"
#include "ompi/mca/coll/han/coll_han_dynamic.h"
#include "coll_han_algorithms.h"

/*
 * Today;
 * . only 2 modules available for intranode (low) level
 * . only 2 modules available for internode (up) level
 */

#define COLL_HAN_LOW_MODULES 2
#define COLL_HAN_UP_MODULES 2

struct mca_coll_han_bcast_args_s {
    mca_coll_task_t *cur_task;
    ompi_communicator_t *up_comm;
    ompi_communicator_t *low_comm;
    void *buff;
    ompi_datatype_t *dtype;
    int seg_count;
    int root_low_rank;
    int root_up_rank;
    int num_segments;
    int cur_seg;
    int w_rank;
    int last_seg_count;
    bool noop;
};
typedef struct mca_coll_han_bcast_args_s mca_coll_han_bcast_args_t;

struct mca_coll_han_reduce_args_s {
    mca_coll_task_t *cur_task;
    ompi_communicator_t *up_comm;
    ompi_communicator_t *low_comm;
    void *sbuf;
    void *rbuf;
    ompi_op_t *op;
    ompi_datatype_t *dtype;
    int seg_count;
    int root_low_rank;
    int root_up_rank;
    int num_segments;
    int cur_seg;
    int w_rank;
    int last_seg_count;
    bool noop;
    bool is_tmp_rbuf;
};
typedef struct mca_coll_han_reduce_args_s mca_coll_han_reduce_args_t;

struct mca_coll_han_allreduce_args_s {
    mca_coll_task_t *cur_task;
    ompi_communicator_t *up_comm;
    ompi_communicator_t *low_comm;
    ompi_request_t *req;
    void *sbuf;
    void *rbuf;
    ompi_op_t *op;
    ompi_datatype_t *dtype;
    int seg_count;
    int root_up_rank;
    int root_low_rank;
    int num_segments;
    int cur_seg;
    int w_rank;
    int last_seg_count;
    bool noop;
    int *completed;
};
typedef struct mca_coll_han_allreduce_args_s mca_coll_han_allreduce_args_t;

struct mca_coll_han_scatter_args_s {
    mca_coll_task_t *cur_task;
    ompi_communicator_t *up_comm;
    ompi_communicator_t *low_comm;
    ompi_request_t *req;
    void *sbuf;
    void *sbuf_inter_free;
    void *sbuf_reorder_free;
    void *rbuf;
    ompi_datatype_t *sdtype;
    ompi_datatype_t *rdtype;
    int scount;
    int rcount;
    int root;
    int root_up_rank;
    int root_low_rank;
    int w_rank;
    bool noop;
};
typedef struct mca_coll_han_scatter_args_s mca_coll_han_scatter_args_t;

struct mca_coll_han_gather_args_s {
    mca_coll_task_t *cur_task;
    ompi_communicator_t *up_comm;
    ompi_communicator_t *low_comm;
    ompi_request_t *req;
    void *sbuf;
    void *sbuf_inter_free;
    void *rbuf;
    ompi_datatype_t *sdtype;
    ompi_datatype_t *rdtype;
    int scount;
    int rcount;
    int root;
    int root_up_rank;
    int root_low_rank;
    int w_rank;
    bool noop;
    bool is_mapbycore;
};
typedef struct mca_coll_han_gather_args_s mca_coll_han_gather_args_t;

struct mca_coll_han_allgather_s {
    mca_coll_task_t *cur_task;
    ompi_communicator_t *up_comm;
    ompi_communicator_t *low_comm;
    ompi_request_t *req;
    void *sbuf;
    void *sbuf_inter_free;
    void *rbuf;
    ompi_datatype_t *sdtype;
    ompi_datatype_t *rdtype;
    int scount;
    int rcount;
    int root_low_rank;
    int w_rank;
    bool noop;
    bool is_mapbycore;
    int *topo;
};
typedef struct mca_coll_han_allgather_s mca_coll_han_allgather_t;

typedef struct mca_coll_han_op_up_low_module_name_t {
    char* han_op_up_module_name;
    char* han_op_low_module_name;
} mca_coll_han_op_up_low_module_name_t;

/**
 * The only reason we need to keep these around is because our MCA system does
 * not support MCA variables that do not point to existing variables (aka. where
 * mbv_storage does not exists until the completion of the application). Thus,
 * we need to keep track of the storage for all variables, even the ones we
 * only use to translated into a string.
 */
typedef struct mca_coll_han_op_module_name_t {
    mca_coll_han_op_up_low_module_name_t bcast;
    mca_coll_han_op_up_low_module_name_t reduce;
    mca_coll_han_op_up_low_module_name_t allreduce;
    mca_coll_han_op_up_low_module_name_t allgather;
    mca_coll_han_op_up_low_module_name_t gather;
    mca_coll_han_op_up_low_module_name_t gatherv;
    mca_coll_han_op_up_low_module_name_t scatter;
} mca_coll_han_op_module_name_t;

/**
 * Structure to hold the han coll component.  First it holds the
 * base coll component, and then holds a bunch of
 * han-coll-component-specific stuff (e.g., current MCA param
 * values).
 */
typedef struct mca_coll_han_component_t {
    /** Base coll component */
    mca_coll_base_component_2_4_0_t super;

    /** MCA parameter: Priority of this component */
    int han_priority;
    /* whether output the log message */
    int han_output;
    int han_output_verbose; /* activation level of coll han verbosity */
    /* segment size for bcast */
    uint32_t han_bcast_segsize;
    /* up level module for bcast */
    uint32_t han_bcast_up_module;
    /* low level module for bcast */
    uint32_t han_bcast_low_module;
    /* segment size for reduce */
    uint32_t han_reduce_segsize;
    /* up level module for reduce */
    uint32_t han_reduce_up_module;
    /* low level module for reduce */
    uint32_t han_reduce_low_module;
    /* segment size for allreduce */
    uint32_t han_allreduce_segsize;
    /* up level module for allreduce */
    uint32_t han_allreduce_up_module;
    /* low level module for allreduce */
    uint32_t han_allreduce_low_module;
    /* up level module for allgather */
    uint32_t han_allgather_up_module;
    /* low level module for allgather */
    uint32_t han_allgather_low_module;
    /* up level module for gather */
    uint32_t han_gather_up_module;
    /* low level module for gather */
    uint32_t han_gather_low_module;
    /* up level module for gatherv */
    uint32_t han_gatherv_up_module;
    /* low level module for gatherv */
    uint32_t han_gatherv_low_module;
    /* up level module for scatter */
    uint32_t han_scatter_up_module;
    /* low level module for scatter */
    uint32_t han_scatter_low_module;
    /* name of the modules */
    mca_coll_han_op_module_name_t han_op_module_name;
    /* whether we need reproducible results
     * (but disables topological optimisations)
     */
    bool han_reproducible;
    bool use_simple_algorithm[COLLCOUNT];
    int use_algorithm[COLLCOUNT];
    int use_algorithm_param[COLLCOUNT]; // MCA parmeter id for algo, to know if user provided

    /* Dynamic configuration rules */
    bool use_dynamic_file_rules;
    bool dump_dynamic_rules;
    char* dynamic_rules_filename;
    /* Dynamic rules from file */
    mca_coll_han_dynamic_rules_t dynamic_rules;
    /* Dynamic rules from mca parameter */
    COMPONENT_T mca_sub_components[COLLCOUNT][NB_TOPO_LVL];

    int num_available_algorithms[COLLCOUNT]; // not counting "default" behaviour
    /* to show algorithms in ompi_info */
    mca_base_var_enum_value_t* algorithm_enumerator[COLLCOUNT];

    /* Define maximum dynamic errors printed by rank 0 with a 0 verbosity level */
    int max_dynamic_errors;
} mca_coll_han_component_t;


/*
 * Structure used to store what is necessary for the collective operations
 * routines in case of fallback.
 */
typedef struct mca_coll_han_single_collective_fallback_s {
    union {
        mca_coll_base_module_allgather_fn_t allgather;
        mca_coll_base_module_allgatherv_fn_t allgatherv;
        mca_coll_base_module_allreduce_fn_t allreduce;
        mca_coll_base_module_barrier_fn_t barrier;
        mca_coll_base_module_bcast_fn_t bcast;
        mca_coll_base_module_gather_fn_t gather;
        mca_coll_base_module_gatherv_fn_t gatherv;
        mca_coll_base_module_reduce_fn_t reduce;
        mca_coll_base_module_scatter_fn_t scatter;
    } module_fn;
    mca_coll_base_module_t* module;
} mca_coll_han_single_collective_fallback_t;

/*
 * The structure containing a replacement for all collective supported
 * by HAN. This structure is used as a fallback during subcommunicator
 * creation.
 */
typedef struct mca_coll_han_collectives_fallback_s {
    mca_coll_han_single_collective_fallback_t allgather;
    mca_coll_han_single_collective_fallback_t allgatherv;
    mca_coll_han_single_collective_fallback_t allreduce;
    mca_coll_han_single_collective_fallback_t barrier;
    mca_coll_han_single_collective_fallback_t bcast;
    mca_coll_han_single_collective_fallback_t reduce;
    mca_coll_han_single_collective_fallback_t gather;
    mca_coll_han_single_collective_fallback_t gatherv;
    mca_coll_han_single_collective_fallback_t scatter;
} mca_coll_han_collectives_fallback_t;

/** Coll han module */
typedef struct mca_coll_han_module_t {
    /** Base module */
    mca_coll_base_module_t super;

    /* Whether this module has been lazily initialized or not yet */
    bool enabled;
    int recursive_free_depth;

    struct ompi_communicator_t *cached_comm;
    struct ompi_communicator_t **cached_low_comms;
    struct ompi_communicator_t **cached_up_comms;
    int *cached_vranks;
    int *cached_topo;
    bool is_mapbycore;
    bool are_ppn_imbalanced;

    /* To be able to fallback when the cases are not supported */
    struct mca_coll_han_collectives_fallback_s fallback;

    /* To be able to fallback on reproducible algorithm */
    mca_coll_base_module_reduce_fn_t reproducible_reduce;
    mca_coll_base_module_t *reproducible_reduce_module;
    mca_coll_base_module_allreduce_fn_t reproducible_allreduce;
    mca_coll_base_module_t *reproducible_allreduce_module;

    /* Topological level of this communicator */
    TOPO_LVL_T topologic_level;

    /* Collective module storage for module choice */
    mca_coll_han_collective_modules_storage_t modules_storage;
    bool storage_initialized;

    /*
     * Number of dynamic errors encountered
     * The first mca_coll_han_component.max_dynamic_errors
     * of rank 0 are printed with verbosity = 0
     */
    int dynamic_errors;

    /* Sub-communicator */
    struct ompi_communicator_t *sub_comm[NB_TOPO_LVL];
} mca_coll_han_module_t;
OBJ_CLASS_DECLARATION(mca_coll_han_module_t);

/*
 * Some defines to stick to the naming used in the other components in terms of
 * fallback routines
 */
#define previous_allgather          fallback.allgather.module_fn.allgather
#define previous_allgather_module   fallback.allgather.module

#define previous_allgatherv         fallback.allgatherv.module_fn.allgatherv
#define previous_allgatherv_module  fallback.allgatherv.module

#define previous_allreduce          fallback.allreduce.module_fn.allreduce
#define previous_allreduce_module   fallback.allreduce.module

#define previous_barrier            fallback.barrier.module_fn.barrier
#define previous_barrier_module     fallback.barrier.module

#define previous_bcast              fallback.bcast.module_fn.bcast
#define previous_bcast_module       fallback.bcast.module

#define previous_reduce             fallback.reduce.module_fn.reduce
#define previous_reduce_module      fallback.reduce.module

#define previous_gather             fallback.gather.module_fn.gather
#define previous_gather_module      fallback.gather.module

#define previous_gatherv            fallback.gatherv.module_fn.gatherv
#define previous_gatherv_module     fallback.gatherv.module

#define previous_scatter            fallback.scatter.module_fn.scatter
#define previous_scatter_module     fallback.scatter.module


/* macro to correctly load a fallback collective module */
#define HAN_LOAD_FALLBACK_COLLECTIVE(HANM, COMM, COLL)                            \
    do {                                                                          \
        if ( ((COMM)->c_coll->coll_ ## COLL ## _module) == (mca_coll_base_module_t*)(HANM) ) { \
            (COMM)->c_coll->coll_ ## COLL = (HANM)->previous_## COLL;               \
            mca_coll_base_module_t *coll_module = (COMM)->c_coll->coll_ ## COLL ## _module; \
            (COMM)->c_coll->coll_ ## COLL ## _module = (HANM)->previous_ ## COLL ## _module;  \
            OBJ_RETAIN((COMM)->c_coll->coll_ ## COLL ## _module);                     \
            OBJ_RELEASE(coll_module);                                                 \
        }                                                                             \
    } while(0)

/* macro to correctly load /all/ fallback collectives */
#define HAN_LOAD_FALLBACK_COLLECTIVES(HANM, COMM)                            \
    do {                                                                     \
        HAN_LOAD_FALLBACK_COLLECTIVE(HANM, COMM, barrier);                   \
        HAN_LOAD_FALLBACK_COLLECTIVE(HANM, COMM, bcast);                     \
        HAN_LOAD_FALLBACK_COLLECTIVE(HANM, COMM, scatter);                   \
        HAN_LOAD_FALLBACK_COLLECTIVE(HANM, COMM, gather);                    \
        HAN_LOAD_FALLBACK_COLLECTIVE(HANM, COMM, gatherv);                    \
        HAN_LOAD_FALLBACK_COLLECTIVE(HANM, COMM, reduce);                    \
        HAN_LOAD_FALLBACK_COLLECTIVE(HANM, COMM, allreduce);                 \
        HAN_LOAD_FALLBACK_COLLECTIVE(HANM, COMM, allgather);                 \
        HAN_LOAD_FALLBACK_COLLECTIVE(HANM, COMM, allgatherv);                \
        han_module->enabled = false;  /* entire module set to pass-through from now on */ \
    } while(0)


/**
 * Global component instance
 */
OMPI_DECLSPEC extern mca_coll_han_component_t mca_coll_han_component;

/*
 * coll module functions
 */
int mca_coll_han_init_query(bool enable_progress_threads, bool enable_mpi_threads);

mca_coll_base_module_t *mca_coll_han_comm_query(struct ompi_communicator_t *comm, int *priority);

int ompi_coll_han_request_free(ompi_request_t ** request);

/* Subcommunicator creation */
int mca_coll_han_comm_create(struct ompi_communicator_t *comm, mca_coll_han_module_t * han_module);
int mca_coll_han_comm_create_new(struct ompi_communicator_t *comm, mca_coll_han_module_t *han_module);

/**
 * Gather topology information
 *
 * Returns a pointer to the (potentially already cached) topology.
 * NOTE: if the rank distribution is imbalanced, no effort will be made to gather
 *       the topology at all ranks and instead NULL is returned and han_module->is_mapbycore
 *       is set to false.
 *       If HAN ever learns to deal with imbalanced topologies, this needs fixing!
 */
int *mca_coll_han_topo_init(struct ompi_communicator_t *comm, mca_coll_han_module_t * han_module,
                            int num_topo_level);

/* Utils */
static inline void
mca_coll_han_get_ranks(int *vranks, int w_rank, int low_size,
                       int *low_rank, int *up_rank)
{
    if (up_rank) {
        *up_rank = vranks[w_rank] / low_size;
    }

    if (low_rank) {
        *low_rank = vranks[w_rank] % low_size;
    }
}

const char* mca_coll_han_topo_lvl_to_str(TOPO_LVL_T topo_lvl);

/** Dynamic component choice */
/*
 * Get all the collective modules initialized on this communicator
 * This function must be call at the start of every selector implementation
 */
int
mca_coll_han_get_all_coll_modules(struct ompi_communicator_t *comm,
                                  mca_coll_han_module_t *han_module);

int
mca_coll_han_allgather_intra_dynamic(ALLGATHER_BASE_ARGS,
                                     mca_coll_base_module_t *module);
int
mca_coll_han_allgatherv_intra_dynamic(ALLGATHERV_BASE_ARGS,
                                      mca_coll_base_module_t *module);
int
mca_coll_han_allreduce_intra_dynamic(ALLREDUCE_BASE_ARGS,
                                     mca_coll_base_module_t *module);
int
mca_coll_han_barrier_intra_dynamic(BARRIER_BASE_ARGS,
                                 mca_coll_base_module_t *module);
int
mca_coll_han_bcast_intra_dynamic(BCAST_BASE_ARGS,
                                 mca_coll_base_module_t *module);
int
mca_coll_han_gather_intra_dynamic(GATHER_BASE_ARGS,
                                  mca_coll_base_module_t *module);
int
mca_coll_han_gatherv_intra_dynamic(GATHERV_BASE_ARGS,
                                   mca_coll_base_module_t *module);
int
mca_coll_han_reduce_intra_dynamic(REDUCE_BASE_ARGS,
                                  mca_coll_base_module_t *module);
int
mca_coll_han_scatter_intra_dynamic(SCATTER_BASE_ARGS,
                                   mca_coll_base_module_t *module);

int mca_coll_han_barrier_intra_simple(struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module);

/* reordering after gather, for unordered ranks */
void
ompi_coll_han_reorder_gather(const void *sbuf,
                             void *rbuf, int rcount,
                             struct ompi_datatype_t *rdtype,
                             struct ompi_communicator_t *comm,
                             int * topo);

size_t
coll_han_utils_gcd(const size_t *numerators, const size_t size);

int
coll_han_utils_create_contiguous_datatype(size_t count, const ompi_datatype_t *oldType,
                                          ompi_datatype_t **newType);
#endif                          /* MCA_COLL_HAN_EXPORT_H */
