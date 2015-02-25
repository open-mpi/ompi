/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file */

#ifndef MCA_COLL_ML_ML_H
#define MCA_COLL_ML_ML_H

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/threads/mutex.h"

#include "ompi/mca/coll/coll.h"
#include "ompi/request/request.h"
#include "ompi/mca/bcol/bcol.h"
#include "ompi/mca/sbgp/sbgp.h"
#include "ompi/op/op.h"
#include "opal/class/opal_free_list.h"

#include "coll_ml_lmngr.h"
#include "coll_ml_functions.h"
#include "coll_ml_colls.h"
#include "coll_ml_allocation.h"
#include "coll_ml_config.h"

BEGIN_C_DECLS

/* macros for return status */
enum {
    ML_OMPI_COMPLETE = 1,
    ML_OMPI_INCOMPLETE
};

enum {
    ML_SMALL_MSG,
    ML_LARGE_MSG,
    ML_NUM_MSG
};

/* ML collectives IDs */
enum {
    /* blocking functions */
    ML_ALLGATHER,
    ML_ALLGATHERV,
    ML_ALLREDUCE,
    ML_ALLTOALL,
    ML_ALLTOALLV,
    ML_ALLTOALLW,
    ML_BARRIER,
    ML_BCAST,
    ML_EXSCAN,
    ML_GATHER,
    ML_GATHERV,
    ML_REDUCE,
    ML_REDUCE_SCATTER,
    ML_SCAN,
    ML_SCATTER,
    ML_SCATTERV,
    ML_FANIN,
    ML_FANOUT,

    /* nonblocking functions */
    ML_IALLGATHER,
    ML_IALLGATHERV,
    ML_IALLREDUCE,
    ML_IALLTOALL,
    ML_IALLTOALLV,
    ML_IALLTOALLW,
    ML_IBARRIER,
    ML_IBCAST,
    ML_IEXSCAN,
    ML_IGATHER,
    ML_IGATHERV,
    ML_IREDUCE,
    ML_IREDUCE_SCATTER,
    ML_ISCAN,
    ML_ISCATTER,
    ML_ISCATTERV,
    ML_IFANIN,
    ML_IFANOUT,
    ML_NUM_OF_FUNCTIONS
};

/* ML broadcast algorithms */
enum {
    COLL_ML_STATIC_BCAST,
    COLL_ML_SEQ_BCAST,
    COLL_ML_UNKNOWN_BCAST,
};

struct mca_bcol_base_module_t;

/* collective function arguments - gives
 * one function signature for calling all collective setup
 * routines, with the initial call to a collective function having
 * the context to access the right parts of the data structure.
 * this information is used by each of the setup functions to
 * setup the correct information for each of the functions in the
 * hierarchy that will be called. */

/* RLG NOTE:  Need to figure out what arguments to store here,
 *   and which ones directly in the message descriptor
 */
struct mpi_coll_fn_params_t {
    union {
        struct {
            ompi_communicator_t *comm;
            int n_fanin_steps;
            int n_fanout_steps;
            int n_recursive_doubling_steps;
        } ibarrier_recursive_doubling;

        struct {
            int root;
            ompi_communicator_t *comm;
            struct ompi_datatype_t *datatype;
        } ibcast;
    } coll_fn;
};
typedef struct mpi_coll_fn_params_t mpi_coll_fn_params_t;

/* algorithm parmeters needed for the setup function */
struct mpi_coll_algorithm_params_t {
    union {
        struct {
            int n_fanin_steps;
            int n_fanout_steps;
            int n_recursive_doubling_steps;
        } ibarrier_recursive_doubling;

        struct {
            int place_holder;
        } ibcast;
    } coll_fn;
};
typedef struct mpi_coll_algorithm_params_t mpi_coll_algorithm_params_t;

/* setup function - used to setup each segment (or fragment)
 * to be processed
 */
struct mca_coll_ml_module_t;
struct mca_coll_ml_topology_t;

typedef int (*coll_fragment_comm_setup_fn)(struct mca_coll_ml_module_t *ml_module,
    mpi_coll_fn_params_t *fn_params, mpi_coll_algorithm_params_t *algorithm_params);
/* full collective description */
struct coll_ml_collective_description_t {
    /* number of temp buffers */
    int n_buffers;

    /* description size */
    int n_functions;

    /* collective setup function - called for every non-blocking
     * function, and for each fragment of such a message
     */
    coll_fragment_comm_setup_fn *coll_fn_setup_fn;

    /* algorithm parameters */
    mpi_coll_algorithm_params_t alg_params;

    /* list of functions */
    mca_bcol_base_function_t *functions;

    /* function names - for debugging */
    char **function_names;

    /* Signalling collective completion */
    bool completion_flag;
};

typedef struct coll_ml_collective_description_t coll_ml_collective_description_t;

/* Utility data structure */
struct rank_properties_t {
    int rank;
    int leaf;
    int num_of_ranks_represented;
}; typedef struct rank_properties_t rank_properties_t;

/* data structure for holding node information for the nodes of the
 * hierarchical communications tree.
 */
struct sub_group_params_t {
    /* rank of root in the communicator */
    int root_rank_in_comm;

    /* index in subgroup */
    int root_index;

    /* number of ranks in subgroup */
    int n_ranks;

    /* index of the first element in the subgroup.  The
     * assumption is that
     * ranks for all subgroups are stored in a single
     * linear array
     */
    int index_of_first_element;

    /*
     * level in the hierarchy - subgroups at the same
     * level don't overlap. May not be the same as the
     * sbgp level.
     */
    int level_in_hierarchy;

    /*
     * Information on the ranks in the subgroup.  This includes
     * the rank, and wether or not the rank is a source/sink of
     * of data in this subgroup, or just a "pass through".
     */
    rank_properties_t *rank_data;

    /* level one index - for example,
       for( i = 0; i < level_one_index; i++) will loop
       through all level one subgroups, this is significant
       since level one is a disjoint partitioning of all ranks
       i.e. all ranks appear once and only once at level one
     */
    int level_one_index;
};
typedef struct sub_group_params_t sub_group_params_t;

/* function to setup information on the order of a given bcol within
 * a specific ML-level algorithm.
 */
int mca_coll_ml_setup_scratch_vals(mca_coll_ml_compound_functions_t *func_list,
                int *scratch_indx, int *scratch_num, int n_hiers);

/* driver for setting up collective communication description */

int ml_coll_schedule_setup(struct mca_coll_ml_module_t *ml_module);

int ml_coll_up_and_down_hier_setup(
        struct mca_coll_ml_module_t *ml_module,
        struct mca_coll_ml_topology_t *topo_info,
        int up_function_idx,
        int top_function_idx,
        int down_function_idx,
        int collective);

int ml_coll_barrier_constant_group_data_setup(
        struct mca_coll_ml_topology_t *topo_info,
        mca_coll_ml_collective_operation_description_t  *schedule);

/* Barrier */
int ml_coll_hier_barrier_setup(struct mca_coll_ml_module_t *ml_module);

/* allreduce */
int ml_coll_hier_allreduce_setup(struct mca_coll_ml_module_t *ml_module);
int ml_coll_hier_allreduce_setup_new(struct mca_coll_ml_module_t *ml_module);
void ml_coll_hier_allreduce_cleanup_new(struct mca_coll_ml_module_t *ml_module);

/* alltoall */
int ml_coll_hier_alltoall_setup(struct mca_coll_ml_module_t *ml_module);
int ml_coll_hier_alltoall_setup_new(struct mca_coll_ml_module_t *ml_module);

/* allgather */
int ml_coll_hier_allgather_setup(struct mca_coll_ml_module_t *ml_module);
void ml_coll_hier_allgather_cleanup(struct mca_coll_ml_module_t *ml_module);

/* gather */
int ml_coll_hier_gather_setup(struct mca_coll_ml_module_t *ml_module);

/* broadcast */
int ml_coll_hier_bcast_setup(struct mca_coll_ml_module_t *ml_module);
void ml_coll_hier_bcast_cleanup(struct mca_coll_ml_module_t *ml_module);

/* reduce */
int ml_coll_hier_reduce_setup(struct mca_coll_ml_module_t *ml_module);
void ml_coll_hier_reduce_cleanup(struct mca_coll_ml_module_t *ml_module);

/* reduce */
int ml_coll_hier_scatter_setup(struct mca_coll_ml_module_t *ml_module);

/* alltoall */
int mca_coll_ml_alltoall(void *sbuf, int scount,
        struct ompi_datatype_t *sdtype,
        void* rbuf, int rcount,
        struct ompi_datatype_t *rdtype,
        struct ompi_communicator_t *comm,
        mca_coll_base_module_t *module);

int mca_coll_ml_alltoall_nb(void *sbuf, int scount,
        struct ompi_datatype_t *sdtype,
        void* rbuf, int rcount,
        struct ompi_datatype_t *rdtype,
        struct ompi_communicator_t *comm,
        ompi_request_t **req,
        mca_coll_base_module_t *module);


/* allgather */
int mca_coll_ml_allgather(void *sbuf, int scount,
        struct ompi_datatype_t *sdtype,
        void* rbuf, int rcount,
        struct ompi_datatype_t *rdtype,
        struct ompi_communicator_t *comm,
        mca_coll_base_module_t *module);

/* non-blocking allgather */
int mca_coll_ml_allgather_nb(void *sbuf, int scount,
        struct ompi_datatype_t *sdtype,
        void* rbuf, int rcount,
        struct ompi_datatype_t *rdtype,
        struct ompi_communicator_t *comm,
        ompi_request_t **req,
        mca_coll_base_module_t *module);

/* gather */
int mca_coll_ml_gather(void *sbuf, int scount,
        struct ompi_datatype_t *sdtype,
        void* rbuf, int rcount,
        struct ompi_datatype_t *rdtype,
        int root,
        struct ompi_communicator_t *comm,
        mca_coll_base_module_t *module);

/* nonblocking Barrier */
int ml_coll_hier_nonblocking_barrier_setup(struct mca_coll_ml_module_t *ml_module, struct mca_coll_ml_topology_t *topo_info);

/* Memory syncronization collective setup */
int ml_coll_memsync_setup(struct mca_coll_ml_module_t *ml_module);

/* Fragment descriptor */
struct mca_coll_ml_descriptor_t;
struct mca_coll_ml_fragment_t {
    opal_list_item_t super;

    struct mca_coll_ml_descriptor_t *full_msg_descriptor;
    int offset; /*offset for progress pointer*/
    int length; /*fragment length I assume*/
    opal_convertor_t convertor; /*convertor for copy/pack data*/

    /* current function index */
    int current_fn_index;

    /* array of function arguments */
    struct bcol_function_args_t *fn_args;

};
typedef struct mca_coll_ml_fragment_t mca_coll_ml_fragment_t;
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_coll_ml_fragment_t);

#define MCA_COLL_ML_NO_BUFFER -1

#define MCA_COLL_IBOFFLOAD_SET_ML_BUFFER_INFO(coll_op, index, desc) \
do {                                                                \
    (coll_op)->variable_fn_params.buffer_index = index;             \
    (coll_op)->fragment_data.buffer_desc = desc;                    \
    /* pasha - why we duplicate it ? */                             \
    (coll_op)->variable_fn_params.src_desc = desc;                  \
    (coll_op)->variable_fn_params.hier_factor = 1;                  \
    (coll_op)->variable_fn_params.need_dt_support = false;          \
} while (0)

/*Full message descriptor*/
struct mca_coll_ml_descriptor_t {
    ompi_request_t super; /*base request*/
    struct ompi_datatype_t *datatype; /*ompi datatype*/
    size_t count; /*count of user datatype elements*/
    uint32_t sequence_num; /*sequence number for collective operation*/
    size_t frags_limit; /*upper limit on # of fragments*/
    size_t frags_start; /*number of fragments started*/

    /*number of fragments completed*/
    size_t frags_complete;

    /* number of fragments needed to process this message */
    size_t n_fragments;

    volatile bool free_resource; /*signals release resource*/

    /*pointer to reduction operation, e.g. MPI_MIN - need to handle
     * user defined functions also */
    /* ompi_predefined_op_t *operation;  */

    /*pointer to a communication schedule, data struct undefined*/
    struct coll_ml_collective_description_t *local_comm_description;

    /* fragment descriptor - we always have a fragment descriptor
     *   if we get a full message descriptor.  Optimization for
     *   small messages */
    mca_coll_ml_fragment_t fragment;
    /* The ML memory buffer index that should consist the send and
       recv information
       if the index is -1, it means no buffer was allocated */
    uint64_t buffer_index;
};
typedef struct mca_coll_ml_descriptor_t mca_coll_ml_descriptor_t;
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_coll_ml_descriptor_t);

/* sbgp and bcol module pairs */
struct hierarchy_pairs {
    mca_sbgp_base_module_t *subgroup_module;
    struct mca_bcol_base_module_t **bcol_modules;
    int num_bcol_modules;
    int bcol_index;
    mca_bcol_base_component_t *bcol_component;
};
typedef struct hierarchy_pairs hierarchy_pairs;

/* list of ranks in each group */
struct ml_level_t {
    int n_modules;
    hierarchy_pairs *modules;
};

typedef struct ml_level_t ml_level_t;

enum {
    COLL_ML_HR_FULL,          /* Full hierarchy topology, all bcols and sbgps attends in discovery */
    COLL_ML_HR_ALLREDUCE,
    COLL_ML_HR_NBS,           /* All hierarchy except base socket */
    COLL_ML_HR_SINGLE_PTP,    /* Single flat ptp hierarchy */
    COLL_ML_HR_SINGLE_IBOFFLOAD,    /* Single flat iboffload hierarchy */
    COLL_ML_TOPO_MAX
};

/* Topology-hierarchy discovery function */
struct mca_coll_ml_module_t; /* forward declaration for the function */

typedef int (* mca_coll_topo_discovery_fn_t)
    (struct mca_coll_ml_module_t *ml_module, int n_hierarchies);

typedef enum {
    COLL_ML_TOPO_DISABLED = 0,
    COLL_ML_TOPO_ENABLED = 1
} topo_status_t;

/**
 * Structure to hold the sm coll component.  First it holds the
 * base coll component, and then holds a bunch of
 * sm-coll-component-specific stuff (e.g., current MCA param
 * values).
 */
struct mca_coll_ml_component_t {
    /** Base coll component */
    mca_coll_base_component_2_0_0_t super;

    /** MCA parameter: Priority of this component */
    int ml_priority;

    /** MCA parameter: subgrouping components to use */
    char *subgroups_string;

    /** MCA parameter: basic collectives components to use */
    char *bcols_string;

    /** verbosity level */
    int verbose;

    /** max of communicators available to run ML */
    unsigned int max_comm;

    /** min size of comm to be available to run ML */
    int min_comm_size;

    /* base sequence number to use - the expectation is that
     * this will be used as a basis for generating IDs for
     * specific collective operations
     */
    int64_t base_sequence_number;

    /** memory pool */
    mca_coll_ml_lmngr_t memory_manager;

    /* We need it because some bcols cannot
       support all possible allreduce data types */
    bool need_allreduce_support;

    int use_knomial_allreduce;

    /* use hdl_framework */
    bool use_hdl_bcast;

    /* Enable / Disable fragmentation (0 - off, 1 - on, 2 - auto) */
    int enable_fragmentation;

    /* Broadcast algorithm */
    int bcast_algorithm;

    /* frag size that is used by list memory_manager */
    size_t lmngr_block_size;

    /* alignment that is used by list memory_manager */
    size_t lmngr_alignment;

    /* list size for memory_manager */
    size_t lmngr_size;

    /* number of payload memory banks */
    int n_payload_mem_banks;

    /* number of payload buffers per bank */
    int n_payload_buffs_per_bank;

    /* size of payload buffer */
    unsigned long long payload_buffer_size;

    /* pipeline depth for msg fragmentation */
    int pipeline_depth;

    /* Free list tunings */
    int free_list_init_size;

    int free_list_grow_size;

    int free_list_max_size;

    /*
     * queues for asynchronous collective progress
     */
    /* tasks that have not started, either because dependencies are not
     * statisfied, or resources are lacking
     */
    opal_list_t pending_tasks;
    opal_mutex_t pending_tasks_mutex;

    /* active incomplete tasks */
    opal_list_t active_tasks;
    opal_mutex_t active_tasks_mutex;

    /* sequential collectives to progress */
    opal_list_t sequential_collectives;
    opal_mutex_t sequential_collectives_mutex;

    bool progress_is_busy;

    /* Temporary hack for IMB test - not all bcols have allgather */
    bool disable_allgather;

    /* Temporary hack for IMB test - not all bcols have alltoall */
    bool disable_alltoall;

    /* Disable Reduce */
    bool disable_reduce;

    /* Brucks alltoall mca and other params */
    int use_brucks_smsg_alltoall;

    mca_coll_topo_discovery_fn_t topo_discovery_fn[COLL_ML_TOPO_MAX];

    /* Configure file for collectives */
    char *config_file_name;

    per_collective_configuration_t coll_config[ML_NUM_OF_FUNCTIONS][ML_NUM_MSG];
};

/**
 * Convenience typedef
 */
typedef struct mca_coll_ml_component_t mca_coll_ml_component_t;

/**
 * Global component instance
 */
OMPI_MODULE_DECLSPEC extern mca_coll_ml_component_t mca_coll_ml_component;

struct mca_coll_ml_leader_offset_info_t {
    size_t offset;
    int level_one_index;
    bool leader;
};
typedef struct mca_coll_ml_leader_offset_info_t mca_coll_ml_leader_offset_info_t;

/* Topolody data structure */
struct mca_coll_ml_topology_t {
    topo_status_t status; /* 0 - enabled , 1 - disabled */
    /* information on the selected groups - needed for collective
     ** algorithms */
    int32_t global_lowest_hier_group_index;
    int32_t global_highest_hier_group_index;
    int number_of_all_subgroups;
    int n_levels;
    /* bcols bits that describe supported features/modes */
    uint64_t all_bcols_mode;
    mca_bcol_base_route_info_t *route_vector;
    coll_ml_collective_description_t *hierarchical_algorithms[BCOL_NUM_OF_FUNCTIONS];
    sub_group_params_t *array_of_all_subgroups;
    /* (sbgp, bcol) pairs */
    hierarchy_pairs *component_pairs;
    /* ordering of ranks when I am the root of the operation.
     * This ordering guarantees that data need to be re-ordered
     * only at the first or last step in rooted operations,
     * depending on whether the opearation is a scatter or
     * gather operation.
     */
    int *sort_list;
    mca_coll_ml_leader_offset_info_t *hier_layout_info;
    /* are ranks laid out contiguously */
    bool ranks_contiguous;
    struct ordering_info_t {
        int next_inorder;
        int next_order_num;
        int num_bcols_need_ordering;
    } topo_ordering_info;
};
typedef struct mca_coll_ml_topology_t mca_coll_ml_topology_t;

struct mca_coll_ml_bcol_list_item_t {
    opal_list_item_t super;
    mca_bcol_base_module_t *bcol_module;
};
typedef struct mca_coll_ml_bcol_list_item_t mca_coll_ml_bcol_list_item_t;
OPAL_DECLSPEC OBJ_CLASS_DECLARATION(mca_coll_ml_bcol_list_item_t);

#define MCA_COLL_MAX_NUM_COLLECTIVES 40 /* ... I do not remember how much exactly collectives do we have */
#define MCA_COLL_MAX_NUM_SUBTYPES 15    /* Maximum number of algorithms per collective */

struct mca_coll_ml_module_t {
    /* base structure */
    mca_coll_base_module_t super;

    /* ML module status - 0 was not initialized, 1 - was initialized */
    bool initialized;
    /* communicator */
    struct ompi_communicator_t *comm;

    /* reference convertor */
    opal_convertor_t *reference_convertor;

    mca_coll_ml_topology_t topo_list[COLL_ML_TOPO_MAX];

    /* Collectives - Topology map */
    int collectives_topology_map
        [MCA_COLL_MAX_NUM_COLLECTIVES][MCA_COLL_MAX_NUM_SUBTYPES];

    /* largest number of function calls for the collective routines.
     * This is used to allocate resources */
    int max_fn_calls;

    /* collective sequence number - unique id for barrier type operations */
    int32_t no_data_collective_sequence_num;

    /* collective sequence number - unique id for each collective */
    int32_t collective_sequence_num;

    /** ompi free list of full message descriptors **/
    opal_free_list_t message_descriptors;

    /** ompi free list of message fragment descriptors **/
    opal_free_list_t fragment_descriptors;

    /** pointer to the payload memory block **/
    struct mca_bcol_base_memory_block_desc_t *payload_block;

    /** the maximum size of collective function description */
    int max_dag_size;

    /** data used to initialize coll_ml_collective_descriptors */
    struct coll_desc_init {
        int max_dag_size;
        size_t max_n_bytes_per_proc_total;
        mca_coll_base_module_t *bcol_base_module;
    } coll_desc_init_data;

    /** collective operation descriptor free list - used to manage a single
     *  collective operation. */
    opal_free_list_t coll_ml_collective_descriptors;

    /** multiple function collective operation support */
    /** broadcast */
    mca_coll_ml_collective_operation_description_t *
        coll_ml_bcast_functions[ML_NUM_BCAST_FUNCTIONS];

    /* bcast size selection criteria - cutoff for the largest size of
     * data for which to apply the specified collective operation.
     * This gives us the ability to choose algorithm based on size */
    size_t bcast_cutoff_size[ML_N_DATASIZE_BINS];

     /** Allreduce functions */
    mca_coll_ml_collective_operation_description_t *
        coll_ml_allreduce_functions[ML_NUM_ALLREDUCE_FUNCTIONS];

    /** Reduce functions */
    mca_coll_ml_collective_operation_description_t *
        coll_ml_reduce_functions[ML_NUM_REDUCE_FUNCTIONS];


    /** scatter */
    mca_coll_ml_collective_operation_description_t *
        coll_ml_scatter_functions[ML_NUM_SCATTER_FUNCTIONS];

    /** alltoall */
    mca_coll_ml_collective_operation_description_t *
        coll_ml_alltoall_functions[ML_NUM_ALLTOALL_FUNCTIONS];

   /** allgather */
    mca_coll_ml_collective_operation_description_t *
        coll_ml_allgather_functions[ML_NUM_ALLGATHER_FUNCTIONS];

   /** gather */
    mca_coll_ml_collective_operation_description_t *
        coll_ml_gather_functions[ML_NUM_GATHER_FUNCTIONS];

    /** Barrier */
    mca_coll_ml_collective_operation_description_t *
                                coll_ml_barrier_function;

    /** ML Memory Syncronization collective operation */
    mca_coll_ml_collective_operation_description_t *
        coll_ml_memsync_function;

    /** The table of allreduce functions for specific type and op **/
    bool allreduce_matrix[OMPI_OP_NUM_OF_TYPES][OMPI_DATATYPE_MAX_PREDEFINED][BCOL_NUM_OF_ELEM_TYPES];

    /* data offset from ML */
    int32_t  data_offset;

    int small_message_thresholds[BCOL_NUM_OF_FUNCTIONS];

    /* fragmenation parameters */
    int use_user_buffers;
    uint64_t fragment_size;
    uint32_t ml_fragment_size;

    /* Bcast index table. Pasha: Do we need to define something more generic ?
     the table  x 2 (large/small)*/
    int bcast_fn_index_table[2];

    /* List of pointer to bcols that have been initilized and used.
     * So far we use it only for ML memory management */
    opal_list_t active_bcols_list;

    /* Buffer size required for Bruck's algorithm */
    int brucks_buffer_threshold_const;

    /* log comm size */
    /* We require this for alltoall algorithm */
    int log_comm_size;
    /* On this list we keep coll_op descriptors that were not
     * be able to start, since no ml buffers were available */
    opal_list_t waiting_for_memory_list;

    /* fallback collectives */
    mca_coll_base_comm_coll_t fallback;
};

typedef struct mca_coll_ml_module_t mca_coll_ml_module_t;
OBJ_CLASS_DECLARATION(mca_coll_ml_module_t);


/* query to see if the component is available for use, and can
 * satisfy the thread and progress requirements
 */
int mca_coll_ml_init_query(bool enable_progress_threads,
        bool enable_mpi_threads);

/* query to see if the module is available for use on the given
 * communicator, and if so, what it's priority is.  This is where
 * the backing shared-memory file is created.
 */
mca_coll_base_module_t *
mca_coll_ml_comm_query(struct ompi_communicator_t *comm, int *priority);

/* Barrier - blocking */
int mca_coll_ml_barrier_intra(struct ompi_communicator_t *comm,
                              mca_coll_base_module_t *module);

/* Barrier - non-blocking */
int mca_coll_ml_ibarrier_intra(struct ompi_communicator_t *comm,
                               ompi_request_t **req,
                               mca_coll_base_module_t *module);

/* Allreduce with EXTRA TOPO using - blocking */
int mca_coll_ml_allreduce_dispatch(void *sbuf, void *rbuf, int count,
                                struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                                struct ompi_communicator_t *comm, mca_coll_base_module_t *module);

/* Allreduce with EXTRA TOPO using - Non-blocking */
int mca_coll_ml_allreduce_dispatch_nb(void *sbuf, void *rbuf, int count,
                                   ompi_datatype_t *dtype, ompi_op_t *op,
                                   ompi_communicator_t *comm,
                                   ompi_request_t **req,
                                   mca_coll_base_module_t *module);

/* Allreduce - blocking */
int mca_coll_ml_allreduce(void *sbuf, void *rbuf, int count,
                                struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                                struct ompi_communicator_t *comm,
                                mca_coll_base_module_t *module);

/* Allreduce - Non-blocking */
int mca_coll_ml_allreduce_nb(void *sbuf, void *rbuf, int count,
                                struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                                struct ompi_communicator_t *comm,
                                ompi_request_t **req,
                                mca_coll_base_module_t *module);

/* Reduce - Blocking */
int mca_coll_ml_reduce(void *sbuf, void *rbuf, int count,
        struct ompi_datatype_t *dtype, struct ompi_op_t *op,
        int root, struct ompi_communicator_t *comm,
        mca_coll_base_module_t *module);

int mca_coll_ml_reduce_nb(void *sbuf, void *rbuf, int count,
        struct ompi_datatype_t *dtype, struct ompi_op_t *op,
        int root, struct ompi_communicator_t *comm,
        ompi_request_t **req,
        mca_coll_base_module_t *module);

int mca_coll_ml_memsync_intra(mca_coll_ml_module_t *module, int bank_index);


int coll_ml_progress_individual_message(mca_coll_ml_fragment_t *frag_descriptor);

/*
 * the ml entry point for the broadcast function
 */
int mca_coll_ml_parallel_bcast(void *buf, int count, struct ompi_datatype_t *dtype,
        int root, struct ompi_communicator_t *comm,
        mca_coll_base_module_t *module);
int mca_coll_ml_parallel_bcast_nb(void *buf, int count, struct ompi_datatype_t *dtype,
        int root, struct ompi_communicator_t *comm,
        ompi_request_t **req,
        mca_coll_base_module_t *module);
int mca_coll_ml_bcast_sequential_root(void *buf, int count, struct ompi_datatype_t *dtype,
        int root, struct ompi_communicator_t *comm,
        mca_coll_base_module_t *module);

/*
 * The ml function interface for non-blocking routines
 */
int mca_coll_ml_bcast_unknown_root_nb(void *buf, int count, struct ompi_datatype_t *dtype,
                                         int root, struct ompi_communicator_t *comm,
                                         ompi_request_t **req,
                                         mca_coll_base_module_t *module);

int mca_coll_ml_bcast_known_root_nb(void *buf, int count, struct ompi_datatype_t *dtype,
                                         int root, struct ompi_communicator_t *comm,
                                         ompi_request_t **req,
                                         mca_coll_base_module_t *module);
OMPI_DECLSPEC int mca_coll_ml_bcast_unknown_root_with_frags_nb(void *buf, int count,
        struct ompi_datatype_t *dtype,
        int root, struct ompi_communicator_t *comm,
        ompi_request_t **req, mca_coll_base_module_t *module);

/* This routine sets up a sequential hierarchical scatter algorithm.  The
 * assumptions are that each rank knows in which sub-group that data will show
 * up first, and that the scatter is executed sequentially, one subgroup at a
 * time.  This is needed, when the full collective needs to be specified before
 * the collective operation starts up.  The algorithm handles all data sizes
 * and data types.
 */

OMPI_DECLSPEC int mca_coll_ml_scatter_sequential(
        void *sbuf, int scount, struct ompi_datatype_t *sdtype,
        void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
        int root, struct ompi_communicator_t *comm,
        mca_coll_base_module_t *module);

#if 0
int mca_coll_ml_bcast_small_dynamic_root(void *buf, int count, struct ompi_datatype_t *dtype,
        int root, struct ompi_communicator_t *comm,
        mca_coll_base_module_t *module);
int mca_coll_ml_bcast_small_known_root(void *buf, int count, struct ompi_datatype_t *dtype,
        int root, struct ompi_communicator_t *comm,
        mca_coll_base_module_t *module);
#endif

/* Topology discovery function */

int mca_coll_ml_fulltree_hierarchy_discovery(mca_coll_ml_module_t *ml_module,
        int n_hierarchies);
int mca_coll_ml_allreduce_hierarchy_discovery(mca_coll_ml_module_t *ml_module,
        int n_hierarchies);
int mca_coll_ml_fulltree_exclude_basesmsocket_hierarchy_discovery(mca_coll_ml_module_t *ml_module,
        int n_hierarchies);
int mca_coll_ml_fulltree_ptp_only_hierarchy_discovery(mca_coll_ml_module_t *ml_module,
        int n_hierarchies);
int mca_coll_ml_fulltree_iboffload_only_hierarchy_discovery(mca_coll_ml_module_t *ml_module,
        int n_hierarchies);

void mca_coll_ml_allreduce_matrix_init(mca_coll_ml_module_t *ml_module,
                     const mca_bcol_base_component_2_0_0_t *bcol_component);
static inline int mca_coll_ml_err(const char* fmt, ...)
{
    va_list list;
    int ret;

    va_start(list, fmt);
    ret = vfprintf(stderr, fmt, list);
    va_end(list);
    return ret;
}


#define ML_ERROR(args)                                       \
do {                                                     \
    mca_coll_ml_err("[%s]%s[%s:%d:%s] COLL-ML ",         \
        ompi_process_info.nodename,                      \
        OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),              \
        __FILE__, __LINE__, __func__);                   \
    mca_coll_ml_err args;                                \
    mca_coll_ml_err("\n");                               \
} while(0)

#if OPAL_ENABLE_DEBUG
#define ML_VERBOSE(level, args)                              \
do {                                                     \
    if(mca_coll_ml_component.verbose >= level) {         \
        mca_coll_ml_err("[%s]%s[%s:%d:%s] COLL-ML ",     \
                ompi_process_info.nodename,              \
                OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),      \
                __FILE__, __LINE__, __func__);           \
        mca_coll_ml_err args;                            \
        mca_coll_ml_err("\n");                           \
    }                                                    \
} while(0)
#else
#define ML_VERBOSE(level, args)
#endif

#define IS_BCOL_TYPE_IDENTICAL(bcol1, bcol2)                                                         \
    (      (NULL != bcol1 && NULL != bcol2) &&                                                       \
     (     /* chech if the len is the same */                                                        \
     (strlen(((mca_base_component_t *)((bcol1)->bcol_component))->mca_component_name) ==             \
      strlen(((mca_base_component_t *)((bcol2)->bcol_component))->mca_component_name))               \
      &&  /* check if the string are identical */                                                    \
     (0 == strncmp(((mca_base_component_t *)((bcol1)->bcol_component))->mca_component_name,          \
                   ((mca_base_component_t *)((bcol2)->bcol_component))->mca_component_name,          \
                   strlen(((mca_base_component_t *)((bcol2)->bcol_component))->mca_component_name))) \
     ) ? true : false)

#define GET_BCOL(module, indx) ((module)->component_pairs[(indx)].bcol_modules[0])

#define GET_BCOL_SYNC_FN(bcol) ((bcol)->filtered_fns_table[DATA_SRC_KNOWN][NON_BLOCKING] \
                                                          [BCOL_SYNC][1][0][0])

/* Allocator macros */
#define BUFFER_INDEX(bank,nbuffs,buffer) (bank*nbuffs+buffer)

#define ML_GET_FRAG_SIZE(op, coll)                                 \
    ((op)->fragment_data.message_descriptor->n_bytes_total -       \
     (op)->fragment_data.message_descriptor->n_bytes_scheduled <   \
     (size_t) OP_ML_MODULE((op))->small_message_thresholds[coll] ? \
     (op)->fragment_data.message_descriptor->n_bytes_total -       \
     (op)->fragment_data.message_descriptor->n_bytes_scheduled :   \
     (size_t) OP_ML_MODULE((op))->small_message_thresholds[coll])

/* Abort mpi process in case of fatal error */
void mca_coll_ml_abort_ml(char *message);

#define ML_SET_VARIABLE_PARAMS_BCAST(op, ml, cnt, datatype, b_desc,       \
                        s_offset, r_offset, frag_len, buf)                \
do {                                                                      \
            op->variable_fn_params.sequence_num =                         \
            OPAL_THREAD_ADD32(&((ml)->collective_sequence_num), 1);       \
            op->variable_fn_params.count = cnt;                           \
            op->variable_fn_params.dtype = datatype;                      \
            op->variable_fn_params.buffer_index = (b_desc)->buffer_index; \
            op->variable_fn_params.src_desc = (b_desc);                   \
            op->variable_fn_params.sbuf_offset = s_offset;                \
            op->variable_fn_params.rbuf_offset = r_offset;                \
            op->variable_fn_params.frag_size = frag_len;                  \
            op->variable_fn_params.sbuf = buf;                            \
} while (0)

#define MCA_COLL_ML_OP_BASIC_SETUP(op, total_bytes, offset_into_user_buff, src, dst, collective_schedule)      \
    do {                                                                                    \
        op->coll_schedule                         = collective_schedule;                    \
        op->process_fn                            = NULL;                                   \
        op->full_message.n_bytes_total            = total_bytes;                            \
        op->full_message.n_bytes_delivered        = 0;                                      \
        op->full_message.n_bytes_scheduled        = 0;                                      \
        op->full_message.dest_user_addr           = dst;                                    \
        op->full_message.src_user_addr            = src;                                    \
        op->full_message.n_active                 = 0;                                      \
        op->full_message.n_bytes_per_proc_total   = 0;                                      \
        op->full_message.send_count               = 0;                                      \
        op->full_message.recv_count               = 0;                                      \
        op->full_message.send_extent              = 0;                                      \
        op->full_message.recv_extent              = 0;                                      \
        op->full_message.offset_into_send_buffer  = 0;                                      \
        op->full_message.offset_into_recv_buffer  = 0;                                      \
        op->full_message.send_data_type           = 0;                                      \
        op->full_message.recv_data_type           = 0;                                      \
        op->full_message.fragment_launcher            = 0;                                  \
        op->sequential_routine.current_active_bcol_fn = 0;                                  \
        op->sequential_routine.current_bcol_status    = SEQ_TASK_NOT_STARTED;               \
                                                                                            \
        op->fragment_data.offset_into_user_buffer = offset_into_user_buff;                  \
        /* Pasha, is it constant ? what to put here */                                      \
        op->fragment_data.fragment_size           = total_bytes;                            \
        op->fragment_data.message_descriptor      = &op->full_message;                      \
        op->fragment_data.current_coll_op         = -1;                                     \
    } while (0)

/* This routine re-orders and packs user data.  The assumption is that
 * there is per-process data, the amount of data is the same for all * ranks,
 * and the user data is contigous.
 */
int mca_coll_ml_pack_reorder_contiguous_data(
        mca_coll_ml_collective_operation_progress_t *coll_op);

/* This routine re-orders and packs user data.  The assumption is that
 * there is per-process data, the amount of data is the same for all * ranks,
 * and the user data is noncontigous.
 */
int mca_coll_ml_pack_reorder_noncontiguous_data(
        mca_coll_ml_collective_operation_progress_t *coll_op);

END_C_DECLS


#endif /* MCA_COLL_ML_ML_H */
