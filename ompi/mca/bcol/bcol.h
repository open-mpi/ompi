/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BCOL_H
#define MCA_BCOL_H

#include "ompi_config.h"
#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"
#include "ompi/mca/coll/coll.h"
#include "opal/mca/mpool/mpool.h"
#include "ompi/mca/sbgp/sbgp.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/op/op.h"
#include "ompi/include/ompi/constants.h"
#include "ompi/patterns/net/netpatterns_knomial_tree.h"

#include "opal/util/show_help.h"

#include <limits.h>

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/* Forward declaration - please do not remove it */
struct ml_buffers_t;

struct mca_bcol_base_coll_fn_comm_attributes_t;
struct mca_bcol_base_coll_fn_invoke_attributes_t;
struct mca_bcol_base_coll_fn_desc_t;

#define NUM_MSG_RANGES      5
#define MSG_RANGE_INITIAL (1024)*12
#define MSG_RANGE_INC      10
#define BCOL_THRESHOLD_UNLIMITED (INT_MAX)
/* Maximum size of a bcol's header. This allows us to correctly calculate the message
 * thresholds. If the header of any bcol exceeds this value then increase this one
 * to match. */
#define BCOL_HEADER_MAX 96

#define BCOL_HEAD_ALIGN 32   /* will turn into an MCA parameter after debug */

/*
 * Functions supported
 */
enum bcol_coll {
    /* blocking functions */
    BCOL_ALLGATHER,
    BCOL_ALLGATHERV,
    BCOL_ALLREDUCE,
    BCOL_ALLTOALL,
    BCOL_ALLTOALLV,
    BCOL_ALLTOALLW,
    BCOL_BARRIER,
    BCOL_BCAST,
    BCOL_EXSCAN,
    BCOL_GATHER,
    BCOL_GATHERV,
    BCOL_REDUCE,
    BCOL_REDUCE_SCATTER,
    BCOL_SCAN,
    BCOL_SCATTER,
    BCOL_SCATTERV,
    BCOL_FANIN,
    BCOL_FANOUT,

    /* nonblocking functions */
    BCOL_IALLGATHER,
    BCOL_IALLGATHERV,
    BCOL_IALLREDUCE,
    BCOL_IALLTOALL,
    BCOL_IALLTOALLV,
    BCOL_IALLTOALLW,
    BCOL_IBARRIER,
    BCOL_IBCAST,
    BCOL_IEXSCAN,
    BCOL_IGATHER,
    BCOL_IGATHERV,
    BCOL_IREDUCE,
    BCOL_IREDUCE_SCATTER,
    BCOL_ISCAN,
    BCOL_ISCATTER,
    BCOL_ISCATTERV,
    BCOL_IFANIN,
    BCOL_IFANOUT,

    BCOL_SYNC,
    /* New function - needed for intermediate steps */
    BCOL_REDUCE_TO_LEADER,
    BCOL_NUM_OF_FUNCTIONS
};
typedef enum bcol_coll bcol_coll;

typedef enum bcol_elem_type {
    BCOL_SINGLE_ELEM_TYPE,
    BCOL_MULTI_ELEM_TYPE,
    BCOL_NUM_OF_ELEM_TYPES
} bcol_elem_type;

typedef int (*mca_bcol_base_module_coll_support_all_types_fn_t)(bcol_coll coll_name);
typedef int (*mca_bcol_base_module_coll_support_fn_t)(int op, int dtype, bcol_elem_type elem_num);

/*
 * Collective function status
 */
enum {
    BCOL_FN_NOT_STARTED = (OMPI_ERR_MAX - 1),
    BCOL_FN_STARTED     = (OMPI_ERR_MAX - 2),
    BCOL_FN_COMPLETE    = (OMPI_ERR_MAX - 3)
};



/**
 * Collective component initialization
 *
 * Initialize the given collective component.  This function should
 * initialize any component-level. data.  It will be called exactly
 * once during MPI_INIT.
 *
 * @note The component framework is not lazily opened, so attempts
 * should be made to minimze the amount of memory allocated during
 * this function.
 *
 * @param[in] enable_progress_threads True if the component needs to
 *                                support progress threads
 * @param[in] enable_mpi_threads  True if the component needs to
 *                                support MPI_THREAD_MULTIPLE
 *
 * @retval OMPI_SUCCESS Component successfully initialized
 * @retval ORTE_ERROR   An unspecified error occurred
 */
typedef int (*mca_bcol_base_component_init_query_fn_t)
    (bool enable_progress_threads, bool enable_mpi_threads);

/**
 * Query whether a component is available for the given sub-group
 *
 * Query whether the component is available for the given
 * sub-group.  If the component is available, an array of pointers should be
 * allocated and returned (with refcount at 1).  The module will not
 * be used for collective operations until module_enable() is called
 * on the module, but may be destroyed (via OBJ_RELEASE) either before
 * or after module_enable() is called.  If the module needs to release
 * resources obtained during query(), it should do so in the module
 * destructor.
 *
 * A component may provide NULL to this function to indicate it does
 * not wish to run or return an error during module_enable().
 *
 * @note The communicator is available for point-to-point
 * communication, but other functionality is not available during this
 * phase of initialization.
 *
 * @param[in] sbgp         Pointer to sub-group module.
 * @param[out] priority    Priority setting for component on
 *                         this communicator
 * @param[out] num_modules Number of modules that where generated
 *                         for the sub-group module.
 *
 * @returns An array of pointer to an initialized modules structures if the component can
 * provide a modules with the requested functionality or NULL if the
 * component should not be used on the given communicator.
 */
typedef struct mca_bcol_base_module_t **(*mca_bcol_base_component_comm_query_fn_t)
    (mca_sbgp_base_module_t *sbgp, int *num_modules);


typedef int (*mca_bcol_barrier_init_fn_t)(struct mca_bcol_base_module_t *bcol_module,
        mca_sbgp_base_module_t *sbgp_module);



/*
 * Macro for use in modules that are of type btl v2.0.0
 */
#define MCA_BCOL_BASE_VERSION_2_0_0 \
    MCA_BASE_VERSION_2_0_0, \
      "bcol", 2, 0, 0


/* This is really an abstarction violation, but is the easiest way to get
 * started.  For memory management we need to know what bcol components
 * have compatible memory management schemes.  Such compatibility can
 * be used to eliminate memory copies between levels in the collective
 * operation hierarchy, by having the output buffer of one level be the
 * input buffer to the next level
 */

enum {
    BCOL_SHARED_MEMORY_UMA=0,
    BCOL_SHARED_MEMORY_SOCKET,
    BCOL_POINT_TO_POINT,
    BCOL_IB_OFFLOAD,
    BCOL_SIZE
};

OMPI_DECLSPEC extern int bcol_mpool_compatibility[BCOL_SIZE][BCOL_SIZE];
OMPI_DECLSPEC extern int bcol_mpool_index[BCOL_SIZE][BCOL_SIZE];

/* what are the input parameters ? too many void * pointers here */
typedef int (*bcol_register_mem_fn_t)(void *context_data, void *base,
        size_t size, void **reg_desc);
/* deregistration function */
typedef int (*bcol_deregister_mem_fn_t)(void *context_data, void *reg_desc);

/* Bcol network context definition */
struct bcol_base_network_context_t {
    opal_object_t super;
    /* Context id - defined by upper layer, ML */
    int context_id;
    /* Any context information that bcol what to use */
    void *context_data;

    /* registration function */
    bcol_register_mem_fn_t register_memory_fn;
    /* deregistration function */
    bcol_deregister_mem_fn_t deregister_memory_fn;
};
typedef struct bcol_base_network_context_t bcol_base_network_context_t;
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(bcol_base_network_context_t);

/*
 *primitive function types
 */

/* bcast */
enum {
    /* small data function */
    BCOL_BCAST_SMALL_DATA,

    /* small data - dynamic decision making supported */
    BCOL_BCAST_SMALL_DATA_DYNAMIC,

    /* number of functions */
    BCOL_NUM_BCAST_FUNCTIONS
};


/**
 *  BCOL instance.
 */

/* no limit on fragment size - this supports using user buffers rather
 * than library buffers
 */
#define FRAG_SIZE_NO_LIMIT -1

/* forward declaration */
struct coll_bcol_collective_description_t;

struct mca_bcol_base_component_2_0_0_t {

    /** Base component description */
    mca_base_component_t bcol_version;

    /** Component initialization function */
    mca_bcol_base_component_init_query_fn_t collm_init_query;

    /** Query whether component is useable for given communicator */
    mca_bcol_base_component_comm_query_fn_t collm_comm_query;

    /** If bcol supports all possible data types */
    mca_bcol_base_module_coll_support_fn_t coll_support;

    /** If bcol supports all possible data types for given collective operation */
    mca_bcol_base_module_coll_support_all_types_fn_t coll_support_all_types;

    /** Use this flag to prevent init_query multiple calls
        in case we have the same bcol more than on a single level */
    bool init_done;

    /** If collective calls with bcols of this type need to be ordered */
    bool need_ordering;

    /** MCA parameter: Priority of this component */
    int priority;

    /** Bcast function pointers */
    struct coll_bcol_collective_description_t *
        bcast_functions[BCOL_NUM_BCAST_FUNCTIONS];

    /** Number of network contexts - need this for resource management */
    int n_net_contexts;

    /** List of network contexts */
    bcol_base_network_context_t **network_contexts;

    /*
     * Fragmentation support
     */

    /** Minimum fragement size */
    int min_frag_size;

    /** Maximum fragment size */
    int max_frag_size;

    /** Supports direct use of user-buffers */
    bool can_use_user_buffers;
};
typedef struct mca_bcol_base_component_2_0_0_t mca_bcol_base_component_2_0_0_t;
typedef struct mca_bcol_base_component_2_0_0_t mca_bcol_base_component_t;
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_bcol_base_component_t);

/* forward declaration */
struct mca_coll_ml_descriptor_t;
struct mca_bcol_base_payload_buffer_desc_t;
struct mca_bcol_base_route_info_t;

typedef struct {
    int order_num;           /* Seq num of collective fragment */
    int bcols_started;       /* How many bcols need ordering have been started */
    int n_fns_need_ordering; /* The number of functions are called for bcols need ordering */
} mca_bcol_base_order_info_t;

/* structure that encapsultes information propagated amongst multiple
 * fragments whereby completing the entire ensemble of fragments is
 * necessary in order to complete the entire collective
 */
struct bcol_fragment_descriptor_t {
    /* start iterator */
    int head;
    /* end iterator */
    int tail;
    /* current iteration */
    int start_iter;
    /* number of full iterations this frag */
    int num_iter;
    /* end iter */
    int end_iter;
};
typedef struct bcol_fragment_descriptor_t bcol_fragment_descriptor_t;

struct bcol_function_args_t {
    /* full message sequence number */
    int64_t sequence_num;
    /* full message descriptor - single copy of fragment invariant
     * parameters */
    /* Pasha: We don need this one for new flow - remove it */
    struct mca_coll_ml_descriptor_t *full_message_descriptor;
    struct mca_bcol_base_route_info_t *root_route;
    /* function status */
    int function_status;
    /* root, for rooted operations */
    int root;
    /* input buffer */
    void *sbuf;
    void *rbuf;
    void *userbuf;
    struct mca_bcol_base_payload_buffer_desc_t *src_desc;
    struct mca_bcol_base_payload_buffer_desc_t *dst_desc;
   /* ml buffer size */
    uint32_t buffer_size;
    /* index of buffer in ml payload cache */
    int buffer_index;
    int count;
    struct ompi_datatype_t *dtype;
    struct ompi_op_t *op;
    int sbuf_offset;
    int rbuf_offset;
    /* for bcol opaque data */
    void *bcol_opaque_data;
    /* An output argument that will be used by BCOL function to tell ML that the result of the BCOL is in rbuf */
    bool result_in_rbuf;
    bool root_flag;      /* True if the rank is root of operation */
    bool need_dt_support; /* will trigger alternate code path for some colls */
    int status;          /* Used for non-blocking collective completion */
    uint32_t frag_size;  /* fragment size for large messages */
    int hier_factor;     /* factor used when bcast is invoked as a service function back down
                          * the tree in allgather for example, the pacl_len is not the actual
                          * len of the data needing bcasting
                          */
    mca_bcol_base_order_info_t order_info;
    bcol_fragment_descriptor_t frag_info;

};

struct mca_bcol_base_route_info_t {
    int level;
    int rank;
};
typedef struct mca_bcol_base_route_info_t mca_bcol_base_route_info_t;

struct mca_bcol_base_lmngr_block_t {
    opal_list_item_t super;
    struct mca_coll_ml_lmngr_t *lmngr;
    void* base_addr;
};
typedef struct mca_bcol_base_lmngr_block_t mca_bcol_base_lmngr_block_t;
OBJ_CLASS_DECLARATION(mca_bcol_base_lmngr_block_t);

struct mca_bcol_base_memory_block_desc_t {

    /* memory block for payload buffers */
    struct mca_bcol_base_lmngr_block_t *block;

    /* Address offset in bytes -- Indicates free memory in the block */
    uint64_t   block_addr_offset;

    /* size of the memory block */
    size_t     size_block;

    /* number of memory banks */
    uint32_t     num_banks;

    /* number of buffers per bank */
    uint32_t    num_buffers_per_bank;

    /* size of a payload buffer */
    uint32_t     size_buffer;

    /* pointer to buffer descriptors initialized */
    struct mca_bcol_base_payload_buffer_desc_t *buffer_descs;

    /* index of the next free buffer in the block */
    uint64_t next_free_buffer;

    uint32_t *bank_release_counters;

    /* Counter that defines what bank should be synchronized next
     * since collectives could be completed out of order, we have to make
     * sure that memory synchronization collectives started in order ! */
    int memsync_counter; 

    /* This arrays of flags used to signal that the bank is ready for recycling */
    bool *ready_for_memsync;

    /* This flags monitors if bank is open for usage. Usually we expect that user
     * will do the check only on buffer-zero allocation */
    bool *bank_is_busy;

};

/* convenience typedef */
typedef struct mca_bcol_base_memory_block_desc_t mca_bcol_base_memory_block_desc_t;

typedef void (*mca_bcol_base_release_buff_fn_t)(struct mca_bcol_base_memory_block_desc_t *ml_memblock, uint32_t buff_id);

struct mca_bcol_base_payload_buffer_desc_t {
    void         *base_data_addr;   /* buffer address */
    void         *data_addr;         /* buffer address  + header offset */
    uint64_t     generation_number;  /* my generation */
    uint64_t     bank_index;         /* my bank */
    uint64_t     buffer_index;       /* my buff index */
};
/* convenience typedef */
typedef struct mca_bcol_base_payload_buffer_desc_t mca_bcol_base_payload_buffer_desc_t;






typedef struct bcol_function_args_t bcol_function_args_t;


/* The collective operation is defined by a series of collective operations
 * invoked through a function pointer.  Each function may be different,
 * so will store the arguments in a struct and pass a pointer to the struct,
 * and use this as a way to hide the different function signatures.
 *
 * @param[in] input_args  Structure with function arguments
 * @param[in] bcol_desc   Component specific paremeters
 * @param[out] status  return status of the function
 *                     MCA_BCOL_COMPLETE    - function completed
 *                     MCA_BCOL_IN_PROGRESS - function incomplete
 *
 * @retval OMPI_SUCCESS successful completion
 * @retval OMPI_ERROR function returned error
 */
/* forward declaration */
struct mca_bcol_base_module_t;

/* collective function prototype - all functions have the same interface
 * so that we can call them via a function pointer */
struct mca_bcol_base_function_t;
typedef int (*mca_bcol_base_module_collective_fn_primitives_t)
    (bcol_function_args_t *input_args, struct mca_bcol_base_function_t *const_args);

typedef int (*mca_bcol_base_module_collective_init_fn_primitives_t)
    (struct mca_bcol_base_module_t *bcol_module);

    /**
     *  function to query for collctive function attributes
     *
     *  @param attribute (IN) the attribute of interest
     *  @param algorithm_parameters (OUT) the value of attribute for this
     *         function.  If this attribute is not supported,
     *         OMPI_ERR_NOT_FOUND is returned.
     */
    typedef int (*mca_bcol_get_collective_attributes)(int attribute,
            void *algorithm_parameters);

/* data structure for tracking the relevant data needed for ml level
 * algorithm construction (e.g., function selection), initialization, and
 * usage.
 */
struct coll_bcol_collective_description_t {
    /* collective initiation function - first functin called */
    mca_bcol_base_module_collective_fn_primitives_t coll_fn;

    /* collective progress function - first functin called */
    mca_bcol_base_module_collective_fn_primitives_t progress_fn;

    /* collective progress function - first functin called */
    mca_bcol_get_collective_attributes get_attributes;

    /* attributes supported - bit map */
    uint64_t attribute;

};
typedef struct coll_bcol_collective_description_t
coll_bcol_collective_description_t;

/* collective operation attributes */
enum {
    /* supports dynamic decisions - e.g., do not need to have the collective
     * operation fully defined before it can be started
     */
    BCOL_ATTRIBUTE_DYNAMIC,

    /* number of attributes */
    BCOL_NUM_ATTRIBUTES
};

/* For rooted collectives,
 * does the algorithm knows its data source ?
 */
enum {
    DATA_SRC_KNOWN=0,
    DATA_SRC_UNKNOWN,
    DATA_SRC_TYPES
};

enum {
    BLOCKING,
    NON_BLOCKING
};
/* gvm For selection logic */
struct mca_bcol_base_coll_fn_comm_attributes_t {
    int bcoll_type;
    int comm_size_min;
    int comm_size_max;
    int data_src;
    int waiting_semantics;
};

typedef struct mca_bcol_base_coll_fn_comm_attributes_t
                        mca_bcol_base_coll_fn_comm_attributes_t;

struct mca_bcol_base_coll_fn_invoke_attributes_t {
    int bcol_msg_min;
    int bcol_msg_max;
    uint64_t datatype_bitmap; /* Max is OMPI_DATATYPE_MAX_PREDEFINED defined to be 45 */
    uint32_t op_types_bitmap; /* bit map of optypes supported */
};

typedef struct mca_bcol_base_coll_fn_invoke_attributes_t
                        mca_bcol_base_coll_fn_invoke_attributes_t;

struct mca_bcol_base_coll_fn_desc_t {
    opal_list_item_t super;
    struct mca_bcol_base_coll_fn_comm_attributes_t *comm_attr;
    struct mca_bcol_base_coll_fn_invoke_attributes_t *inv_attr;
    mca_bcol_base_module_collective_fn_primitives_t coll_fn;
    mca_bcol_base_module_collective_fn_primitives_t progress_fn;
};

typedef struct mca_bcol_base_coll_fn_desc_t mca_bcol_base_coll_fn_desc_t;
OBJ_CLASS_DECLARATION(mca_bcol_base_coll_fn_desc_t);

/* end selection logic */

typedef int (*mca_bcol_base_module_collective_init_fn_t)
    (struct mca_bcol_base_module_t *bcol_module,
     mca_sbgp_base_module_t *sbgp_module);

    /* per communicator memory initialization function */
typedef  int (*mca_bcol_module_mem_init)(struct ml_buffers_t *registered_buffers,
 mca_bcol_base_component_t *module);

/* Initialize memory block - ml_memory_block initialization interface function
 *
 * Invoked at the ml level, used to pass bcol specific registration information
 * for the "ml_memory_block"
 *
 * @param[in] ml_memory_block   Pointer to the ml_memory_block. This struct
 *  contains bcol specific registration information and a call back function
 *  used for resource recycling.
 *
 * @param[in] reg_data         bcol specific registration data.
 *
 * @returns   On Success: OMPI_SUCCESS
 *            On Failure: OMPI_ERROR
 *
 */
/*typedef int (*mca_bcol_base_init_memory_fn_t)
    (struct mca_bcol_base_memory_block_desc_t *ml_block, void *reg_data);*/

typedef int (*mca_bcol_base_init_memory_fn_t)
     (struct mca_bcol_base_memory_block_desc_t *payload_block,
     uint32_t data_offset,
     struct mca_bcol_base_module_t *bcol,
     void *reg_data);

typedef int (*mca_common_allgather_init_fn_t)
    (struct mca_bcol_base_module_t *bcol_module);

typedef void (*mca_bcol_base_set_thresholds_fn_t)
    (struct mca_bcol_base_module_t *bcol_module);

enum {
    MCA_BCOL_BASE_ZERO_COPY                   = 1,
    MCA_BCOL_BASE_NO_ML_BUFFER_FOR_LARGE_MSG  = 1 << 1,
    MCA_BCOL_BASE_NO_ML_BUFFER_FOR_BARRIER    = 1 << 2
};

/* base  module */
struct mca_bcol_base_module_t {
    /* base coll component */
    opal_object_t super;

    /* bcol component (Pasha: Do we really need cache the component?)*/
    mca_bcol_base_component_t *bcol_component;

    /* network context that is used by this bcol
    only one context per bcol is allowed */
    bcol_base_network_context_t *network_context;

    /* We are going to use the context index a lot,
    int order to decrease number of dereferences
    bcol->network_context->index
    we are caching the value on bcol */
    int context_index;

    /* Set of flags that describe features supported by bcol */
    uint64_t supported_mode;

    /* per communicator memory initialization function */
    mca_bcol_module_mem_init init_module;

    /* sub-grouping module partner */
    mca_sbgp_base_module_t *sbgp_partner_module;

    /* size of subgroup - cache this, so can have access when
     * sbgp_partner_module no longer existes */
    int size_of_subgroup;

    /* sequence number offset - want to make sure that we start
     * id'ing collectives with id 0, so we can have simple
     * resource management.
     */
    int64_t squence_number_offset;


    /* number of times to poll for operation completion before
     * breaking out of a non-blocking collective operation
     */
    int n_poll_loops;

    /* size of header that will go in data buff, should not include
     * any info regarding alignment, let the ml level handle this
     */
    uint32_t header_size;


   /* Each bcol is assigned a unique value
    * see if we can get away with 16-bit id
    */
    int16_t bcol_id;

    /*FIXME:
     * Since mca_bcol_base_module_t is the only parameter which will be passed
     * into the bcol_basesmuma_bcast_init(), add the flag to indicate whether
     * the hdl-based algorithms will get enabled.
     */
    bool use_hdl;
        /*
     * Collective function pointers
     */
    /* changing function signature - will replace bcol_functions */
    mca_bcol_base_module_collective_fn_primitives_t bcol_function_table[BCOL_NUM_OF_FUNCTIONS];

    /* Tables hold pointers to functions */
    mca_bcol_base_module_collective_init_fn_primitives_t bcol_function_init_table[BCOL_NUM_OF_FUNCTIONS];
    opal_list_t bcol_fns_table[BCOL_NUM_OF_FUNCTIONS];
    struct mca_bcol_base_coll_fn_desc_t*
    filtered_fns_table[DATA_SRC_TYPES][2][BCOL_NUM_OF_FUNCTIONS][NUM_MSG_RANGES+1][OMPI_OP_NUM_OF_TYPES][OMPI_DATATYPE_MAX_PREDEFINED];

    /*
     * Bcol interface function to pass bcol specific
     * info and memory recycling call back
     */
    mca_bcol_base_init_memory_fn_t bcol_memory_init;

    /*
     * netpatterns interface function, would like to invoke this on
     * on the ml level
     */
    mca_common_allgather_init_fn_t k_nomial_tree;
     /* Each bcol caches a list which describes how many ranks
     * are "below" each rank in this bcol
     */
    int *list_n_connected;

    /* offsets for scatter/gather */
    int hier_scather_offset;

    /* Small message threshold for each collective */
    int small_message_thresholds[BCOL_NUM_OF_FUNCTIONS];

    /* Set small_message_thresholds array */
    mca_bcol_base_set_thresholds_fn_t set_small_msg_thresholds;

    /* Pointer to the order counter on the upper layer,
       used if the bcol needs to be ordered */
    int *next_inorder;
};
typedef struct mca_bcol_base_module_t mca_bcol_base_module_t;
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_bcol_base_module_t);

/* function description */
struct mca_bcol_base_function_t {
    int fn_idx;
    /* module */
    struct mca_bcol_base_module_t *bcol_module;

    /*
     *  The following two parameters are used for bcol modules
     *  that want to do some optimizations based on the fact that
     *  n functions from the same bcol module are called in a row.
     *  For example, in the iboffload case, on the first call one
     *  will want to initialize the MWR, and start to instantiate
     *  it, but only post it at the end of the last call.
     *  The index of this function in a sequence of consecutive
     *  functions from the same bcol
     */
    int index_in_consecutive_same_bcol_calls;

    /* number of times functions from this bcol are
     * called in order
     */
    int n_of_this_type_in_a_row;

    /*
     * number of times functions from this module are called in the
     * collective operation.
     */
    int n_of_this_type_in_collective;
    int index_of_this_type_in_collective;
};
typedef struct mca_bcol_base_function_t mca_bcol_base_function_t;




struct mca_bcol_base_descriptor_t {
    opal_free_list_item_t super;
/* Vasily: will be described in the future */
};
typedef struct mca_bcol_base_descriptor_t mca_bcol_base_descriptor_t;

static inline __opal_attribute_always_inline__ size_t
             mca_bcol_base_get_buff_length(ompi_datatype_t *dtype, int count)
{
    ptrdiff_t lb, extent;
    ompi_datatype_get_extent(dtype, &lb, &extent);

    return (size_t) (extent * count);
}

#define MCA_BCOL_CHECK_ORDER(module, bcol_function_args)                     \
    do {                                                                     \
        if (*((module)->next_inorder) !=                                     \
                               (bcol_function_args)->order_info.order_num) { \
            return BCOL_FN_NOT_STARTED;                                      \
        }                                                                    \
    } while (0);

#define MCA_BCOL_UPDATE_ORDER_COUNTER(module, order_info) \
    do {                                                  \
       (order_info)->bcols_started++;                     \
        if ((order_info)->n_fns_need_ordering ==          \
                        (order_info)->bcols_started) {    \
            ++(*((module)->next_inorder));                \
        }                                                 \
    } while (0);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_BCOL_H */
