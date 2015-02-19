/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2014-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COLL_ML_COLLS_H
#define MCA_COLL_ML_COLLS_H

#include "ompi_config.h"
#include "ompi/mca/bcol/bcol.h"

#define COLL_ML_FN_NAME_LEN 256


/* utility information used to coordinate activities, such as resource
 * management between different functions in the hierarchy
 */
struct mca_coll_ml_utility_data_t {

    /* RLG - temp fix  !!!! - really need to remove this, but right now
     do not want to change the signature of the collective primitives to
     use coll_ml_utility_data_t rather than mca_bcol_base_function_t */
    int dummy;

    /* module */
    struct mca_bcol_base_module_t *bcol_module;

    /* */
    int index_in_consecutive_same_bcol_calls;

    /* number of times functions from this bcol are called in order */
    int n_of_this_type_in_a_row;

    /* number of times functions from this module are called
     * in the collective operation. */
    int n_of_this_type_in_collective;
    int index_of_this_type_in_collective;

};
typedef struct mca_coll_ml_utility_data_t mca_coll_ml_utility_data_t;


/* forward declaration */
struct mca_coll_ml_collective_operation_progress_t;
struct mca_coll_ml_task_status_t;

typedef int (* mca_coll_ml_process_op_fn_t)
    (struct mca_coll_ml_collective_operation_progress_t *coll_op);

typedef int (* mca_coll_ml_task_comp_fn_t)
    (struct mca_coll_ml_task_status_t *coll_op);

typedef int (* mca_coll_ml_fragment_launch_fn_t)
    ( struct mca_coll_ml_collective_operation_progress_t *coll_op);

typedef int (* mca_coll_ml_sequential_task_setup_fn_t)
    ( struct mca_coll_ml_collective_operation_progress_t *coll_op);
/* This data structure defines the dependencies for a given
 * compound operation.  We will use this as a basis for implementing
 * collective operations.
 */
struct mca_coll_ml_compound_functions_t {
    /* label */
    char fn_name[COLL_ML_FN_NAME_LEN];

    /* hierarchy level that is used for this bcol */
    int h_level;

    /* the list of functions that make up this task */
    /* coll_bcol_collective_description_t *bcol_function; */
    mca_bcol_base_coll_fn_desc_t *bcol_function;
    /* task completion function for this compound function */
    mca_coll_ml_task_comp_fn_t task_comp_fn;

    /* module specific information that is a constant on a per group
     * basis
     */
    mca_coll_ml_utility_data_t constant_group_data;

    /* number of dependencies to be satified before these function can be
     * started */
    int num_dependencies;

    /*
     * number of notifications to perform on completion.  The assumption
     * is that a counter will be incremented.
     */
    int num_dependent_tasks;

    /*
     * pointers to counters that need be updated.  This assumes
     * an array of tasks is used to describe the ML level
     * collective operation, with these indecies referencing elements
     * in this array.
     */
    int *dependent_task_indices;

};

typedef struct mca_coll_ml_compound_functions_t mca_coll_ml_compound_functions_t;

/* Forward declaration for operation_description_t */
struct mca_coll_ml_module_t;

enum {
    COLL_ML_GENERAL_TASK_FN,
    COLL_ML_ROOT_TASK_FN,
    COLL_ML_MAX_TASK_FN
};

enum {
    SEQ_TASK_NOT_STARTED,
    SEQ_TASK_PENDING,
    SEQ_TASK_IN_PROG
};

typedef void (*mca_coll_ml_task_setup_fn_t) (struct mca_coll_ml_task_status_t *task_status, int index, struct mca_coll_ml_compound_functions_t *func);

/*
 * Collective operation definition
 */
struct mca_coll_ml_collective_operation_description_t {

    /*
     * Type of collective opeartion - there are two types:
     * 1) sequential progress through the collectives is sufficient
     * 2) general treatment, popping tasks onto execution queus is needed.
     */
    int progress_type;

    struct mca_coll_ml_topology_t *topo_info;

    /*
     * number of functions in collective operation
     */
    int n_fns;

    /*
     * list of functions
     */
    mca_coll_ml_compound_functions_t *component_functions;

    /*
     * array of lists of functions
     */
    mca_coll_ml_compound_functions_t **comp_fn_arr;

    /*
     * indices into the list - fixes a sequential schedule
     */
    int *sch_idx;

    /*
     * Task setup functions, so far we have only 3 - root and non-root
     */
    mca_coll_ml_task_setup_fn_t task_setup_fn[COLL_ML_MAX_TASK_FN];

    /* number of functions are called for bcols need ordering */
    int n_fns_need_ordering;
};
typedef struct mca_coll_ml_collective_operation_description_t
               mca_coll_ml_collective_operation_description_t;

/* Data structure used to track the state of individual bcol
 * functions.  This is used to track dependencies and completion
 * to progress the ML level function correctly.
 *
 * mca_coll_ml_task_status_t will be associated with an
 * mca_coll_ml_collective_operation_progress_t structure for
 * the duration of the lifetime of a communicator.
 * An array of task statuses will be stored with
 * the mca_coll_ml_collective_operation_progress_t data structure, so
 * that the taks status elements do not need to be moved back to
 * a free list before they are re-used.  When the ML level function
 * is complete, all mca_coll_ml_task_status_t are available for
 * re-use.
 */
struct mca_coll_ml_task_status_t{
    /* need to move this between lists to progress this correctly */
    opal_list_item_t item;

    /* number of dependencies satisfied */
    int n_dep_satisfied;

    /* ***************************************************************
     * Pasha:
     * I'm adding to the status: num_dependencies, num_dependent_tasks and
     * dependent_task_indices. The information originally resided on mca_coll_ml_compound_functions_t.
     * For collective operation with static nature it is not problem.
     * But for Bcast operation, where run time parameters, like root, actually
     * define the dependency. rt prefix mean run-time.
     */

    /* number of dependencies to be satisfied before these function can be
     * started */
    int rt_num_dependencies;

    /*
     * number of notifications to perform on completion.  The assumption
     * is that a counter will be incremented.
     */
    int rt_num_dependent_tasks;

    /*
     * pointers to counters that need be updated.  This assumes
     * an array of tasks is used to describe the ML level
     * collective operation, with these indecies referencing elements
     * in this array.
     */
    int *rt_dependent_task_indices;
    /*
     *
     * ***************************************************************/

    /* index in collective schedule */
    int my_index_in_coll_schedule;

    /* function pointers */
    mca_bcol_base_coll_fn_desc_t *bcol_fn;

    /* association with a specific collective task - the ML
     * mca_coll_ml_collective_operation_progress_t stores the
     * specific function parameters */
    struct mca_coll_ml_collective_operation_progress_t *ml_coll_operation;

    mca_coll_ml_task_comp_fn_t task_comp_fn;
};
typedef struct mca_coll_ml_task_status_t mca_coll_ml_task_status_t;

typedef enum mca_coll_ml_pending_type_t {
    REQ_OUT_OF_ORDER = 1,
    REQ_OUT_OF_MEMORY = 1 << 1
} mca_coll_ml_pending_type_t;

/* Forward declaration */
struct mca_bcol_base_payload_buffer_desc_t;
/* Data structure used to track ML level collective operation
 * progress.
 */
struct mca_coll_ml_collective_operation_progress_t {
    /* need this to put on a list properly */
    /* Full message information */
    struct full_message_t {
        /* make this a list item */
        ompi_request_t super;
        /* Next expected fragment.
         * It used for controling order of converter unpack operation */
        size_t next_expected_index;
        /* Pointer to last intilized fragment.
         * It used for controling order of converter unpack operation */
        struct mca_coll_ml_collective_operation_progress_t *last_started_frag;
        /* destination data address in user memory */
        void *dest_user_addr;
        /* source data address in user memory */
        void *src_user_addr;
        /* total message size */
        size_t n_bytes_total;
        /* per-process total message size - relevant for operations
         * such as gather and scatter, where each rank has it's
         * own unique data
         */
        size_t n_bytes_per_proc_total;
        size_t max_n_bytes_per_proc_total;
        /* data processes - from a local perspective */
        size_t n_bytes_delivered;
        /* current offset - where to continue with next fragment */
        size_t n_bytes_scheduled;
        /* number of fragments needed to process this message */
        size_t n_fragments;
        /* number of active frags */
        int n_active;
        /* actual pipeline depth */
        int pipeline_depth;
        /* am I the real root of the collective ? */
        bool root;
        /* collective fragment launcher */
        mca_coll_ml_fragment_launch_fn_t fragment_launcher;
        /* is data contingous */
        bool send_data_continguous;
        bool recv_data_continguous;
        /* data type count */
        int64_t send_count;
        int64_t recv_count;
        /* extent of the data types */
        size_t send_extent;
        size_t recv_extent;
        /* send data type */
        struct ompi_datatype_t * send_data_type;
        /* needed for non-contigous buffers */
        size_t offset_into_send_buffer;
        /* receive data type */
        struct ompi_datatype_t * recv_data_type;
        /* needed for non-contigous buffers */
        size_t offset_into_recv_buffer;
        /* Convertors for non contigous data */
        opal_convertor_t send_convertor;
        opal_convertor_t recv_convertor;
        /* Will be used by receiver for #bytes calc in the next frag */
        opal_convertor_t dummy_convertor;
        size_t dummy_conv_position;
        /* Size of packed data */
        size_t send_converter_bytes_packed;
        size_t recv_converter_bytes_packed;
        /* In case if ordering is needed: order num for next frag */
        int next_frag_num;
        /* The variable is used by non-blocking memory synchronization code
         * for caching bank index */
        int bank_index_to_recycle;
        /* need a handle for collective progress e.g. alltoall*/
        bcol_fragment_descriptor_t frag_info;
    } full_message;

    /* collective operation being progressed */
    mca_coll_ml_collective_operation_description_t *coll_schedule;
    /* */
    mca_coll_ml_process_op_fn_t process_fn;

    mca_coll_base_module_t *coll_module;

    /* If not null , we have to release next fragment */
    struct mca_coll_ml_collective_operation_progress_t *next_to_process_frag;
    /* pointer to previous fragment */
    struct mca_coll_ml_collective_operation_progress_t *prev_frag;
    /* This flag marks that the fragment is pending on the waiting
     * to be processed prior to recycling
     */
    enum mca_coll_ml_pending_type_t pending;

    /* Fragment data */
    struct fragment_data_t {
        /* current buffer pointer - offset (in bytes) into the user data */
        size_t offset_into_user_buffer;
        size_t offset_into_user_buffer_per_proc;

        /* amount of data (in bytes) in this fragment - amount of data
         * actually processed */
        size_t fragment_size;
        size_t per_rank_fragment_size;
        size_t data_type_count_per_frag;

        /* pointer to full message progress data */
        struct full_message_t *message_descriptor;

        /* ML buffer descriptor attached to this buffer */
        struct mca_bcol_base_payload_buffer_desc_t *buffer_desc;
        /* handle for collective progress, e.g. alltoall */
        bcol_fragment_descriptor_t bcol_fragment_desc;

        /* Which collective algorithm */
        int current_coll_op;
    } fragment_data;

    /* specific function parameters */
    /* the assumption is that the variable parameters passed into
     * the ML level function will persist until the collective operation
     * is complete.  For a blocking function this is until the collective
     * function is exited, and for nonblocking collective functions this
     * is until test or wait completes the collective.
     */
    int global_root;
    bcol_function_args_t variable_fn_params;

    struct{
        /* current active function - for sequential algorithms */
        int current_active_bcol_fn;

        /* current function status - not started, or in progress.
         * When the routine has completed, the active bcol index is
         * incremented, so no need to keep track of a completed
         * status.
         */
        int current_bcol_status;

        /* use this call back to setup algorithm specific info
           after each level necessary
          */
       mca_coll_ml_sequential_task_setup_fn_t seq_task_setup;

    } sequential_routine;

    struct{
        /*
         * BCOL function status - individual elements will be posted to
         * ml level component queues, as appropriate.
         */
        mca_coll_ml_task_status_t *status_array;

        /* number of completed tasks - need this for collective completion.
         * Resource completion is tracked by each BCOL module .
         */
        int num_tasks_completed;
    } dag_description;
};
typedef struct mca_coll_ml_collective_operation_progress_t
mca_coll_ml_collective_operation_progress_t;
OBJ_CLASS_DECLARATION(mca_coll_ml_collective_operation_progress_t);

#define OP_ML_MODULE(op) ((mca_coll_ml_module_t *)((op)->coll_module))
#define GET_COMM(op) ((OP_ML_MODULE(op))->comm)
#define IS_COLL_SYNCMEM(op) (ML_MEMSYNC == op->fragment_data.current_coll_op)

#define CHECK_AND_RECYCLE(op)                                                   \
do {                                                                            \
    if (0 == (op)->pending) {                                                   \
        /* Caching 2 values that we can't to touch on op after returing it */   \
        /* back to the free list  (free list may release memory on distruct )*/ \
        struct ompi_communicator_t *comm = GET_COMM(op);                        \
        bool is_coll_sync = IS_COLL_SYNCMEM(op);                                \
        ML_VERBOSE(10, ("Releasing %p", op));                                   \
        OMPI_REQUEST_FINI(&(op)->full_message.super);                           \
        opal_free_list_return (&(((mca_coll_ml_module_t *)(op)->coll_module)->  \
                                 coll_ml_collective_descriptors),               \
                               (opal_free_list_item_t *)op);                    \
        /* Special check for memory synchronization completion */               \
        /* We have to return it first to free list, since the communicator  */  \
        /* release potentially may trigger ML module distraction and having */  \
        /* the element not on the list may cause memory leak.               */  \
        if (OPAL_UNLIKELY(is_coll_sync)) {                                      \
            if (OMPI_COMM_IS_INTRINSIC(comm)) {                                 \
                opal_show_help("help-mpi-coll-ml.txt",                          \
                               "coll-ml-check-fatal-error", true,               \
                               comm->c_name);                                   \
                ompi_mpi_abort(comm, 6);                                        \
            } else {                                                            \
                opal_show_help("help-mpi-coll-ml.txt",                          \
                               "coll-ml-check-error", true,                     \
                               comm->c_name);                                   \
                /* After this point it is UNSAFE to touch ml module */          \
                /* or communicator */                                           \
                OBJ_RELEASE(comm);                                              \
            }                                                                   \
        }                                                                       \
    }                                                                           \
} while (0)

#define MCA_COLL_ML_SET_ORDER_INFO(coll_progress, num_frags)                      \
do {                                                                              \
    mca_coll_ml_topology_t *topo = (coll_progress)->coll_schedule->topo_info;     \
    bcol_function_args_t *variable_params = &(coll_progress)->variable_fn_params; \
    if (topo->topo_ordering_info.num_bcols_need_ordering > 0) {                   \
        variable_params->order_info.bcols_started = 0;                            \
        variable_params->order_info.order_num =                                   \
                      topo->topo_ordering_info.next_order_num;                    \
        variable_params->order_info.n_fns_need_ordering =                         \
                       (coll_progress)->coll_schedule->n_fns_need_ordering;       \
        topo->topo_ordering_info.next_order_num += num_frags;                     \
        (coll_progress)->fragment_data.message_descriptor->next_frag_num =        \
                                      variable_params->order_info.order_num + 1;  \
    }                                                                             \
} while (0)

#define MCA_COLL_ML_SET_NEW_FRAG_ORDER_INFO(coll_progress)                                    \
do {                                                                                          \
    mca_coll_ml_topology_t *topo = (coll_progress)->coll_schedule->topo_info;                 \
    if (topo->topo_ordering_info.num_bcols_need_ordering > 0) {                               \
        bcol_function_args_t *variable_params = &(coll_progress)->variable_fn_params;         \
        struct fragment_data_t *frag_data = &(coll_progress)->fragment_data;                  \
        variable_params->order_info.bcols_started = 0;                                        \
        variable_params->order_info.order_num = frag_data->message_descriptor->next_frag_num; \
        variable_params->order_info.n_fns_need_ordering =                                     \
                       (coll_progress)->coll_schedule->n_fns_need_ordering;                   \
        frag_data->message_descriptor->next_frag_num++;                                       \
    }                                                                                         \
} while (0)

#define MCA_COLL_ML_SET_SCHEDULE_ORDER_INFO(schedule)                           \
do {                                                                            \
    int i;                                                                      \
    (schedule)->n_fns_need_ordering = 0;                                        \
    for (i = 0; i < (schedule)->n_fns; ++i) {                                   \
        mca_bcol_base_module_t *current_bcol =                                  \
            (schedule)->component_functions[i].constant_group_data.bcol_module; \
        assert (NULL != current_bcol);                                          \
        if (current_bcol->bcol_component->need_ordering) {                      \
            (schedule)->n_fns_need_ordering++;                                  \
        }                                                                       \
    }                                                                           \
} while (0)

enum {
    MCA_COLL_ML_NET_STREAM_SEND,
    MCA_COLL_ML_NET_STREAM_RECV
};

static inline  __opal_attribute_always_inline__
    int mca_coll_ml_convertor_prepare(ompi_datatype_t *dtype, int count, void *buff,
                                            opal_convertor_t *convertor, int stream)
{
    size_t bytes_packed;

    if (MCA_COLL_ML_NET_STREAM_SEND == stream) {
        opal_convertor_copy_and_prepare_for_send(
                ompi_mpi_local_convertor,
                &dtype->super, count, buff, 0,
                convertor);
    } else {
        opal_convertor_copy_and_prepare_for_recv(
                ompi_mpi_local_convertor,
                &dtype->super, count, buff, 0,
                convertor);
    }

    opal_convertor_get_packed_size(convertor, &bytes_packed);

    return bytes_packed;
}

static inline  __opal_attribute_always_inline__
    int mca_coll_ml_convertor_pack(void *data_addr, size_t buff_size,
                                            opal_convertor_t *convertor)
{
    struct iovec iov;

    size_t max_data = 0;
    uint32_t iov_count = 1;

    iov.iov_base = (IOVBASE_TYPE*) data_addr;
    iov.iov_len  = buff_size;

    opal_convertor_pack(convertor, &iov, &iov_count, &max_data);

    return max_data;
}

static inline  __opal_attribute_always_inline__
    int mca_coll_ml_convertor_unpack(void *data_addr, size_t buff_size,
                                            opal_convertor_t *convertor)
{
    struct iovec iov;

    size_t max_data = 0;
    uint32_t iov_count = 1;

    iov.iov_base = (void *) (uintptr_t) data_addr;
    iov.iov_len  = buff_size;

    opal_convertor_unpack(convertor, &iov, &iov_count, &max_data);

    return max_data;
}
#endif /* MCA_COLL_ML_COLLS_H */

