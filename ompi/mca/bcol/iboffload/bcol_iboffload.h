/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BCOL_IBOFFLOAD_H
#define MCA_BCOL_IBOFFLOAD_H

#include "ompi_config.h"

#include <stdio.h>
#include <assert.h>

#include <infiniband/mqe.h>
#include <infiniband/verbs.h>
#include <infiniband/mverbs.h>

#include "opal/mca/mca.h"

#include "ompi/op/op.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/datatype/ompi_datatype_internal.h"

#include "ompi/mca/coll/ml/coll_ml.h"

#include "ompi/mca/bcol/bcol.h"
#include "ompi/mca/bcol/base/base.h"

#include "ompi/mca/sbgp/ibnet/sbgp_ibnet.h"

#include "ompi/mca/mpool/mpool.h"
#include "ompi/request/request.h"

#include "ompi/mca/common/ofacm/connect.h"
#include "ompi/mca/common/netpatterns/common_netpatterns.h"

#include "bcol_iboffload_qp_info.h"

BEGIN_C_DECLS

#define IMM_RDMA 1
#define INLINE 1
#define NO_INLINE 0

#define MCA_IBOFFLOAD_CALC_SIZE_EXT 8
#define MCA_IBOFFLOAD_IB_DRIVER_OPERAND_SIZE 8
#define MCA_IBOFFLOAD_CACHE_LINE_SIZE 128

#if OMPI_HAVE_IBOFFLOAD_CALC_RDMA
#define MCA_BCOL_IBOFFLOAD_SEND_CALC IBV_M_WR_CALC_SEND
#else
#define MCA_BCOL_IBOFFLOAD_SEND_CALC IBV_M_WR_CALC
#endif


/* 0 - barrier rdma info
   1 - ML rdma info */
#define MAX_REMOTE_RDMA_INFO 2

/* forward declarations */
struct mca_bcol_iboffload_module_t;
struct mca_bcol_iboffload_collreq_t;
struct mca_bcol_iboffload_endpoint_t;
struct mca_bcol_iboffload_frag_t;
struct mca_bcol_iboffload_task_t;
struct mca_bcol_iboffload_qp_info_t;
struct mca_bcol_iboffload_collfrag_t;
struct mca_bcol_iboffload_algth_lst_t;
struct mca_bcol_iboffload_device_t;

typedef int (*mca_bcol_iboffload_coll_algth_fn_t) (
        struct mca_bcol_iboffload_module_t *iboffload,
        struct mca_bcol_iboffload_collreq_t *coll_request);

struct mca_bcol_iboffload_rdma_info_t {
    uint64_t    addr;
    uint32_t    rkey;
    uint32_t    lkey;
};
typedef struct mca_bcol_iboffload_rdma_info_t mca_bcol_iboffload_rdma_info_t;

struct mca_bcol_iboffload_rdma_buffer_desc_t {
    void     *data_addr;             /* buffer address */
    uint64_t     generation_number;  /* my generation */
    uint64_t     bank_index;         /* my bank */
    uint64_t     buffer_index;       /* my buff index */
};
typedef struct mca_bcol_iboffload_rdma_buffer_desc_t mca_bcol_iboffload_rdma_buffer_desc_t;

struct mca_bcol_iboffload_rdma_block_desc_t {
    /* number of memory banks */
    uint32_t     num_banks;
    /* number of buffers per bank */
    uint32_t     num_buffers_per_bank;
    /* size of a payload buffer */
    uint32_t     size_buffer;
    /* data offset from ML */
    uint32_t     data_offset;
    /* pointer to buffer descriptors initialized */
    mca_bcol_iboffload_rdma_buffer_desc_t *rdma_desc;
};
typedef struct mca_bcol_iboffload_rdma_block_desc_t mca_bcol_iboffload_rdma_block_desc_t;

/* Information that we need to keep in order to access remote
   memory. For each remote peer (endpoint) we will keep this
   structure */
struct mca_bcol_iboffload_rem_rdma_block_t {
    /* IB related information first */
    mca_bcol_iboffload_rdma_info_t ib_info;

    mca_bcol_iboffload_rdma_buffer_desc_t *rdma_desc;
};
typedef struct mca_bcol_iboffload_rem_rdma_block_t mca_bcol_iboffload_rem_rdma_block_t;

enum {
    MCA_BCOL_IBOFFLOAD_BK_COUNTER_INDEX = 0,
    MCA_BCOL_IBOFFLOAD_BK_SYNC_INDEX,
    MCA_BCOL_IBOFFLOAD_BK_LAST
};

/* Information that we need to keep in order to access and
   track local memory that is used as source and destinatination
   for RDMA operations */
struct mca_bcol_iboffload_local_rdma_block_t {
    /* sync counter keeps next to start bank id */
    int sync_counter;
    /* Counter for released ml buffers */
    int *bank_buffer_counter[MCA_BCOL_IBOFFLOAD_BK_LAST];
    /* IB related information first */
    struct mca_bcol_iboffload_rdma_info_t ib_info;
    /* back pointer to original ML memory descriptor */
    struct ml_memory_block_desc_t *ml_mem_desc;
    /* Pasha: do we really need this one ?*/
    /* caching ml memory descriptor configurations localy */
    mca_bcol_iboffload_rdma_block_desc_t bdesc;
};
typedef struct mca_bcol_iboffload_local_rdma_block_t mca_bcol_iboffload_local_rdma_block_t;

struct mca_bcol_iboffload_recv_wr_manager {
    opal_mutex_t lock;
    /** Array of ready to use receive work requests.
     * it is 2 dimensional array since for each
     * qp size we want to keep separate recv wr  */
    struct ibv_recv_wr **recv_work_requests;
};
typedef struct mca_bcol_iboffload_recv_wr_manager mca_bcol_iboffload_recv_wr_manager;

/**
 * Structure to hold the basic shared memory coll component.  First it holds the
 * base coll component, and then holds a bunch of
 * sm-coll-component-specific stuff (e.g., current MCA param
 * values).
 */
struct mca_bcol_iboffload_component_t {
    /** Base coll component */
    mca_bcol_base_component_2_0_0_t super;
    /** Enable disable verbose mode */
    int verbose;
    int num_qps;
    /** Whether we want a warning if non default GID prefix is not configured
      on multiport setup */
    bool warn_default_gid_prefix;
    /** Whether we want a warning if the user specifies a non-existent
      device and/or port via bcol_ibofflad_if_[in|ex]clude MCA params */
    bool warn_nonexistent_if;
    /** initial size of free lists */
    int free_list_num;
    /** maximum size of free lists */
    int free_list_max;
    /** number of elements to alloc when growing free lists */
    int free_list_inc;
    /** name of ib memory pool */
    char* mpool_name;
    /** max outstanding CQE on the CQ */
    uint32_t cq_size;
    /** Max size of inline data */
    uint32_t max_inline_data;
    /** IB partition definition */
    uint32_t pkey_val;
    /** Outstanding atomic reads */
    uint32_t qp_ous_rd_atom;
    /** IB MTU */
    uint32_t mtu;
    /** Recv not ready timer */
    uint32_t min_rnr_timer;
    /** IB timeout */
    uint32_t timeout;
    /** IB retry count */
    uint32_t retry_count;
    /** Recv not ready retry count */
    uint32_t rnr_retry;
    /** IB maximum pending RDMA */
    uint32_t max_rdma_dst_ops;
    /** IB Service level (QOS) */
    uint32_t service_level;
    /** number of iboffload modules that we want to open per single lid */
    uint32_t bcols_per_lid;
    /** Max LMCs that we want to support */
    uint32_t max_lmc;
    /** Max number of bcols */
    uint32_t max_bcols;
    /** Use the async event handler */
    uint32_t use_async_event_thread;
    /** Preferred communication buffer alignment in Bytes (must be power of two) */
    uint32_t buffer_alignment;
    /** Max tasks number for MQ */
    uint32_t max_mqe_tasks;
    /** Max MQ size */
    uint32_t max_mq_size;
    /** Memory fragment size */
    uint32_t frag_size;
    /** HCA/Port include exclude list */
    char *if_include;
    char **if_include_list;
    char *if_exclude;
    char **if_exclude_list;
    /** Dummy argv-style list; a copy of names from the
        if_[in|ex]clude list that we use for error checking (to ensure
        that they all exist) */
    char **if_list;
    /** Array of ibv devices */
    struct ibv_device **ib_devs;
    /** devices count */
    int num_devs;
    /** MCA param bcol_iboffload_receive_queues */
    char *receive_queues;
    /** Common info about all kinds of QPs on each iboffload module */
    struct mca_bcol_iboffload_qp_info_t qp_infos[MCA_BCOL_IBOFFLOAD_QP_LAST];
    /** Array of iboffload devices */
    opal_pointer_array_t devices;
    /** Free lists of collfrag descriptors */
    ompi_free_list_t collfrags_free;
    /** Free lists of outstanding collective operations */
    ompi_free_list_t collreqs_free;
    /** Free lists for free task operations */
    ompi_free_list_t tasks_free;
    /** Free lists for free calc task operations */
    ompi_free_list_t calc_tasks_free;
    /** Free list of empty frags, that do not keep any
      registration information */
    ompi_free_list_t ml_frags_free;
    /** Recv work request mananger */
    mca_bcol_iboffload_recv_wr_manager recv_wrs;
    /** We allocate some resources on the component
      * with creating of the first iboffload module
      * and set this flag to true */
    bool init_done;
    /** Maximal number of fragments of the same colective request that can be sent in parallel */
    uint32_t max_pipeline_depth;
    /** array mapping Open MPI reduction operators to MVerbs reduction operators */
    enum ibv_m_wr_calc_op map_ompi_to_ib_calcs[OMPI_OP_NUM_OF_TYPES];
    /** array mapping Open MPI data types to MVerbs data types */
    enum ibv_m_wr_data_type map_ompi_to_ib_dt[OMPI_DATATYPE_MPI_MAX_PREDEFINED];
    /** The last ib offload calculation will be done by the cpu */
    bool last_calc_in_cpu;
    /** The last ib offload calculation will be done by the cpu */
    bool enable_rdma_calc;
    /** The order of the exchange tree */
    int exchange_tree_order;
    /** Knomial tree order */
    int knomial_tree_order;
    /** K-nomial radix */
    int k_nomial_radix;
    /** Maximum number of pulls for completion check */
    int max_progress_pull;
    /** Barrier function selector */
    int barrier_mode;
    /** MCA for selecting Bruck's alltoall algorithms */
    int use_brucks_smsg_alltoall_rdma;
    int use_brucks_smsg_alltoall_sr;
    /** radix of small-data alltoall Bruck-like algorithm */
    int k_alltoall_bruck_radix;
    /** alltoall small data buffer alignment */
    int tmp_buf_alignment;
};

/**
 * Convenience typedef
 */
typedef struct mca_bcol_iboffload_component_t mca_bcol_iboffload_component_t;

/* List of all algorithms that we use */
enum {
    FANIN_ALG,
    FANOUT_ALG,
    RECURSIVE_DOUBLING_BARRIER_ALG,
    RECURSIVE_KNOMIAL_BARRIER_ALG,
    RECURSIVE_DOUBLING_ALLREDUCE_ALG,
    RECURSIVE_DOUBLING_REDUCE_ALG,
    RECURSIVE_DOUBLING_TREE_BCAST,
    ALL_ENDPOINTS, /* connected to all peers */
    ALLGATHER_KNOMIAL_ALG,
    ALLGATHER_NEIGHBOR_ALG,
    REMOTE_EXCHANGE_ALG,
    LAST_ALG
};

struct mca_bcol_iboffload_port_t {
    int             id;         /** Port number on device: 1 or 2 */
    int             stat;       /** Port status - Active,Init,etc.. */
    enum ibv_mtu    mtu;        /** MTU on this port */
    uint64_t        subnet_id;  /** Sunnet id for the port */
    uint16_t        lid;
    uint16_t        lmc;
};
typedef struct mca_bcol_iboffload_port_t mca_bcol_iboffload_port_t;

enum {
    COLL_MQ  = 0,
    SERVICE_MQ,
    BCOL_IBOFFLOAD_MQ_NUM
};

struct mca_bcol_iboffload_module_t {
    /* base structure */
    mca_bcol_base_module_t super;

    /* size */
    int group_size;
    int log_group_size;

    /* size of each memory segment */
    size_t segment_size;

    /* collective tag */
    long long collective_tag;

    /* pointer to device */
    struct mca_bcol_iboffload_device_t *device;

    /* caching port number */
    uint32_t port;

    /* Connecting iboffload with ibnet module information */
    /* pointer to sbgp ibnet */
    mca_sbgp_ibnet_module_t *ibnet;

    /* connection group inder for the ibnet */
    int cgroup_index;

    /* array of endpoints */
    struct mca_bcol_iboffload_endpoint_t **endpoints;

    /* Size of the endpoints array */
    int num_endpoints;

    /* caching port subnet id and lid
     * the same information we have on device */
    uint64_t        subnet_id;
    uint16_t        lid;

    /* Pointer to management queue */
    struct mqe_context *mq[BCOL_IBOFFLOAD_MQ_NUM];
    int mq_credit[BCOL_IBOFFLOAD_MQ_NUM];

    /* pending list of collfrags */
    opal_list_t collfrag_pending;

    /* recursive-doubling tree node */
    mca_common_netpatterns_pair_exchange_node_t recursive_doubling_tree;

    /* N exchange tree */
    mca_common_netpatterns_pair_exchange_node_t n_exchange_tree;

    /* Knomial exchange tree */
    mca_common_netpatterns_k_exchange_node_t knomial_exchange_tree;

    /* Knomial exchange tree */
    mca_common_netpatterns_k_exchange_node_t knomial_allgather_tree;

    /* The array will keep pre-calculated task consumption per
     * algorithm
     */
    uint32_t alg_task_consump[LAST_ALG];

    /* Pointer to a func that's implementation of a barrier algorithm */
    mca_bcol_iboffload_coll_algth_fn_t barrier_algth;

    /* Pointer to a func that's implementation of a fanin algorithm */
    mca_bcol_iboffload_coll_algth_fn_t fanin_algth;

    /* Pointer to a func that's implementation of a fanin algorithm */
    mca_bcol_iboffload_coll_algth_fn_t fanout_algth;

    /* Pointer to a func that's implementation of a allreduce algorithm */
    mca_bcol_iboffload_coll_algth_fn_t allreduce_algth;

    /* Pointer to a func that's implementation of a non blocking memory syncronization algorithm */
    mca_bcol_iboffload_coll_algth_fn_t memsync_algth;

    /* rdma block memory information */
    mca_bcol_iboffload_local_rdma_block_t rdma_block;

    /* The largest power of two which 1 << power_of_2
       is not larger than the group size */
    int power_of_2;

    /* The largest power of two number which is not larger than the group size */
    int power_of_2_ranks;

    /* Connection status array */
    bool connection_status[LAST_ALG];

    /* map from communicator ranks to ibsubnet */
    int *comm_to_ibnet_map;

    /* order preserving value */
    int64_t prev_sequence_num;

    /* Temp iovec to send the data fragments -- alltoall Brucks */
    struct iovec *alltoall_iovec;
    struct iovec *alltoall_recv_iovec;

    /* tree radix for the knomial bruck small data alltoall */
    int k_alltoall_bruck_radix;

    /* Temp buffer alignment for knomial bruck small data alltoall */
    int tmp_buf_alignment;

    /* Free task list with sge's array */
    ompi_free_list_t iovec_tasks_free;
};

typedef struct mca_bcol_iboffload_module_t mca_bcol_iboffload_module_t;
OBJ_CLASS_DECLARATION(mca_bcol_iboffload_module_t);

/**
 * Global component instance
 */
OMPI_MODULE_DECLSPEC
    extern mca_bcol_iboffload_component_t mca_bcol_iboffload_component;

static inline int mca_bcol_iboffload_err(const char* fmt, ...)
{
    va_list list;
    int ret;

    va_start(list, fmt);
    ret = vfprintf(stderr, fmt, list);
    va_end(list);
    return ret;
}

#define MCA_BCOL_IBOFFLOAD_ALLREDUCE_DO_CALC(ompi_op, c_type, l_operand, r_operand, result) \
do {                                                                                        \
    switch (ompi_op) {                                                                      \
        case OMPI_OP_MAX:                                                                   \
            *((c_type *)&result) = ((*(c_type *)&(l_operand) > *(c_type *)&(r_operand)) ?   \
                                     *(c_type *)&(l_operand) : *(c_type *)&(r_operand));    \
            break;                                                                          \
        case OMPI_OP_MIN:                                                                   \
            *((c_type *)&result) = ((*(c_type *)&(l_operand) < *(c_type *)&(r_operand)) ?   \
                                     *(c_type *)&(l_operand) : *(c_type *)&(r_operand));    \
            break;                                                                          \
        case OMPI_OP_SUM:                                                                   \
            *((c_type *)&result) = (*((c_type *)&(l_operand)) + *((c_type *)&(r_operand))); \
            break;                                                                          \
        default:                                                                            \
            break;                                                                          \
    }                                                                                       \
} while (0);

#define MCA_BCOL_IBOFFLOAD_PKEY_MASK 0x7fff
#define MCA_BCOL_IBOFFLOAD_DEFAULT_GID_PREFIX 0xfe80000000000000ll

#define IBOFFLOAD_ERROR(args)                                       \
    do {                                                            \
        mca_bcol_iboffload_err("[%s]%s[%s:%d:%s] IBOFFLOAD ",       \
            ompi_process_info.nodename,                             \
            OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),                     \
            __FILE__, __LINE__, __func__);                          \
        mca_bcol_iboffload_err args;                                \
        mca_bcol_iboffload_err("\n");                               \
    } while(0)

#if OPAL_ENABLE_DEBUG
#define IBOFFLOAD_VERBOSE(level, args)                              \
    do {                                                            \
        if (mca_bcol_iboffload_component.verbose >= level) {        \
            mca_bcol_iboffload_err("[%s]%s[%s:%d:%s] IBOFFLOAD ",   \
                    ompi_process_info.nodename,                     \
                    OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),             \
                    __FILE__, __LINE__, __func__);                  \
            mca_bcol_iboffload_err args;                            \
            mca_bcol_iboffload_err("\n");                           \
        }                                                           \
    } while(0)
#else
#define IBOFFLOAD_VERBOSE(level, args)
#endif

#define MCA_BCOL_IBOFFLOAD_SET_COLL_REQ_LINKS(coll_req, coll_work_req) \
    do {                                                               \
        opal_list_append(&(coll_req)->work_requests,                   \
                        (opal_list_item_t*) (coll_work_req));          \
        (coll_work_req)->coll_full_req = (coll_req);                   \
    } while(0)
/* Vasily: will be removed soon */
#define APPEND_TO_TASKLIST(task_ptr_to_set, event, last_event_type)  \
    do {                                                             \
        *task_ptr_to_set = &(event)->element;                        \
        last_event_type = &(event)->element;                         \
        task_ptr_to_set = &((event)->element.next);                  \
    } while(0)

#define MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(task_ptr_to_set, task) \
    do {                                                              \
        *task_ptr_to_set = (task);                                    \
        task_ptr_to_set = &((task)->next_task);                       \
    } while(0)

#define MCA_BCOL_IBOFFLOAD_APPEND_MQ_TASK_TO_LIST(task_ptr_to_set, event) \
    do {                                                                  \
        *task_ptr_to_set = &(event)->element;                             \
        task_ptr_to_set = &((event)->element.next);                       \
    } while(0)

#define BCOL_IS_COMPLETED(req) (((req)->n_frag_mpi_complete == (req)->n_fragments) && \
                                ((req)->n_fragments > 0))

#define BCOL_AND_NET_ARE_COMPLETED(req) (BCOL_IS_COMPLETED(req) && \
                                        ((req)->n_frag_net_complete == (req)->n_fragments))

/* Pasha: Need to add locks here */
#define BCOL_IBOFFLOAD_MQ_HAVE_CREDITS(module, mq_index, num_of_credits) \
                (((module)->mq_credit[mq_index] -= (num_of_credits)) < 0 ? false : true)
/* Pasha: Need to add locks here */
#define BCOL_IBOFFLOAD_MQ_RETURN_CREDITS(module, mq_index, num_of_credits) \
                ((module)->mq_credit[mq_index] += (num_of_credits))

#define BCOL_IBOFFLOAD_IS_FIRST_CALL(args) (0 == (args)->index_in_consecutive_same_bcol_calls)

#define BCOL_IBOFFLOAD_IS_LAST_CALL(args) (((args)->n_of_this_type_in_collective - 1) == \
                                            (args)->index_of_this_type_in_collective)

#define BCOL_IBOFFLOAD_READY_TO_POST(args) (((args)->n_of_this_type_in_a_row - 1) == \
                                             (args)->index_in_consecutive_same_bcol_calls)
/*
 * bcol module functions
 */

int mca_bcol_iboffload_rec_doubling_start_connections(struct mca_bcol_iboffload_module_t *iboffload);

/* RDMA addr exchange with rem proc */
int mca_bcol_iboffload_exchange_rem_addr(struct mca_bcol_iboffload_endpoint_t *ep);

/* Progress function */
int mca_bcol_iboffload_component_progress(void);

/* Register memory */
int mca_bcol_iboffload_register_mr(void *reg_data, void * base, size_t size,
        mca_mpool_base_registration_t *reg);

/* Deregister memory */
int mca_bcol_iboffload_deregister_mr(void *reg_data, mca_mpool_base_registration_t *reg);

/*
 * The function is used for create CQ in this module.
 */
int mca_bcol_iboffload_adjust_cq(struct mca_bcol_iboffload_device_t *device,
                                 struct ibv_cq **ib_cq);
/*
 * Query to see if the component is available for use,
 * and can satisfy the thread and progress requirements
 */
int mca_bcol_iboffload_init_query(bool enable_progress_threads,
        bool enable_mpi_threads);


/* Interface to setup the allgather tree */
int mca_bcol_iboffload_setup_knomial_tree(mca_bcol_base_module_t *super);

/*
 * Query to see if the module is available for use on
 * the given communicator, and if so, what it's priority is.
 */
mca_bcol_base_module_t **
mca_bcol_iboffload_comm_query(mca_sbgp_base_module_t *sbgp, int *num_modules);

int
mca_bcol_iboffload_free_tasks_frags_resources(
        struct mca_bcol_iboffload_collfrag_t *collfrag,
        ompi_free_list_t *frags_free);

/**
 * Shared memory blocking barrier
 */

int mca_bcol_iboffload_small_msg_bcast_intra(bcol_function_args_t *fn_arguments,
                                                   struct coll_ml_function_t
                                                   *const_args);

int mca_bcol_iboffload_barrier_intra_recursive_doubling_start(
        struct mca_bcol_iboffload_module_t *iboffload,
        struct mca_bcol_iboffload_collreq_t *coll_request);

int mca_bcol_iboffload_barrier_intra_recursive_knomial_start(
        struct mca_bcol_iboffload_module_t *iboffload,
        struct mca_bcol_iboffload_collreq_t *coll_request);

int mca_bcol_iboffload_barrier_intra_recursive_doubling(
        mca_bcol_iboffload_module_t *iboffload,
        struct mca_bcol_iboffload_collreq_t *coll_request);

int mca_bcol_iboffload_nb_memory_service_barrier_start(
        struct mca_bcol_iboffload_module_t *iboffload,
        struct mca_bcol_iboffload_collreq_t *coll_request);

int mca_bcol_iboffload_allreduce_first_call(
                struct mca_bcol_iboffload_module_t *iboffload,
                struct mca_bcol_iboffload_collreq_t *coll_request);

int mca_bcol_iboffload_fanin_register(mca_bcol_base_module_t *super);
int mca_bcol_iboffload_fanout_register(mca_bcol_base_module_t *super);
int mca_bcol_iboffload_barrier_register(mca_bcol_base_module_t *super);
int mca_bcol_iboffload_memsync_register(mca_bcol_base_module_t *super);
int mca_bcol_iboffload_allreduce_register(mca_bcol_base_module_t *super);

int mca_bcol_iboffload_new_style_fanin_first_call(
                mca_bcol_iboffload_module_t *iboffload,
                struct mca_bcol_iboffload_collreq_t *coll_request);

int mca_bcol_iboffload_new_style_fanout_first_call(
                mca_bcol_iboffload_module_t *iboffload,
                struct mca_bcol_iboffload_collreq_t *coll_request);

int mca_bcol_iboffload_nb_memory_service_barrier_intra(bcol_function_args_t *input_args,
        struct coll_ml_function_t *const_args);

int mca_bcol_iboffload_coll_support_all_types(bcol_coll coll_name);
int mca_bcol_iboffload_coll_supported(int op, int dtype, bcol_elem_type elem_type);

static inline __opal_attribute_always_inline__ int
                        mca_bcol_iboffload_fls(int num)
{
    int i = 1;
    int j = 0;

    if (0 == num) {
        return 0;
    }

    while (i < num) {
        i <<= 1;
        j++;
    }

    if (i > num) {
        j--;
    }

   return j;
}

#define BCOL_IBOFFLOAD_IS_EVEN(num) (!((num) & 1))
static inline __opal_attribute_always_inline__ int
                        mca_bcol_iboffload_ffs(int num)
{
    int j = 0;

    if (0 == num) {
        return 0;
    }

    while (BCOL_IBOFFLOAD_IS_EVEN(num)) {
        num >>= 1;
        j++;
    }

   return j;
}

#if OPAL_ENABLE_DEBUG

/* Post task list MQ */
#define IS_IMM(a) (a & MQE_WR_FLAG_IMM_EXE)
#define IS_SIG(a) (a & MQE_WR_FLAG_SIGNAL)
#define IS_BLK(a) (a & MQE_WR_FLAG_BLOCK)

int task_to_rank(mca_bcol_iboffload_module_t *iboffload, struct mqe_task *task);
int wait_to_rank(mca_bcol_iboffload_module_t *iboffload, struct mqe_task *task);

#endif

/* MQ posting function */
static inline __opal_attribute_always_inline__ int
                 mca_bcol_iboffload_post_mqe_tasks(
                            mca_bcol_iboffload_module_t *iboffload,
                            struct mqe_task *head_mqe)
{
    int rc;
    struct mqe_task *bad_mqe = NULL;

#if OPAL_ENABLE_DEBUG /* debug code */

    struct mqe_task *curr_mqe_task = NULL;
    int send_count = 0, recv_count = 0, wait_count = 0;

    curr_mqe_task = head_mqe;
    IBOFFLOAD_VERBOSE(10, ("Processing MQE Head with addr %p <START>\n",
                          (uintptr_t) (void*) curr_mqe_task));

    while (NULL != curr_mqe_task) {
        switch(curr_mqe_task->opcode) {
            case MQE_WR_SEND:
                IBOFFLOAD_VERBOSE(10, ("Posting task %p id 0x%x: send on QP 0x%x\n"
                                   "rank %d, sg_entry: addr %p LEN %d lkey %u, flag[%d-%d-%d]\n",
                            (void*) curr_mqe_task, (uintptr_t) curr_mqe_task->wr_id,
                            curr_mqe_task->post.qp->qp_num,
                            task_to_rank(iboffload, curr_mqe_task),
                            curr_mqe_task->post.send_wr->sg_list->addr,
                            curr_mqe_task->post.send_wr->sg_list->length,
                            curr_mqe_task->post.send_wr->sg_list->lkey,
                            IS_IMM(curr_mqe_task->flags), IS_SIG(curr_mqe_task->flags), IS_BLK(curr_mqe_task->flags)));

                ++send_count;
                break;
            case MQE_WR_RECV:
                IBOFFLOAD_VERBOSE(10, ("Posting task %p id 0x%x: recv on QP 0x%x rank %d flag[%d-%d-%d]\n",
                        (void*) curr_mqe_task, (uintptr_t) curr_mqe_task->wr_id,
                        curr_mqe_task->post.qp->qp_num, task_to_rank(iboffload, curr_mqe_task),
                        IS_IMM(curr_mqe_task->flags), IS_SIG(curr_mqe_task->flags), IS_BLK(curr_mqe_task->flags)));

                ++recv_count;
                break;
            case MQE_WR_CQE_WAIT:

                IBOFFLOAD_VERBOSE(10, ("Posting task %p id %x: wait on CQ %p for rank %d num of waits %d flag[%d-%d-%d]\n",
                            (void*) curr_mqe_task, (uintptr_t) curr_mqe_task->wr_id,
                            (void*) curr_mqe_task->wait.cq, wait_to_rank(iboffload, curr_mqe_task),
                            curr_mqe_task->wait.count,
                            IS_IMM(curr_mqe_task->flags), IS_SIG(curr_mqe_task->flags), IS_BLK(curr_mqe_task->flags)));

                wait_count += curr_mqe_task->wait.count;
                break;
            default:
                IBOFFLOAD_ERROR(("Fatal error, unknow packet type %d\n",
                                                   curr_mqe_task->opcode));
                return OMPI_ERROR;
        }

        /* pointer to next task */
        curr_mqe_task = curr_mqe_task->next;
    }

    IBOFFLOAD_VERBOSE(10, ("wait[%d] send[%d] recv[%d]\n",
                            wait_count, send_count, recv_count));
#endif

    IBOFFLOAD_VERBOSE(10, ("Posting MQ %p <DONE>\n", (uintptr_t) head_mqe->wr_id));

    rc = mqe_post_task(iboffload->mq[0], head_mqe, &bad_mqe);
    if (OPAL_UNLIKELY(0 != rc)) {
        IBOFFLOAD_ERROR(("ibv_post_mqe failed, errno says: %s,"
                         " the return code is [%d]\n",
                         strerror(errno), rc));

        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

static inline __opal_attribute_always_inline__  
                                int lognum(int n) {
    int count = 1, lognum = 0;

    while (count < n) {
        count = count << 1;
        lognum++;
    }

    return lognum;
}

END_C_DECLS

#endif /* MCA_BCOL_IBOFFLOAD_H */

