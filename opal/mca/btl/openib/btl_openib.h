/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2009 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2006-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * Copyright (c) 2009-2010 Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2013-2014 NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2014      Bull SAS.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * @file
 */

#ifndef MCA_BTL_IB_H
#define MCA_BTL_IB_H

#include "opal_config.h"
#include <sys/types.h>
#include <string.h>
#include <infiniband/verbs.h>

/* Open MPI includes */
#include "opal/class/opal_pointer_array.h"
#include "opal/class/opal_hash_table.h"
#include "opal/util/arch.h"
#include "opal/util/output.h"
#include "opal/mca/event/event.h"
#include "opal/threads/threads.h"
#include "opal/mca/btl/btl.h"
#include "opal/mca/mpool/mpool.h"
#include "opal/mca/btl/base/btl_base_error.h"
#include "opal/mca/btl/base/base.h"

#include "connect/connect.h"

BEGIN_C_DECLS

#define HAVE_XRC (1 == OPAL_HAVE_CONNECTX_XRC)
#define ENABLE_DYNAMIC_SL (1 == OPAL_ENABLE_DYNAMIC_SL)

#define MCA_BTL_IB_LEAVE_PINNED 1
#define IB_DEFAULT_GID_PREFIX 0xfe80000000000000ll
#define MCA_BTL_IB_PKEY_MASK 0x7fff
#define MCA_BTL_OPENIB_CQ_POLL_BATCH_DEFAULT (256)


/*--------------------------------------------------------------------*/

#if OPAL_ENABLE_DEBUG
#define ATTACH() do { \
  int i = 0; \
  opal_output(0, "WAITING TO DEBUG ATTACH"); \
  while (i == 0) sleep(5); \
  } while(0);
#else
#define ATTACH()
#endif

/*--------------------------------------------------------------------*/

/**
 * Infiniband (IB) BTL component.
 */

typedef enum {
    MCA_BTL_OPENIB_TRANSPORT_IB,
    MCA_BTL_OPENIB_TRANSPORT_IWARP,
    MCA_BTL_OPENIB_TRANSPORT_RDMAOE,
    MCA_BTL_OPENIB_TRANSPORT_UNKNOWN,
    MCA_BTL_OPENIB_TRANSPORT_SIZE
} mca_btl_openib_transport_type_t;

typedef enum {
    MCA_BTL_OPENIB_PP_QP,
    MCA_BTL_OPENIB_SRQ_QP,
    MCA_BTL_OPENIB_XRC_QP
} mca_btl_openib_qp_type_t;

struct mca_btl_openib_pp_qp_info_t {
    int32_t rd_win;
    int32_t rd_rsv;
}; typedef struct mca_btl_openib_pp_qp_info_t mca_btl_openib_pp_qp_info_t;

struct mca_btl_openib_srq_qp_info_t {
    int32_t sd_max;
    /* The init value for rd_curr_num variables of all SRQs */
    int32_t rd_init;
    /* The watermark, threshold - if the number of WQEs in SRQ is less then this value =>
       the SRQ limit event (IBV_EVENT_SRQ_LIMIT_REACHED) will be generated on corresponding SRQ.
       As result the maximal number of pre-posted WQEs on the SRQ will be increased */
    int32_t srq_limit;
}; typedef struct mca_btl_openib_srq_qp_info_t mca_btl_openib_srq_qp_info_t;

struct mca_btl_openib_qp_info_t {
    mca_btl_openib_qp_type_t type;
    size_t size;
    int32_t rd_num;
    int32_t rd_low;
    union {
        mca_btl_openib_pp_qp_info_t pp_qp;
        mca_btl_openib_srq_qp_info_t srq_qp;
    } u;
}; typedef struct mca_btl_openib_qp_info_t mca_btl_openib_qp_info_t;

#define BTL_OPENIB_QP_TYPE(Q) (mca_btl_openib_component.qp_infos[(Q)].type)
#define BTL_OPENIB_QP_TYPE_PP(Q) \
    (BTL_OPENIB_QP_TYPE(Q) == MCA_BTL_OPENIB_PP_QP)
#define BTL_OPENIB_QP_TYPE_SRQ(Q) \
    (BTL_OPENIB_QP_TYPE(Q) == MCA_BTL_OPENIB_SRQ_QP)
#define BTL_OPENIB_QP_TYPE_XRC(Q) \
    (BTL_OPENIB_QP_TYPE(Q) == MCA_BTL_OPENIB_XRC_QP)

typedef enum {
    BTL_OPENIB_RQ_SOURCE_DEVICE_INI = MCA_BASE_VAR_SOURCE_MAX,
} btl_openib_receive_queues_source_t;

typedef enum {
    BTL_OPENIB_DT_IB,
    BTL_OPENIB_DT_IWARP,
    BTL_OPENIB_DT_ALL
} btl_openib_device_type_t;

/* The structer for manage all BTL SRQs */
typedef struct mca_btl_openib_srq_manager_t {
    opal_mutex_t lock;
    /* The keys of this hash table are addresses of
       SRQs structures, and the elements are BTL modules
       pointers that associated with these SRQs */
    opal_hash_table_t srq_addr_table;
} mca_btl_openib_srq_manager_t;

struct mca_btl_openib_component_t {
    mca_btl_base_component_3_0_0_t          super;  /**< base BTL component */

    int                                ib_max_btls;
    /**< maximum number of devices available to openib component */

    int                                ib_num_btls;
    /**< number of devices available to the openib component */

    struct mca_btl_openib_module_t             **openib_btls;
    /**< array of available BTLs */

    opal_pointer_array_t devices; /**< array of available devices */
    int devices_count;

    int ib_free_list_num;
    /**< initial size of free lists */

    int ib_free_list_max;
    /**< maximum size of free lists */

    int ib_free_list_inc;
    /**< number of elements to alloc when growing free lists */

    opal_list_t                             ib_procs;
    /**< list of ib proc structures */

    opal_event_t                            ib_send_event;
    /**< event structure for sends */

    opal_event_t                            ib_recv_event;
    /**< event structure for recvs */

    opal_mutex_t                            ib_lock;
    /**< lock for accessing module state */

    char* ib_mpool_name;
    /**< name of ib memory pool */

    uint8_t num_pp_qps;          /**< number of pp qp's */
    uint8_t num_srq_qps;         /**< number of srq qp's */
    uint8_t num_xrc_qps;         /**< number of xrc qp's */
    uint8_t num_qps;             /**< total number of qp's */

    opal_hash_table_t ib_addr_table; /**< used only for xrc.hash-table that
                                       keeps table of all lids/subnets */
    mca_btl_openib_qp_info_t* qp_infos;

    size_t eager_limit;      /**< Eager send limit of first fragment, in Bytes */
    size_t max_send_size;    /**< Maximum send size, in Bytes */
    uint32_t max_hw_msg_size;/**< Maximum message size for RDMA protocols in Bytes */
    uint32_t reg_mru_len;    /**< Length of the registration cache most recently used list */
    uint32_t use_srq;        /**< Use the Shared Receive Queue (SRQ mode) */

    uint32_t ib_cq_size[2];  /**< Max outstanding CQE on the CQ */

    int      ib_max_inline_data; /**< Max size of inline data */
    unsigned int ib_pkey_val;
    unsigned int ib_psn;
    unsigned int ib_qp_ous_rd_atom;
    uint32_t ib_mtu;
    unsigned int ib_min_rnr_timer;
    unsigned int ib_timeout;
    unsigned int ib_retry_count;
    unsigned int ib_rnr_retry;
    unsigned int ib_max_rdma_dst_ops;
    unsigned int ib_service_level;
#if (ENABLE_DYNAMIC_SL)
    unsigned int ib_path_record_service_level;
#endif
    int     use_eager_rdma;
    int     eager_rdma_threshold; /**< After this number of msg, use RDMA for short messages, always */
    int     eager_rdma_num;
    int32_t max_eager_rdma;
    unsigned int btls_per_lid;
    unsigned int max_lmc;
    int     apm_lmc;
    int     apm_ports;
    unsigned int buffer_alignment;    /**< Preferred communication buffer alignment in Bytes (must be power of two) */
    int32_t error_counter;           /**< Counts number on error events that we got on all devices */
    int async_pipe[2];               /**< Pipe for comunication with async event thread */
    int async_comp_pipe[2];          /**< Pipe for async thread comunication with main thread */
    pthread_t   async_thread;        /**< Async thread that will handle fatal errors */
    bool use_async_event_thread;     /**< Use the async event handler */
    mca_btl_openib_srq_manager_t srq_manager;     /**< Hash table for all BTL SRQs */
#if BTL_OPENIB_FAILOVER_ENABLED
    bool port_error_failover;        /**< Report port errors to speed up failover */
#endif
    /* declare as an int instead of btl_openib_device_type_t since there is no
       guarantee about the size of an enum. this value will be registered as an
       integer with the MCA variable system */
    int device_type;
    char *if_include;
    char **if_include_list;
    char *if_exclude;
    char **if_exclude_list;
    char *ipaddr_include;
    char *ipaddr_exclude;

    /* MCA param btl_openib_receive_queues */
    char *receive_queues;
    /* Whether we got a non-default value of btl_openib_receive_queues */
    mca_base_var_source_t receive_queues_source;

    /** Colon-delimited list of filenames for device parameters */
    char *device_params_file_names;

    /** Whether we're in verbose mode or not */
    bool verbose;

    /** Whether we want a warning if no device-specific parameters are
        found in INI files */
    bool warn_no_device_params_found;
    /** Whether we want a warning if non default GID prefix is not configured
        on multiport setup */
    bool warn_default_gid_prefix;
    /** Whether we want a warning if the user specifies a non-existent
        device and/or port via btl_openib_if_[in|ex]clude MCA params */
    bool warn_nonexistent_if;
    /** Whether we want to abort if there's not enough registered
        memory available */
    bool abort_not_enough_reg_mem;

    /** Dummy argv-style list; a copy of names from the
        if_[in|ex]clude list that we use for error checking (to ensure
        that they all exist) */
    char **if_list;
    bool use_message_coalescing;
    unsigned int cq_poll_ratio;
    unsigned int cq_poll_progress;
    unsigned int cq_poll_batch;
    unsigned int eager_rdma_poll_ratio;
    int rdma_qp;
    int credits_qp; /* qp used for software flow control */
    bool cpc_explicitly_defined;
    /**< free list of frags only; used for pining user memory */
    opal_free_list_t send_user_free;
    /**< free list of frags only; used for pining user memory */
    opal_free_list_t recv_user_free;
    /**< frags for coalesced massages */
    opal_free_list_t send_free_coalesced;
    /** Default receive queues */
    char* default_recv_qps;
    /** GID index to use */
    int gid_index;
    /** Whether we want a dynamically resizing srq, enabled by default */
    bool enable_srq_resize;
    bool allow_max_memory_registration;
    int memory_registration_verbose_level;
    int memory_registration_verbose;
    int ignore_locality;
#if BTL_OPENIB_FAILOVER_ENABLED
    int verbose_failover;
#endif
#if BTL_OPENIB_MALLOC_HOOKS_ENABLED
    int use_memalign;
    size_t memalign_threshold;
    void* (*previous_malloc_hook)(size_t __size, const void*);
#endif
#if OPAL_CUDA_SUPPORT
    bool cuda_async_send;
    bool cuda_async_recv;
    bool cuda_have_gdr;
    bool driver_have_gdr;
    bool cuda_want_gdr;
#endif /* OPAL_CUDA_SUPPORT */
#if HAVE_DECL_IBV_LINK_LAYER_ETHERNET
    bool rroce_enable;
#endif
}; typedef struct mca_btl_openib_component_t mca_btl_openib_component_t;

OPAL_MODULE_DECLSPEC extern mca_btl_openib_component_t mca_btl_openib_component;

typedef mca_btl_base_recv_reg_t mca_btl_openib_recv_reg_t;

/**
 * Common information for all ports that is sent in the modex message
 */
typedef struct mca_btl_openib_modex_message_t {
    /** The subnet ID of this port */
    uint64_t subnet_id;
    /** LID of this port */
    uint16_t lid;
    /** APM LID for this port */
    uint16_t apm_lid;
    /** The MTU used by this port */
    uint8_t mtu;
    /** vendor id define device type and tuning */
    uint32_t vendor_id;
    /** vendor part id define device type and tuning */
    uint32_t vendor_part_id;
    /** Transport type of remote port */
    uint8_t transport_type;
    /** Dummy field used to calculate the real length */
    uint8_t end;
} mca_btl_openib_modex_message_t;

#define MCA_BTL_OPENIB_MODEX_MSG_NTOH(hdr)     \
    do {                              \
        (hdr).subnet_id = ntoh64((hdr).subnet_id); \
        (hdr).lid = ntohs((hdr).lid); \
    } while (0)
#define MCA_BTL_OPENIB_MODEX_MSG_HTON(hdr)     \
    do {                              \
        (hdr).subnet_id = hton64((hdr).subnet_id); \
        (hdr).lid = htons((hdr).lid); \
    } while (0)

typedef struct mca_btl_openib_device_qp_t {
    opal_free_list_t send_free;     /**< free lists of send buffer descriptors */
    opal_free_list_t recv_free;     /**< free lists of receive buffer descriptors */
} mca_btl_openib_device_qp_t;

struct mca_btl_base_endpoint_t;

typedef struct mca_btl_openib_device_t {
    opal_object_t super;
    struct ibv_device *ib_dev;  /* the ib device */
#if OPAL_ENABLE_PROGRESS_THREADS == 1
    struct ibv_comp_channel *ib_channel; /* Channel event for the device */
    opal_thread_t thread;                /* Progress thread */
    volatile bool progress;              /* Progress status */
#endif
    opal_mutex_t device_lock;          /* device level lock */
    struct ibv_context *ib_dev_context;
    struct ibv_device_attr ib_dev_attr;
    struct ibv_pd *ib_pd;
    struct ibv_cq *ib_cq[2];
    uint32_t cq_size[2];
    mca_mpool_base_module_t *mpool;
    /* MTU for this device */
    uint32_t mtu;
    /* Whether this device supports eager RDMA */
    uint8_t use_eager_rdma;
    uint8_t btls;              /** < number of btls using this device */
    opal_pointer_array_t *endpoints;
    opal_pointer_array_t *device_btls;
    uint16_t hp_cq_polls;
    uint16_t eager_rdma_polls;
    bool pollme;
    volatile bool got_fatal_event;
    volatile bool got_port_event;
#if HAVE_XRC
#if OPAL_HAVE_CONNECTX_XRC_DOMAINS
    struct ibv_xrcd *xrcd;
#else
    struct ibv_xrc_domain *xrc_domain;
#endif
    int xrc_fd;
#endif
    int32_t non_eager_rdma_endpoints;
    int32_t eager_rdma_buffers_count;
    struct mca_btl_base_endpoint_t **eager_rdma_buffers;
    /**< frags for control massages */
    opal_free_list_t send_free_control;
    /* QP types and attributes that will be used on this device */
    mca_btl_openib_device_qp_t *qps;
    /* Maximum value supported by this device for max_inline_data */
    uint32_t max_inline_data;
    /* Registration limit and current count */
    uint64_t mem_reg_max, mem_reg_active;
    /* Device is ready for use */
    bool ready_for_use;
} mca_btl_openib_device_t;
OBJ_CLASS_DECLARATION(mca_btl_openib_device_t);

struct mca_btl_openib_module_pp_qp_t {
    int32_t dummy;
}; typedef struct mca_btl_openib_module_pp_qp_t mca_btl_openib_module_pp_qp_t;

struct mca_btl_openib_module_srq_qp_t {
    struct ibv_srq *srq;
    int32_t rd_posted;
    int32_t sd_credits;  /* the max number of outstanding sends on a QP when using SRQ */
                         /*  i.e. the number of frags that  can be outstanding (down counter) */
    opal_list_t pending_frags[2];    /**< list of high/low prio frags */
    /** The number of receive buffers that can be post in the current time.
        The value may be increased in the IBV_EVENT_SRQ_LIMIT_REACHED
        event handler. The value starts from (rd_num / 4) and increased up to rd_num */
    int32_t rd_curr_num;
    /** We post additional WQEs only if a number of WQEs (in specific SRQ) is less of this value.
         The value increased together with rd_curr_num. The value is unique for every SRQ. */
    int32_t rd_low_local;
    /** The flag points if we want to get the
         IBV_EVENT_SRQ_LIMIT_REACHED events for dynamically resizing SRQ */
    bool srq_limit_event_flag;
    /**< In difference of the "--mca enable_srq_resize" parameter that says, if we want(or no)
         to start with small num of pre-posted receive buffers (rd_curr_num) and to increase this number by needs
         (the max of this value is rd_num * the whole size of SRQ), the "srq_limit_event_flag" says if we want to get limit event
         from device if the defined srq limit was reached (signal to the main thread) and we put off this flag if the rd_curr_num
         was increased up to rd_num.
         In order to prevent lock/unlock operation in the critical path we prefer only put-on
         the srq_limit_event_flag in asynchronous thread, because in this way we post receive buffers
         in the main thread only and only after posting we set (if srq_limit_event_flag is true)
         the limit for IBV_EVENT_SRQ_LIMIT_REACHED event. */
}; typedef struct mca_btl_openib_module_srq_qp_t mca_btl_openib_module_srq_qp_t;

struct mca_btl_openib_module_qp_t {
    union {
        mca_btl_openib_module_pp_qp_t pp_qp;
        mca_btl_openib_module_srq_qp_t srq_qp;
    } u;
}; typedef struct mca_btl_openib_module_qp_t mca_btl_openib_module_qp_t;

/**
 * IB BTL Interface
 */
struct mca_btl_openib_module_t {
    /* Base BTL module */
    mca_btl_base_module_t  super;

    bool btl_inited;

    /** Common information about all ports */
    mca_btl_openib_modex_message_t port_info;

    /** Array of CPCs on this port */
    opal_btl_openib_connect_base_module_t **cpcs;

    /** Number of elements in the cpcs array */
    uint8_t num_cpcs;

    mca_btl_openib_device_t *device;
    uint8_t port_num;                  /**< ID of the PORT */
    uint16_t pkey_index;
    struct ibv_port_attr ib_port_attr;
    uint16_t lid;                      /**< lid that is actually used (for LMC) */
    int apm_port;                      /**< Alternative port that may be used for APM */
    uint8_t src_path_bits;             /**< offset from base lid (for LMC) */

    int32_t num_peers;

    opal_mutex_t ib_lock;              /**< module level lock */

    size_t eager_rdma_frag_size;                /**< length of eager frag */
    volatile int32_t eager_rdma_channels;  /**< number of open RDMA channels */

    mca_btl_base_module_error_cb_fn_t error_cb; /**< error handler */

    mca_btl_openib_module_qp_t * qps;

    int local_procs;                   /** number of local procs */
};
typedef struct mca_btl_openib_module_t mca_btl_openib_module_t;

extern mca_btl_openib_module_t mca_btl_openib_module;

struct mca_btl_base_registration_handle_t {
    uint32_t rkey;
    uint32_t lkey;
};

struct mca_btl_openib_reg_t {
    mca_mpool_base_registration_t base;
    struct ibv_mr *mr;
    mca_btl_base_registration_handle_t btl_handle;
};
typedef struct mca_btl_openib_reg_t mca_btl_openib_reg_t;

#if OPAL_ENABLE_PROGRESS_THREADS == 1
extern void* mca_btl_openib_progress_thread(opal_object_t*);
#endif


/**
 * Register a callback function that is called on error..
 *
 * @param btl (IN)     BTL module
 * @return             Status indicating if cleanup was successful
 */

int mca_btl_openib_register_error_cb(
    struct mca_btl_base_module_t* btl,
    mca_btl_base_module_error_cb_fn_t cbfunc
);


/**
 * Cleanup any resources held by the BTL.
 *
 * @param btl  BTL instance.
 * @return     OPAL_SUCCESS or error status on failure.
 */

extern int mca_btl_openib_finalize(
    struct mca_btl_base_module_t* btl
);


/**
 * PML->BTL notification of change in the process list.
 *
 * @param btl (IN)            BTL module
 * @param nprocs (IN)         Number of processes
 * @param procs (IN)          Set of processes
 * @param peers (OUT)         Set of (optional) peer addressing info.
 * @param reachable (IN/OUT)  Set of processes that are reachable via this BTL.
 * @return     OPAL_SUCCESS or error status on failure.
 *
 */

extern int mca_btl_openib_add_procs(
    struct mca_btl_base_module_t* btl,
    size_t nprocs,
    struct opal_proc_t **procs,
    struct mca_btl_base_endpoint_t** peers,
    opal_bitmap_t* reachable
);

/**
 * PML->BTL notification of change in the process list.
 *
 * @param btl (IN)     BTL instance
 * @param nproc (IN)   Number of processes.
 * @param procs (IN)   Set of processes.
 * @param peers (IN)   Set of peer data structures.
 * @return             Status indicating if cleanup was successful
 *
 */
extern int mca_btl_openib_del_procs(
    struct mca_btl_base_module_t* btl,
    size_t nprocs,
    struct opal_proc_t **procs,
    struct mca_btl_base_endpoint_t** peers
);


/**
 * PML->BTL Initiate a send of the specified size.
 *
 * @param btl (IN)               BTL instance
 * @param btl_peer (IN)          BTL peer addressing
 * @param descriptor (IN)        Descriptor of data to be transmitted.
 * @param tag (IN)               Tag.
 */
extern int mca_btl_openib_send(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* btl_peer,
    struct mca_btl_base_descriptor_t* descriptor,
    mca_btl_base_tag_t tag
);

/**
 * PML->BTL Initiate a immediate send of the specified size.
 *
 * @param btl (IN)               BTL instance
 * @param ep (IN)                Endpoint
 * @param convertor (IN)         Datatypes converter
 * @param header (IN)            PML header
 * @param header_size (IN)       PML header size
 * @param payload_size (IN)      Payload size
 * @param order (IN)             Order
 * @param flags (IN)             Flags
 * @param tag (IN)               Tag
 * @param descriptor (OUT)       Messages descriptor
 */
extern int mca_btl_openib_sendi( struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct opal_convertor_t* convertor,
    void* header,
    size_t header_size,
    size_t payload_size,
    uint8_t order,
    uint32_t flags,
    mca_btl_base_tag_t tag,
    mca_btl_base_descriptor_t** descriptor
);

/* forward decaration for internal put/get */
struct mca_btl_openib_put_frag_t;
struct mca_btl_openib_get_frag_t;

/**
 * @brief Schedule a put fragment with the HCA (internal)
 *
 * @param btl (IN)               BTL instance
 * @param ep (IN)                BTL endpoint
 * @param frag (IN)              Fragment prepared by mca_btl_openib_put
 *
 * If the fragment can not be scheduled due to resource limitations then
 * the fragment will be put on the pending put fragment list and retried
 * when another get/put fragment has completed.
 */
int mca_btl_openib_put_internal (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *ep,
                                 struct mca_btl_openib_put_frag_t *frag);

/**
 * @brief Schedule an RDMA write with the HCA
 *
 * @param btl (IN)               BTL instance
 * @param ep (IN)                BTL endpoint
 * @param local_address (IN)     Source address
 * @param remote_address (IN)    Destination address
 * @param local_handle (IN)      Registration handle for region containing the region {local_address, size}
 * @param remote_handle (IN)     Registration handle for region containing the region {remote_address, size}
 * @param size (IN)              Number of bytes to write
 * @param flags (IN)             Transfer flags
 * @param order (IN)             Ordering
 * @param cbfunc (IN)            Function to call on completion
 * @param cbcontext (IN)         Context for completion callback
 * @param cbdata (IN)            Data for completion callback
 *
 * @return OPAL_ERR_BAD_PARAM if a bad parameter was passed
 * @return OPAL_SUCCCESS if the operation was successfully scheduled
 *
 * This function will attempt to schedule a put operation with the HCA.
 */
int mca_btl_openib_put (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                        uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                        mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                        int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata);

/**
 * @brief Schedule a get fragment with the HCA (internal)
 *
 * @param btl (IN)               BTL instance
 * @param ep (IN)                BTL endpoint
 * @param qp (IN)                ID of queue pair to schedule the get on
 * @param frag (IN)              Fragment prepared by mca_btl_openib_get
 *
 * If the fragment can not be scheduled due to resource limitations then
 * the fragment will be put on the pending get fragment list and retried
 * when another get/put fragment has completed.
 */
int mca_btl_openib_get_internal (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *ep,
                                 struct mca_btl_openib_get_frag_t *frag);

/**
 * @brief Schedule an RDMA read with the HCA
 *
 * @param btl (IN)               BTL instance
 * @param ep (IN)                BTL endpoint
 * @param local_address (IN)     Destination address
 * @param remote_address (IN)    Source address
 * @param local_handle (IN)      Registration handle for region containing the region {local_address, size}
 * @param remote_handle (IN)     Registration handle for region containing the region {remote_address, size}
 * @param size (IN)              Number of bytes to read
 * @param flags (IN)             Transfer flags
 * @param order (IN)             Ordering
 * @param cbfunc (IN)            Function to call on completion
 * @param cbcontext (IN)         Context for completion callback
 * @param cbdata (IN)            Data for completion callback
 *
 * @return OPAL_ERR_BAD_PARAM if a bad parameter was passed
 * @return OPAL_SUCCCESS if the operation was successfully scheduled
 *
 * This function will attempt to schedule a get operation with the HCA.
 */
int mca_btl_openib_get (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                        uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                        mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                        int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata);

/**
 * Initiate an asynchronous fetching atomic operation.
 * Completion Semantics: if this function returns a 1 then the operation
 *                       is complete. a return of OPAL_SUCCESS indicates
 *                       the atomic operation has been queued with the
 *                       network.
 *
 * @param btl (IN)            BTL module
 * @param endpoint (IN)       BTL addressing information
 * @param local_address (OUT) Local address to store the result in
 * @param remote_address (IN) Remote address perfom operation on to (registered remotely)
 * @param local_handle (IN)   Local registration handle for region containing
 *                            (local_address, local_address + 8)
 * @param remote_handle (IN)  Remote registration handle for region containing
 *                            (remote_address, remote_address + 8)
 * @param op (IN)             Operation to perform
 * @param operand (IN)        Operand for the operation
 * @param flags (IN)          Flags for this put operation
 * @param order (IN)          Ordering
 * @param cbfunc (IN)         Function to call on completion (if queued)
 * @param cbcontext (IN)      Context for the callback
 * @param cbdata (IN)         Data for callback
 *
 * @retval OPAL_SUCCESS    The operation was successfully queued
 * @retval 1               The operation is complete
 * @retval OPAL_ERROR      The operation was NOT successfully queued
 * @retval OPAL_ERR_OUT_OF_RESOURCE  Insufficient resources to queue the atomic
 *                         operation. Try again later
 * @retval OPAL_ERR_NOT_AVAILABLE  Atomic operation can not be performed due to
 *                         alignment restrictions or the operation {op} is not supported
 *                         by the hardware.
 *
 * After the operation is complete the remote address specified by {remote_address} and
 * {remote_handle} will be updated with (*remote_address) = (*remote_address) op operand.
 * {local_address} will be updated with the previous value stored in {remote_address}.
 * The btl will guarantee consistency of atomic operations performed via the btl. Note,
 * however, that not all btls will provide consistency between btl atomic operations and
 * cpu atomics.
 */
int mca_btl_openib_atomic_fop (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                               void *local_address, uint64_t remote_address,
                               struct mca_btl_base_registration_handle_t *local_handle,
                               struct mca_btl_base_registration_handle_t *remote_handle, mca_btl_base_atomic_op_t op,
                               uint64_t operand, int flags, int order, mca_btl_base_rdma_completion_fn_t cbfunc,
                               void *cbcontext, void *cbdata);

/**
 * Initiate an asynchronous compare and swap operation.
 * Completion Semantics: if this function returns a 1 then the operation
 *                       is complete. a return of OPAL_SUCCESS indicates
 *                       the atomic operation has been queued with the
 *                       network.
 *
 * @param btl (IN)            BTL module
 * @param endpoint (IN)       BTL addressing information
 * @param local_address (OUT) Local address to store the result in
 * @param remote_address (IN) Remote address perfom operation on to (registered remotely)
 * @param local_handle (IN)   Local registration handle for region containing
 *                            (local_address, local_address + 8)
 * @param remote_handle (IN)  Remote registration handle for region containing
 *                            (remote_address, remote_address + 8)
 * @param compare (IN)        Operand for the operation
 * @param value (IN)          Value to store on success
 * @param flags (IN)          Flags for this put operation
 * @param order (IN)          Ordering
 * @param cbfunc (IN)         Function to call on completion (if queued)
 * @param cbcontext (IN)      Context for the callback
 * @param cbdata (IN)         Data for callback
 *
 * @retval OPAL_SUCCESS    The operation was successfully queued
 * @retval 1               The operation is complete
 * @retval OPAL_ERROR      The operation was NOT successfully queued
 * @retval OPAL_ERR_OUT_OF_RESOURCE  Insufficient resources to queue the atomic
 *                         operation. Try again later
 * @retval OPAL_ERR_NOT_AVAILABLE  Atomic operation can not be performed due to
 *                         alignment restrictions or the operation {op} is not supported
 *                         by the hardware.
 *
 * After the operation is complete the remote address specified by {remote_address} and
 * {remote_handle} will be updated with {value} if *remote_address == compare.
 * {local_address} will be updated with the previous value stored in {remote_address}.
 * The btl will guarantee consistency of atomic operations performed via the btl. Note,
 * however, that not all btls will provide consistency between btl atomic operations and
 * cpu atomics.
 */
int mca_btl_openib_atomic_cswap (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                                 void *local_address, uint64_t remote_address,
                                 struct mca_btl_base_registration_handle_t *local_handle,
                                 struct mca_btl_base_registration_handle_t *remote_handle, uint64_t compare,
                                 uint64_t value, int flags, int order, mca_btl_base_rdma_completion_fn_t cbfunc,
                                 void *cbcontext, void *cbdata);

/**
 * Allocate a descriptor.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Requested descriptor size.
 */
extern mca_btl_base_descriptor_t* mca_btl_openib_alloc(
        struct mca_btl_base_module_t* btl,
        struct mca_btl_base_endpoint_t* endpoint,
        uint8_t order,
        size_t size,
        uint32_t flags);


/**
 * Return a segment allocated by this BTL.
 *
 * @param btl (IN)         BTL module
 * @param descriptor (IN)  Allocated descriptor.
 */
extern int mca_btl_openib_free(
                               struct mca_btl_base_module_t* btl,
                               mca_btl_base_descriptor_t* des);


/**
 * Pack data and return a descriptor that can be
 * used for send/put.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
mca_btl_base_descriptor_t* mca_btl_openib_prepare_src(
                                                      struct mca_btl_base_module_t* btl,
                                                      struct mca_btl_base_endpoint_t* peer,
                                                      struct opal_convertor_t* convertor,
                                                      uint8_t order,
                                                      size_t reserve,
                                                      size_t* size,
                                                      uint32_t flags
                                                      );

extern void mca_btl_openib_frag_progress_pending_put_get(
        struct mca_btl_base_endpoint_t*, const int);

/**
 * Fault Tolerance Event Notification Function
 *
 * @param state (IN)  Checkpoint State
 * @return OPAL_SUCCESS or failure status
 */
extern int mca_btl_openib_ft_event(int state);


/**
 * Show an error during init, particularly when running out of
 * registered memory.
 */
void mca_btl_openib_show_init_error(const char *file, int line,
                                    const char *func, const char *dev);

#define BTL_OPENIB_HP_CQ 0
#define BTL_OPENIB_LP_CQ 1


/**
 * Post to Shared Receive Queue with certain priority
 *
 * @param openib_btl (IN) BTL module
 * @param additional (IN) Additional Bytes to reserve
 * @param prio (IN)       Priority (either BTL_OPENIB_HP_QP or BTL_OPENIB_LP_QP)
 * @return OPAL_SUCCESS or failure status
 */

int mca_btl_openib_post_srr(mca_btl_openib_module_t* openib_btl, const int qp);

/**
 * Get a transport name of btl by its transport type.
 */

const char* btl_openib_get_transport_name(mca_btl_openib_transport_type_t transport_type);

/**
 * Get a transport type of btl.
 */

mca_btl_openib_transport_type_t mca_btl_openib_get_transport_type(mca_btl_openib_module_t* openib_btl);

static inline int qp_cq_prio(const int qp)
{
    if(0 == qp)
        return BTL_OPENIB_HP_CQ; /* smallest qp is always HP */

    /* If the size for this qp is <= the eager limit, make it a
       high priority QP.  Otherwise, make it a low priority QP. */
    return (mca_btl_openib_component.qp_infos[qp].size <=
            mca_btl_openib_component.eager_limit) ?
        BTL_OPENIB_HP_CQ : BTL_OPENIB_LP_CQ;
}

#define BTL_OPENIB_RDMA_QP(QP) \
    ((QP) == mca_btl_openib_component.rdma_qp)

END_C_DECLS

#endif /* MCA_BTL_IB_H */
