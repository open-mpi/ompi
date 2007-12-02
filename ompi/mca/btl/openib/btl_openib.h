/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2007 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
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

/* Standard system includes */
#include <sys/types.h>
#include <string.h>
#include <infiniband/verbs.h>

/* Open MPI includes */
#include "ompi/class/ompi_free_list.h"
#include "ompi/class/ompi_bitmap.h"
#include "orte/class/orte_pointer_array.h"
#include "opal/event/event.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "opal/util/output.h"
#include "ompi/mca/mpool/mpool.h" 
#include "ompi/mca/btl/base/btl_base_error.h" 

#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h" 

BEGIN_C_DECLS

#define HAVE_XRC (defined(HAVE_IBV_OPEN_XRC_DOMAIN) && (1 == OMPI_ENABLE_CONNECTX_XRC_SUPPORT))

#define MCA_BTL_IB_LEAVE_PINNED 1
#define IB_DEFAULT_GID_PREFIX 0xfe80000000000000ll


/**
 * Infiniband (IB) BTL component.
 */

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

struct mca_btl_openib_component_t {
    mca_btl_base_component_1_0_1_t          super;  /**< base BTL component */ 

    int                                ib_max_btls;
    /**< maximum number of hcas available to the IB component */
    
    int                                ib_num_btls;
    /**< number of hcas available to the IB component */

    struct mca_btl_openib_module_t             **openib_btls;
    /**< array of available BTLs */

    ompi_pointer_array_t hcas; /**< array of available hcas */
    int hcas_count;

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
    uint32_t reg_mru_len;    /**< Length of the registration cache most recently used list */
    uint32_t use_srq;        /**< Use the Shared Receive Queue (SRQ mode) */ 
    
    uint32_t ib_cq_size[2];  /**< Max outstanding CQE on the CQ */  
    
    uint32_t ib_sg_list_size; /**< Max scatter/gather descriptor entries on the WQ */ 
    uint32_t ib_pkey_ix;     /**< InfiniBand pkey index */
    uint32_t ib_pkey_val;
    uint32_t ib_psn; 
    uint32_t ib_qp_ous_rd_atom; 
    uint32_t ib_mtu; 
    uint32_t ib_min_rnr_timer; 
    uint32_t ib_timeout; 
    uint32_t ib_retry_count; 
    uint32_t ib_rnr_retry; 
    uint32_t ib_max_rdma_dst_ops; 
    uint32_t ib_service_level; 
    uint32_t use_eager_rdma;
    int32_t eager_rdma_threshold; /**< After this number of msg, use RDMA for short messages, always */
    int32_t eager_rdma_num;
    int32_t max_eager_rdma;
    uint32_t btls_per_lid;
    uint32_t max_lmc;
    uint32_t buffer_alignment;    /**< Preferred communication buffer alignment in Bytes (must be power of two) */
#if OMPI_HAVE_THREADS
    int32_t fatal_counter;           /**< Counts number on fatal events that we got on all hcas */
    int async_pipe[2];               /**< Pipe for comunication with async event thread */
    pthread_t   async_thread;        /**< Async thread that will handle fatal errors */
    uint32_t use_async_event_thread; /**< Use the async event handler */ 
#endif
    char *if_include;
    char **if_include_list;
    char *if_exclude;
    char **if_exclude_list;

    /** Colon-delimited list of filenames for HCA parameters */
    char *hca_params_file_names;

    /** Whether we're in verbose mode or not */
    bool verbose;

    /** Whether we want a warning if no HCA-specific parameters are
        found in INI files */
    bool warn_no_hca_params_found;
    /** Whether we want a warning if non default GID prefix is not configured
        on multiport setup */
    bool warn_default_gid_prefix;
    /** Whether we want a warning if the user specifies a non-existent
        HCA and/or port via btl_openib_if_[in|ex]clude MCA params */
    bool warn_nonexistent_if;
    /** Dummy argv-style list; a copy of names from the
        if_[in|ex]clude list that we use for error checking (to ensure
        that they all exist) */
    char **if_list;
#ifdef HAVE_IBV_FORK_INIT
    /** Whether we want fork support or not */
    int want_fork_support;
#endif
    int rdma_qp;
    int credits_qp; /* qp used for software flow control */
}; typedef struct mca_btl_openib_component_t mca_btl_openib_component_t;

OMPI_MODULE_DECLSPEC extern mca_btl_openib_component_t mca_btl_openib_component;

typedef mca_btl_base_recv_reg_t mca_btl_openib_recv_reg_t; 
    
struct mca_btl_openib_port_info_t {
    uint32_t mtu;
#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT 
    uint8_t padding[4]; 
#endif
    uint64_t subnet_id;
#if HAVE_XRC
    uint16_t lid; /* used only in xrc */
#endif
};
typedef struct mca_btl_openib_port_info_t mca_btl_openib_port_info_t;

#if HAVE_XRC
#define MCA_BTL_OPENIB_LID_NTOH(hdr) (hdr).lid = ntohs((hdr).lid)
#define MCA_BTL_OPENIB_LID_HTON(hdr) (hdr).lid = htons((hdr).lid)
#else
#define MCA_BTL_OPENIB_LID_NTOH(hdr)
#define MCA_BTL_OPENIB_LID_HTON(hdr)
#endif

#define MCA_BTL_OPENIB_PORT_INFO_NTOH(hdr)     \
    do {                              \
        (hdr).mtu = ntohl((hdr).mtu); \
        (hdr).subnet_id = ntoh64((hdr).subnet_id); \
        MCA_BTL_OPENIB_LID_NTOH(hdr); \
    } while (0)
#define MCA_BTL_OPENIB_PORT_INFO_HTON(hdr)     \
    do {                              \
        (hdr).mtu = htonl((hdr).mtu); \
        (hdr).subnet_id = hton64((hdr).subnet_id); \
        MCA_BTL_OPENIB_LID_HTON(hdr); \
    } while (0)

struct mca_btl_openib_hca_t {
    struct ibv_device *ib_dev;  /* the ib device */
#if OMPI_ENABLE_PROGRESS_THREADS == 1
    struct ibv_comp_channel *ib_channel; /* Channel event for the HCA */
    opal_thread_t thread;                /* Progress thread */
    volatile bool progress;              /* Progress status */
#endif
    opal_mutex_t hca_lock;          /* hca level lock */ 
    struct ibv_context *ib_dev_context;
    struct ibv_device_attr ib_dev_attr;
    struct ibv_pd *ib_pd;
    struct ibv_cq *ib_cq[2];
    uint32_t cq_size[2];
    mca_mpool_base_module_t *mpool;
    /* MTU for this HCA */
    uint32_t mtu;
    /* Whether this HCA supports eager RDMA */
    uint8_t use_eager_rdma;
    uint8_t btls;              /** < number of btls using this HCA */
    orte_pointer_array_t *endpoints;
#if OMPI_HAVE_THREADS
    volatile bool got_fatal_event;
#endif
#if HAVE_XRC
    struct ibv_xrc_domain *xrc_domain;
    int xrc_fd;
#endif
};
typedef struct mca_btl_openib_hca_t mca_btl_openib_hca_t;

struct mca_btl_openib_module_pp_qp_t {
    int32_t dummy;
}; typedef struct mca_btl_openib_module_pp_qp_t mca_btl_openib_module_pp_qp_t;

struct mca_btl_openib_module_srq_qp_t {
    struct ibv_srq *srq; 
    int32_t rd_posted; 
    int32_t sd_credits;  /* the max number of outstanding sends on a QP when using SRQ */ 
                         /*  i.e. the number of frags that  can be outstanding (down counter) */ 
    opal_list_t pending_frags[2];    /**< list of high/low prio frags */
}; typedef struct mca_btl_openib_module_srq_qp_t mca_btl_openib_module_srq_qp_t;

struct mca_btl_openib_module_qp_t {
    ompi_free_list_t send_free;     /**< free lists of send buffer descriptors */
    ompi_free_list_t recv_free;     /**< free lists of receive buffer descriptors */
    union {
        mca_btl_openib_module_pp_qp_t pp_qp;
        mca_btl_openib_module_srq_qp_t srq_qp;
    } u;
}; typedef struct mca_btl_openib_module_qp_t mca_btl_openib_module_qp_t;

/**
 * IB BTL Interface
 */
struct mca_btl_openib_module_t {
    mca_btl_base_module_t  super;      /**< base BTL interface */
    bool btl_inited; 
    mca_btl_openib_recv_reg_t ib_reg[256]; 
    mca_btl_openib_port_info_t port_info;  /* contains only the subnet id right now */ 
    mca_btl_openib_hca_t *hca;
    uint8_t port_num;                  /**< ID of the PORT */ 
    uint16_t pkey_index;
    struct ibv_port_attr ib_port_attr; 
    uint16_t lid;                      /**< lid that is actually used (for LMC) */
    uint8_t src_path_bits;             /**< offset from base lid (for LMC) */
        
    int32_t num_peers;
    
    ompi_free_list_t send_user_free;   /**< free list of frags only... 
                                       *   used for pining user memory */ 
    
    ompi_free_list_t recv_user_free;   /**< free list of frags only... 
                                       *   used for pining user memory */ 

    ompi_free_list_t send_free_control; /**< frags for control massages */ 

    opal_mutex_t ib_lock;              /**< module level lock */ 
    
    size_t ib_inline_max; /**< max size of inline send*/ 
    
    size_t eager_rdma_frag_size;                /**< length of eager frag */
    orte_pointer_array_t *eager_rdma_buffers;   /**< RDMA buffers to poll */
    volatile int32_t eager_rdma_buffers_count;  /**< number of RDMA buffers */

    mca_btl_base_module_error_cb_fn_t error_cb; /**< error handler */
   
    mca_btl_openib_module_qp_t * qps;
};
typedef struct mca_btl_openib_module_t mca_btl_openib_module_t;

extern mca_btl_openib_module_t mca_btl_openib_module;

struct mca_btl_openib_reg_t {
    mca_mpool_base_registration_t base;
    struct ibv_mr *mr;
};
typedef struct mca_btl_openib_reg_t mca_btl_openib_reg_t;

#if OMPI_ENABLE_PROGRESS_THREADS == 1
extern void* mca_btl_openib_progress_thread(opal_object_t*);
#endif

/**
 * Register a callback function that is called on receipt
 * of a fragment.
 *
 * @param btl (IN)     BTL module
 * @return             Status indicating if cleanup was successful
 *
 * When the process list changes, the PML notifies the BTL of the
 * change, to provide the opportunity to cleanup or release any
 * resources associated with the peer.
 */

int mca_btl_openib_register(
    struct mca_btl_base_module_t* btl,
    mca_btl_base_tag_t tag,
    mca_btl_base_module_recv_cb_fn_t cbfunc,
    void* cbdata
);


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
 * @return     OMPI_SUCCESS or error status on failure.
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
 * @return     OMPI_SUCCESS or error status on failure.
 * 
 */

extern int mca_btl_openib_add_procs(
    struct mca_btl_base_module_t* btl,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_btl_base_endpoint_t** peers,
    ompi_bitmap_t* reachable
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
    struct ompi_proc_t **procs,
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
 * PML->BTL Initiate a put of the specified size.
 *
 * @param btl (IN)               BTL instance
 * @param btl_peer (IN)          BTL peer addressing
 * @param descriptor (IN)        Descriptor of data to be transmitted.
 */
extern int mca_btl_openib_put(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* btl_peer,
    struct mca_btl_base_descriptor_t* descriptor
    );

/**
 * PML->BTL Initiate a get of the specified size.
 *
 * @param btl (IN)               BTL instance
 * @param btl_base_peer (IN)     BTL peer addressing
 * @param descriptor (IN)        Descriptor of data to be transmitted.
 */
extern int mca_btl_openib_get(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* btl_peer,
    struct mca_btl_base_descriptor_t* descriptor
    );
    

/**
 * Allocate a descriptor.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Requested descriptor size.
 */
extern mca_btl_base_descriptor_t* mca_btl_openib_alloc(
                                                       struct mca_btl_base_module_t* btl, 
                                                       uint8_t order,
                                                       size_t size); 
    

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
                                                      mca_mpool_base_registration_t* registration, 
                                                      struct ompi_convertor_t* convertor,
                                                      uint8_t order,
                                                      size_t reserve,
                                                      size_t* size
                                                      );

/**
 * Allocate a descriptor initialized for RDMA write.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
extern mca_btl_base_descriptor_t* mca_btl_openib_prepare_dst( 
                                                             struct mca_btl_base_module_t* btl, 
                                                             struct mca_btl_base_endpoint_t* peer,
                                                             mca_mpool_base_registration_t* registration, 
                                                             struct ompi_convertor_t* convertor,
                                                             uint8_t order,
                                                             size_t reserve,
                                                             size_t* size); 

extern void mca_btl_openib_frag_progress_pending_put_get(
        struct mca_btl_base_endpoint_t*, const int);

/**
 * Fault Tolerance Event Notification Function
 *
 * @param state (IN)  Checkpoint State
 * @return OMPI_SUCCESS or failure status
 */
extern int mca_btl_openib_ft_event(int state);


#define BTL_OPENIB_HP_CQ 0
#define BTL_OPENIB_LP_CQ 1


/**
 * Post to Shared Receive Queue with certain priority 
 *
 * @param openib_btl (IN) BTL module
 * @param additional (IN) Additional Bytes to reserve
 * @param prio (IN)       Priority (either BTL_OPENIB_HP_QP or BTL_OPENIB_LP_QP)
 * @return OMPI_SUCCESS or failure status
 */

int mca_btl_openib_post_srr(mca_btl_openib_module_t* openib_btl, const int qp);

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

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
END_C_DECLS

#endif /* MCA_BTL_IB_H */
