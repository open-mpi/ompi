/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * @file
 */

#ifndef MCA_PTL_IB_H
#define MCA_PTL_IB_H

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

#include "btl_openib_frag.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

#define MCA_BTL_IB_LEAVE_PINNED 1
#define IB_DEFAULT_GID_PREFIX 0xfe80000000000000ll

/**
 * Infiniband (IB) BTL component.
 */

struct mca_btl_openib_component_t {
    mca_btl_base_component_1_0_1_t          super;  /**< base BTL component */ 

    int                                ib_max_btls;
    /**< maximum number of hcas available to the IB component */
    
    int                                ib_num_btls;
    /**< number of hcas available to the IB component */

    struct mca_btl_openib_module_t             *openib_btls;
    /**< array of available PTLs */

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
   
    int32_t rd_num; /**< the number of receive descriptors to post to each queue pair */
    int32_t rd_low; /**< low water mark to reach before posting additional receive descriptors */
    int32_t rd_win; /**< ack credits when window size exceeded */
    int32_t rd_rsv; /**< descriptors held in reserve for control messages */

    int32_t srq_rd_max; /* maximum number of receive descriptors posted */
    int32_t srq_rd_per_peer; /* number of receive descriptors to post per log2(peers) in SRQ mode */
    int32_t srq_sd_max; /* maximum number of send descriptors posted */

    size_t eager_limit; 
    size_t max_send_size; 
    uint32_t reg_mru_len; 
    uint32_t use_srq; 
    
    uint32_t ib_cq_size;   /**< Max outstanding CQE on the CQ */  
    uint32_t ib_sg_list_size; /**< Max scatter/gather descriptor entries on the WQ*/ 
    uint32_t ib_pkey_ix; 
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
    uint32_t ib_static_rate; 
    uint32_t use_eager_rdma;
    int32_t eager_rdma_threshold;
    uint32_t eager_rdma_num;
    int32_t max_eager_rdma;
    uint32_t btls_per_lid;
    uint32_t max_lmc;
    uint32_t buffer_alignment;

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
#ifdef HAVE_IBV_FORK_INIT
    /** Whether we want fork support or not */
    int want_fork_support;
#endif
}; typedef struct mca_btl_openib_component_t mca_btl_openib_component_t;

OMPI_MODULE_DECLSPEC extern mca_btl_openib_component_t mca_btl_openib_component;

typedef mca_btl_base_recv_reg_t mca_btl_openib_recv_reg_t; 
    
struct mca_btl_openib_port_info_t {
    uint32_t mtu;
#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT 
    uint8_t padding[4]; 
#endif
    uint64_t subnet_id;
};
typedef struct mca_btl_openib_port_info_t mca_btl_openib_port_info_t;

#define MCA_BTL_OPENIB_PORT_INFO_NTOH(hdr)     \
    do {                              \
        (hdr).mtu = ntohl((hdr).mtu); \
        (hdr).subnet_id = ntoh64((hdr).subnet_id); \
    } while (0)
#define MCA_BTL_OPENIB_PORT_INFO_HTON(hdr)     \
    do {                              \
        (hdr).mtu = htonl((hdr).mtu); \
        (hdr).subnet_id = hton64((hdr).subnet_id); \
    } while (0)

struct mca_btl_openib_hca_t {
    struct ibv_device *ib_dev;  /* the ib device */
    struct ibv_context *ib_dev_context;
    struct ibv_device_attr ib_dev_attr;
    struct ibv_pd *ib_pd;
    mca_mpool_base_module_t *mpool;
    /* MTU for this HCA */
    uint32_t mtu;
    /* Whether this HCA supports eager RDMA */
    uint8_t use_eager_rdma;
    uint8_t btls;              /** < number of btls using this HCA */
};
typedef struct mca_btl_openib_hca_t mca_btl_openib_hca_t;
/**
 * IB PTL Interface
 */
struct mca_btl_openib_module_t {
    mca_btl_base_module_t  super;  /**< base PTL interface */
    bool btl_inited; 
    mca_btl_openib_recv_reg_t ib_reg[256]; 
    mca_btl_openib_port_info_t port_info;  /* contains only the subnet id right now */ 
    mca_btl_openib_hca_t *hca;
    uint8_t port_num;           /**< ID of the PORT */ 
    uint16_t pkey_index;
    struct ibv_cq *ib_cq[2];
    struct ibv_port_attr ib_port_attr; 
    uint16_t lid;                      /**< lid that is actually used (for LMC) */
    uint8_t src_path_bits;             /**< offset from base lid (for LMC) */

    ompi_free_list_t send_free_eager;  /**< free list of eager buffer descriptors */
    ompi_free_list_t send_free_max;    /**< free list of max buffer descriptors */ 
    ompi_free_list_t send_free_frag;   /**< free list of frags only... used for pining memory */ 
    
    ompi_free_list_t recv_free_eager;  /**< High priority free list of buffer descriptors */
    ompi_free_list_t recv_free_max;    /**< Low priority free list of buffer descriptors */ 
    ompi_free_list_t recv_free_frag;   /**< free list of frags only... used for pining memory */ 

    ompi_free_list_t send_free_control; /**< frags for control massages */ 
    opal_mutex_t ib_lock;          /**< module level lock */ 
    
    
    /**< an array to allow posting of rr in one swoop */ 
    size_t ib_inline_max; /**< max size of inline send*/ 
    bool poll_cq; 
    
    struct ibv_srq *srq[2]; 
    int32_t srd_posted[2]; 
    int32_t num_peers;
    int32_t  rd_num; 
    int32_t  rd_low;
    
    int32_t sd_tokens[2];
    /**< number of frags that  can be outstanding (down counter) */ 
    
    opal_list_t pending_frags[2]; /**< list of pending frags */ 

    size_t eager_rdma_frag_size; /**< length of eager frag */
    orte_pointer_array_t *eager_rdma_buffers; /**< RDMA buffers to poll */
    volatile int32_t eager_rdma_buffers_count; /**< number of RDMA buffers */

    mca_btl_base_module_error_cb_fn_t error_cb; /**< error handler */
   
    orte_pointer_array_t *endpoints;
}; typedef struct mca_btl_openib_module_t mca_btl_openib_module_t;

extern mca_btl_openib_module_t mca_btl_openib_module;

struct mca_btl_openib_reg_t {
    mca_mpool_base_registration_t base;
    struct ibv_mr *mr;
};
typedef struct mca_btl_openib_reg_t mca_btl_openib_reg_t;

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
 * @param btl (IN)
 * @param nprocs (IN)     Number of processes
 * @param procs (IN)      Set of processes
 * @param peers (OUT)     Set of (optional) peer addressing info.
 * @param peers (IN/OUT)  Set of processes that are reachable via this BTL.
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
 * @param btl_base_peer (IN)     BTL peer addressing
 * @param send_request (IN/OUT)  Send request (allocated by PML via mca_btl_base_request_alloc_fn_t)
 * @param size (IN)              Number of bytes PML is requesting BTL to deliver
 * @param flags (IN)             Flags that should be passed to the peer via the message header.
 * @param request (OUT)          OMPI_SUCCESS if the BTL was able to queue one or more fragments
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
 * @param btl_base_peer (IN)     BTL peer addressing
 * @param send_request (IN/OUT)  Send request (allocated by PML via mca_btl_base_request_alloc_fn_t)
 * @param size (IN)              Number of bytes PML is requesting BTL to deliver
 * @param flags (IN)             Flags that should be passed to the peer via the message header.
 * @param request (OUT)          OMPI_SUCCESS if the BTL was able to queue one or more fragments
 */
extern int mca_btl_openib_put(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* btl_peer,
    struct mca_btl_base_descriptor_t* decriptor
    );

/**
 * PML->BTL Initiate a get of the specified size.
 *
 * @param btl (IN)               BTL instance
 * @param btl_base_peer (IN)     BTL peer addressing
 * @param send_request (IN/OUT)  Send request (allocated by PML via mca_btl_base_request_alloc_fn_t)
 * @param size (IN)              Number of bytes PML is requesting BTL to deliver
 * @param flags (IN)             Flags that should be passed to the peer via the message header.
 * @param request (OUT)          OMPI_SUCCESS if the BTL was able to queue one or more fragments
 */
extern int mca_btl_openib_get(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* btl_peer,
    struct mca_btl_base_descriptor_t* decriptor
    );
    

/**
 * Allocate a descriptor.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Requested descriptor size.
 */
extern mca_btl_base_descriptor_t* mca_btl_openib_alloc(
                                                       struct mca_btl_base_module_t* btl, 
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
                                                             size_t reserve,
                                                             size_t* size); 
/**
 * Return a send fragment to the modules free list.
 *
 * @param btl (IN)   BTL instance
 * @param frag (IN)  IB send fragment
 *
 */
extern void mca_btl_openib_send_frag_return(mca_btl_base_module_t* btl,
        mca_btl_openib_frag_t*);


int mca_btl_openib_create_cq_srq(mca_btl_openib_module_t* openib_btl); 

#define BTL_OPENIB_HP_QP 0
#define BTL_OPENIB_LP_QP 1

static inline int mca_btl_openib_post_srr(mca_btl_openib_module_t* openib_btl,
        const int additional, const int prio)
{
    OPAL_THREAD_LOCK(&openib_btl->ib_lock);
    if(openib_btl->srd_posted[prio] <= openib_btl->rd_low + additional &&
             openib_btl->srd_posted[prio] < openib_btl->rd_num) {
        int32_t i, rc;
        int32_t num_post = openib_btl->rd_num - openib_btl->srd_posted[prio];
        ompi_free_list_item_t* item;
        mca_btl_openib_frag_t* frag;
        struct ibv_recv_wr *bad_wr;
        ompi_free_list_t *free_list;

        if(BTL_OPENIB_HP_QP == prio)
            free_list = &openib_btl->recv_free_eager;
        else
            free_list = &openib_btl->recv_free_max;

        for(i = 0; i < num_post; i++) {
            OMPI_FREE_LIST_WAIT(free_list, item, rc);
            frag = (mca_btl_openib_frag_t*)item;
            if(ibv_post_srq_recv(openib_btl->srq[prio], &frag->wr_desc.rd_desc,
                        &bad_wr)) {
                BTL_ERROR(("error posting receive descriptors to shared "
                            "receive queue: %s", strerror(errno)));
                OPAL_THREAD_UNLOCK(&openib_btl->ib_lock);
                return OMPI_ERROR;
            }
        }
        OPAL_THREAD_ADD32(&openib_btl->srd_posted[prio], num_post);
    }
    OPAL_THREAD_UNLOCK(&openib_btl->ib_lock);

    return OMPI_SUCCESS;
}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
