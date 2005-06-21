
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_IB_H
#define MCA_PTL_IB_H

/* Standard system includes */
#include <sys/types.h>
#include <string.h>

/* Open MPI includes */
#include "class/ompi_free_list.h"
#include "class/ompi_bitmap.h"
#include "event/event.h"
#include "mca/pml/pml.h"
#include "mca/bmi/bmi.h"
#include "util/output.h"
#include "mca/mpool/mpool.h" 
#include "bmi_ib_error.h" 

#include "mca/bmi/bmi.h"
#include "mca/bmi/base/base.h" 

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

#define MCA_BMI_IB_LEAVE_PINNED 1

/**
 * Infiniband (IB) BMI component.
 */

struct mca_bmi_ib_component_t {
    mca_bmi_base_component_1_0_0_t          super;  /**< base BMI component */ 
    
    uint32_t                                ib_num_bmis;
    /**< number of hcas available to the IB component */

    struct mca_bmi_ib_module_t             *ib_bmis;
    /**< array of available PTLs */

    int ib_free_list_num;
    /**< initial size of free lists */

    int ib_free_list_max;
    /**< maximum size of free lists */

    int ib_free_list_inc;
    /**< number of elements to alloc when growing free lists */

    ompi_list_t                             ib_procs;
    /**< list of ib proc structures */

    ompi_event_t                            ib_send_event;
    /**< event structure for sends */

    ompi_event_t                            ib_recv_event;
    /**< event structure for recvs */

    ompi_mutex_t                            ib_lock;
    /**< lock for accessing module state */

    int                                     ib_mem_registry_hints_log_size;
    /**< log2 size of hints hash array used by memory registry */
    
    char* ib_mpool_name; 
    /**< name of ib memory pool */ 
   
    uint32_t ib_rr_buf_max; 
    /**< the maximum number of posted rr */  
   
    uint32_t ib_rr_buf_min; 
    /**< the minimum number of posted rr */ 
    
    size_t eager_limit; 
    size_t max_send_size; 

    uint32_t leave_pinned; 

    
}; typedef struct mca_bmi_ib_component_t mca_bmi_ib_component_t;

extern mca_bmi_ib_component_t mca_bmi_ib_component;

typedef mca_bmi_base_recv_reg_t mca_bmi_ib_recv_reg_t; 
    


/**
 * IB PTL Interface
 */
struct mca_bmi_ib_module_t {
    mca_bmi_base_module_t  super;  /**< base PTL interface */
    bool bmi_inited; 
    mca_bmi_ib_recv_reg_t ib_reg[256]; 
    VAPI_hca_id_t   hca_id;        /**< ID of HCA */
    IB_port_t port_id; /**< ID of the PORT */ 
    VAPI_hca_port_t port;          /**< IB port of this PTL */
    VAPI_hca_hndl_t nic;           /**< NIC handle */
    VAPI_pd_hndl_t  ptag;          /**< Protection Domain tag */
    
    VAPI_cq_hndl_t cq_hndl_high;    /**< High Priority Completion Queue handle */ 
    VAPI_cq_hndl_t  cq_hndl_low;       /**< Low Priority Completion Queue handle */
    
    EVAPI_async_handler_hndl_t async_handler;
    /**< Async event handler used to detect weird/unknown events */
    
    ompi_free_list_t send_free_eager;    /**< free list of eager buffer descriptors */
    ompi_free_list_t send_free_max; /**< free list of max buffer descriptors */ 
    ompi_free_list_t send_free_frag; /**< free list of frags only... used for pining memory */ 
    
    ompi_free_list_t recv_free_eager;    /**< High priority free list of buffer descriptors */
    ompi_free_list_t recv_free_max;      /**< Low priority free list of buffer descriptors */ 

    
    ompi_list_t repost;            /**< list of buffers to repost */
    ompi_mutex_t ib_lock;          /**< module level lock */ 
    

    mca_mpool_base_module_t* ib_pool;  /**< ib memory pool */
    

    uint32_t rr_posted_high;  /**< number of high priority rr posted to the nic*/ 
    uint32_t rr_posted_low;  /**< number of low priority rr posted to the nic*/ 
    
    
    VAPI_rr_desc_t*                          rr_desc_post;
  
    /**< an array to allow posting of rr in one swoop */ 
    size_t ib_inline_max; /**< max size of inline send*/ 
    size_t ib_pin_min;  /**< min size to pin memory*/ 
    uint32_t ib_cq_size;   /**< Max outstanding CQE on the CQ */  
    uint32_t ib_wq_size;   /**< Max outstanding WR on the WQ */ 
    uint32_t ib_sg_list_size; /**< Max scatter/gather descriptor entries on the WQ*/ 
    uint32_t ib_pkey_ix; 
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
    uint32_t ib_src_path_bits; 

    
}; typedef struct mca_bmi_ib_module_t mca_bmi_ib_module_t;
    

    struct mca_bmi_ib_frag_t; 
extern mca_bmi_ib_module_t mca_bmi_ib_module;

/**
 * Register IB component parameters with the MCA framework
 */
extern int mca_bmi_ib_component_open(void);

/**
 * Any final cleanup before being unloaded.
 */
extern int mca_bmi_ib_component_close(void);

/**
 * IB component initialization.
 * 
 * @param num_bmi_modules (OUT)                  Number of BMIs returned in BMI array.
 * @param allow_multi_user_threads (OUT)  Flag indicating wether BMI supports user threads (TRUE)
 * @param have_hidden_threads (OUT)       Flag indicating wether BMI uses threads (TRUE)
 *
 *  (1) read interface list from kernel and compare against component parameters
 *      then create a BMI instance for selected interfaces
 *  (2) setup IB listen socket for incoming connection attempts
 *  (3) publish BMI addressing info 
 *
 */
extern mca_bmi_base_module_t** mca_bmi_ib_component_init(
    int *num_bmi_modules, 
    bool allow_multi_user_threads,
    bool have_hidden_threads
);


/**
 * IB component progress.
 */
extern int mca_bmi_ib_component_progress(
                                         void
);



/**
 * Cleanup any resources held by the BMI.
 * 
 * @param bmi  BMI instance.
 * @return     OMPI_SUCCESS or error status on failure.
 */

extern int mca_bmi_ib_finalize(
    struct mca_bmi_base_module_t* bmi
);


/**
 * PML->BMI notification of change in the process list.
 * 
 * @param bmi (IN)
 * @param nprocs (IN)     Number of processes
 * @param procs (IN)      Set of processes
 * @param peers (OUT)     Set of (optional) peer addressing info.
 * @param peers (IN/OUT)  Set of processes that are reachable via this BMI.
 * @return     OMPI_SUCCESS or error status on failure.
 * 
 */

extern int mca_bmi_ib_add_procs(
    struct mca_bmi_base_module_t* bmi,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_bmi_base_endpoint_t** peers,
    ompi_bitmap_t* reachable
);

/**
 * PML->BMI notification of change in the process list.
 *
 * @param bmi (IN)     BMI instance
 * @param nproc (IN)   Number of processes.
 * @param procs (IN)   Set of processes.
 * @param peers (IN)   Set of peer data structures.
 * @return             Status indicating if cleanup was successful
 *
 */
extern int mca_bmi_ib_del_procs(
    struct mca_bmi_base_module_t* bmi,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_bmi_base_endpoint_t** peers
);


/**
 * PML->BMI Initiate a send of the specified size.
 *
 * @param bmi (IN)               BMI instance
 * @param bmi_base_peer (IN)     BMI peer addressing
 * @param send_request (IN/OUT)  Send request (allocated by PML via mca_bmi_base_request_alloc_fn_t)
 * @param size (IN)              Number of bytes PML is requesting BMI to deliver
 * @param flags (IN)             Flags that should be passed to the peer via the message header.
 * @param request (OUT)          OMPI_SUCCESS if the BMI was able to queue one or more fragments
 */
extern int mca_bmi_ib_send(
    struct mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* bmi_peer,
    struct mca_bmi_base_descriptor_t* descriptor, 
    mca_bmi_base_tag_t tag
);

/**
 * PML->BMI Initiate a put of the specified size.
 *
 * @param bmi (IN)               BMI instance
 * @param bmi_base_peer (IN)     BMI peer addressing
 * @param send_request (IN/OUT)  Send request (allocated by PML via mca_bmi_base_request_alloc_fn_t)
 * @param size (IN)              Number of bytes PML is requesting BMI to deliver
 * @param flags (IN)             Flags that should be passed to the peer via the message header.
 * @param request (OUT)          OMPI_SUCCESS if the BMI was able to queue one or more fragments
 */
extern int mca_bmi_ib_put(
    struct mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* bmi_peer,
    struct mca_bmi_base_descriptor_t* decriptor
);

extern int mca_bmi_ib_register(
                        struct mca_bmi_base_module_t* bmi, 
                        mca_bmi_base_tag_t tag, 
                        mca_bmi_base_module_recv_cb_fn_t cbfunc, 
                        void* cbdata); 
    
/**
 * Return a recv fragment to the modules free list.
 *
 * @param bmi (IN)   BMI instance
 * @param frag (IN)  IB receive fragment
 *
 */
extern void mca_bmi_ib_recv_frag_return(
    struct mca_bmi_base_module_t* bmi,
    struct mca_bmi_ib_frag_t* frag
);

/**
 * Allocate a segment.
 *
 * @param bmi (IN)      BMI module
 * @param size (IN)     Request segment size.
 */
extern mca_bmi_base_descriptor_t* mca_bmi_ib_alloc(
                                                   struct mca_bmi_base_module_t* bmi, 
                                                   size_t size); 


/**
 * Return a segment allocated by this BMI.
 *
 * @param bmi (IN)      BMI module
 * @param segment (IN)  Allocated segment.
 */
extern int mca_bmi_ib_free(
                           struct mca_bmi_base_module_t* bmi, 
                           mca_bmi_base_descriptor_t* des); 
    
   


/**
 * Pack data and return a descriptor that can be
 * used for send/put.
 *
 * @param bmi (IN)      BMI module
 * @param peer (IN)     BMI peer addressing
 */
mca_bmi_base_descriptor_t* mca_bmi_ib_prepare_src(
    struct mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* peer,
    struct mca_bmi_base_registration_t* registration, 
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size
);

/**
 * Pack data and return a descriptor that can be
 * used for send/put.
 *
 * @param bmi (IN)      BMI module
 * @param peer (IN)     BMI peer addressing
 */
extern mca_bmi_base_descriptor_t* mca_bmi_ib_prepare_dst( 
                                                         struct mca_bmi_base_module_t* bmi, 
                                                         struct mca_bmi_base_endpoint_t* peer,
                                                         struct mca_bmi_base_registration_t* registration, 
                                                         struct ompi_convertor_t* convertor,
                                                         size_t reserve,
                                                         size_t* size); 
    /**
 * Return a send fragment to the modules free list.
 *
 * @param bmi (IN)   BMI instance
 * @param frag (IN)  IB send fragment
 *
 */
extern void mca_bmi_ib_send_frag_return(
    struct mca_bmi_base_module_t* bmi,
    struct mca_bmi_ib_frag_t*
);


int mca_bmi_ib_module_init(mca_bmi_ib_module_t* ib_bmi); 


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
