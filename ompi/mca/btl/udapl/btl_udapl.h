/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_BTL_UDAPL_H
#define MCA_BTL_UDAPL_H

/* Standard system includes */
#include <sys/types.h>
#include <string.h>
#include <dat/udat.h>

/* Open MPI includes */
#include "orte/class/orte_pointer_array.h"
#include "ompi/class/ompi_free_list.h"
#include "ompi/class/ompi_bitmap.h"
#include "opal/event/event.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"
#include "opal/util/output.h"
#include "ompi/mca/mpool/mpool.h" 
#include "ompi/mca/btl/btl.h"
#include "btl_udapl_endpoint.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


/**
 * uDAPL BTL component.
 */

struct mca_btl_udapl_component_t {
    mca_btl_base_component_1_0_1_t super;  /**< base BTL component */ 
    
    size_t  udapl_num_btls; /**< number of hcas available to the uDAPL component */
    size_t  udapl_max_btls; /**< maximum number of supported hcas */
    struct  mca_btl_udapl_module_t **udapl_btls; /**< array of available BTL modules */
    int32_t udapl_num_recvs;    /**< number of recv buffers to keep posted */
    int32_t udapl_num_sends;    /**< number of sends to post on endpoint */
    int32_t udapl_sr_win;       /**< number of fragments recieved before
                                   returning credits to sender */
    uint32_t udapl_timeout;      /**< connection timeout, in microseconds */
    size_t udapl_eager_frag_size;
    size_t udapl_max_frag_size;
    size_t udapl_eager_rdma_frag_size; /* size of the rdma fragement including data
                                        * payload space
                                        */
    
    int udapl_free_list_num;   /**< initial size of free lists */
    int udapl_free_list_max;   /**< maximum size of free lists */
    int udapl_free_list_inc;   /**< number of elements to alloc when growing */
    int32_t udapl_eager_rdma_num;  /**< number of rdma buffers allocated
                                      for short messages */
    int32_t udapl_max_eager_rdma_peers; /**< maximum number of peers allowed to
                                           use RDMA for short messages (cap)
                                        */
    int32_t udapl_eager_rdma_win; /**< number of eager RDMA fragments
                                    recieved before returning credits to
                                    sender */
    int32_t udapl_conn_priv_data; /**< use connect priv data for proc data */
    int32_t udapl_async_events;  /**< dequeue asynchronous events */
    int32_t udapl_buffer_alignment;  /**< preferred communication buffer alignment, in bytes */
    opal_list_t udapl_procs;   /**< list of udapl proc structures */
    opal_mutex_t udapl_lock;   /**< lock for accessing module state */
    char* udapl_mpool_name;    /**< name of memory pool */ 
    bool leave_pinned;      /**< pin memory on first use and leave pinned */
}; 
typedef struct mca_btl_udapl_component_t mca_btl_udapl_component_t;

OMPI_MODULE_DECLSPEC extern mca_btl_udapl_component_t mca_btl_udapl_component;



/**
 * BTL Module Interface
 */
struct mca_btl_udapl_module_t {
    mca_btl_base_module_t  super;  /**< base BTL interface */
    mca_btl_base_recv_reg_t udapl_reg[256]; 
    mca_btl_udapl_addr_t udapl_addr;

    /* uDAPL interface and other handles */
    DAT_IA_HANDLE udapl_ia;
    DAT_PZ_HANDLE udapl_pz;
    DAT_PSP_HANDLE udapl_psp;
    DAT_IA_ATTR udapl_ia_attr;
    
    /* event dispatchers - async, data transfer, connection negotiation */
    DAT_EVD_HANDLE udapl_evd_async;
    DAT_EVD_HANDLE udapl_evd_dto;
    DAT_EVD_HANDLE udapl_evd_conn;
    DAT_EP_PARAM   udapl_ep_param;

    /* free list of fragment descriptors */
    ompi_free_list_t udapl_frag_eager;
    ompi_free_list_t udapl_frag_max;
    ompi_free_list_t udapl_frag_user;
    ompi_free_list_t udapl_frag_control;
    
    opal_mutex_t udapl_lock;    /* lock for accessing module state */
    opal_mutex_t udapl_eager_rdma_lock;         /* eager rdma lock  */
    int32_t udapl_eager_rdma_endpoint_count;   /* count of the number of
                                                 * endpoints in
                                                 * udapl_eager_rdma_endpoints
                                                 */
    orte_pointer_array_t *udapl_eager_rdma_endpoints;   /* array of endpoints
                                                         * with eager rdma
                                                         * connections
                                                         */
    int32_t udapl_async_events;
    int32_t udapl_connect_inprogress;
    int32_t udapl_num_peers;

    /* module specific limits */
    int     udapl_async_evd_qlen;
    int     udapl_conn_evd_qlen;
    int     udapl_dto_evd_qlen;
    int udapl_max_request_dtos; /**< maximum number of outstanding consumer
                                       submitted sends and rdma operations, see
                                       section 6.6.6 of uDAPL Spec */
    int udapl_max_recv_dtos;    /**< maximum number of outstanding consumer
                                       submitted recv operations, see section
                                       6.6.6 of uDAPL Spec */
}; 
typedef struct mca_btl_udapl_module_t mca_btl_udapl_module_t;
extern mca_btl_udapl_module_t mca_btl_udapl_module;

struct mca_btl_udapl_reg_t {
    mca_mpool_base_registration_t base;
    DAT_LMR_HANDLE lmr;          /* local memory region (LMR) */
    DAT_LMR_TRIPLET lmr_triplet; /* LMR triplet - context, address, length */
    DAT_RMR_CONTEXT rmr_context; /* remote memory region context handle */

};
typedef struct mca_btl_udapl_reg_t mca_btl_udapl_reg_t;

/**
  * Report a uDAPL error - for debugging
  */

#if OMPI_ENABLE_DEBUG
extern void mca_btl_udapl_error(DAT_RETURN ret, char* str);

#define MCA_BTL_UDAPL_ERROR(ret, str) \
    mca_btl_udapl_error((ret), (str));

#else
#define MCA_BTL_UDAPL_ERROR(ret, str)
#endif


/**
 * Register uDAPL component parameters with the MCA framework
 */
extern int mca_btl_udapl_component_open(void);

/**
 * Any final cleanup before being unloaded.
 */
extern int mca_btl_udapl_component_close(void);

/**
 * uDAPL component initialization.
 * 
 * @param num_btl_modules (OUT)           Number of BTLs returned in BTL array.
 * @param allow_multi_user_threads (OUT)  Flag indicating wether BTL supports user threads (TRUE)
 * @param have_hidden_threads (OUT)       Flag indicating wether BTL uses threads (TRUE)
 */
extern mca_btl_base_module_t** mca_btl_udapl_component_init(
    int *num_btl_modules, 
    bool allow_multi_user_threads,
    bool have_hidden_threads
);


/**
 * uDAPL component progress.
 */

extern int mca_btl_udapl_component_progress(void);


/**
 * Initialize resources for a new BTL/uDAPL IA
 *
 * @param ia_name   Name of uDAPL interface adapter
 * @param btl       BTL instance.
 * @return          OMPI_SUCCESS or error status on failure.
 */

extern int mca_btl_udapl_init(
    DAT_NAME_PTR ia_name,
    struct mca_btl_udapl_module_t* btl
);


/**
 * Cleanup any resources held by the BTL.
 * 
 * @param btl  BTL instance.
 * @return     OMPI_SUCCESS or error status on failure.
 */

extern int mca_btl_udapl_finalize(
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

extern int mca_btl_udapl_add_procs(
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

extern int mca_btl_udapl_del_procs(
    struct mca_btl_base_module_t* btl,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_btl_base_endpoint_t** peers
);


/**
 * Initiate an asynchronous send.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 * @param tag (IN)         The tag value used to notify the peer.
 */

extern int mca_btl_udapl_send(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* btl_peer,
    struct mca_btl_base_descriptor_t* descriptor, 
    mca_btl_base_tag_t tag
);


/**
 * Initiate an asynchronous put.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */
                                                                                                    
extern int mca_btl_udapl_put(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* btl_peer,
    struct mca_btl_base_descriptor_t* decriptor
);


/**
 * Initiate an asynchronous get.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */
                                                                                                    
extern int mca_btl_udapl_get(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* btl_peer,
    struct mca_btl_base_descriptor_t* decriptor
);

/**
 * Register a callback function that is called on receipt
 * of a fragment.
 *
 * @param btl (IN)     BTL module
 * @return             Status indicating if registration was successful
 *
 */

extern int mca_btl_udapl_register(
    struct mca_btl_base_module_t* btl, 
    mca_btl_base_tag_t tag, 
    mca_btl_base_module_recv_cb_fn_t cbfunc, 
    void* cbdata); 
    
/**
 * Allocate a descriptor with a segment of the requested size.
 * Note that the BTL layer may choose to return a smaller size
 * if it cannot support the request.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */

extern mca_btl_base_descriptor_t* mca_btl_udapl_alloc(
    struct mca_btl_base_module_t* btl, 
    size_t size); 


/**
 * Return a segment allocated by this BTL.
 *
 * @param btl (IN)      BTL module
 * @param descriptor (IN)  Allocated descriptor.
 */

extern int mca_btl_udapl_free(
    struct mca_btl_base_module_t* btl, 
    mca_btl_base_descriptor_t* des); 
    

/**
 * Prepare a descriptor for send/rdma using the supplied
 * convertor. If the convertor references data that is contigous,
 * the descriptor may simply point to the user buffer. Otherwise,
 * this routine is responsible for allocating buffer space and
 * packing if required.
 *
 * @param btl (IN)          BTL module
 * @param endpoint (IN)     BTL peer addressing
 * @param convertor (IN)    Data type convertor
 * @param reserve (IN)      Additional bytes requested by upper layer to precede user data
 * @param size (IN/OUT)     Number of bytes to prepare (IN), number of bytes actually prepared (OUT) 
*/

mca_btl_base_descriptor_t* mca_btl_udapl_prepare_src(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* peer,
    struct mca_mpool_base_registration_t*,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size
);

extern mca_btl_base_descriptor_t* mca_btl_udapl_prepare_dst( 
    struct mca_btl_base_module_t* btl, 
    struct mca_btl_base_endpoint_t* peer,
    struct mca_mpool_base_registration_t*,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size); 



#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
