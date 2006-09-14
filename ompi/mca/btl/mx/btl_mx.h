
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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_MX_H
#define MCA_PTL_MX_H

/* Standard system includes */
#include <sys/types.h>
#include <string.h>

/* Open MPI includes */
#include "ompi/class/ompi_free_list.h"
#include "ompi/class/ompi_bitmap.h"
#include "opal/event/event.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"
#include "opal/util/output.h"
#include "ompi/mca/mpool/mpool.h" 
#include "ompi/mca/btl/btl.h"

#include "myriexpress.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * MX BTL component.
 */

struct mca_btl_mx_component_t {
    mca_btl_base_component_1_0_1_t          super;  /**< base BTL component */ 
    
    int32_t                                 mx_num_btls;
    int32_t                                 mx_max_btls;
    /**< number of hcas available to the MX component */

    struct mca_btl_mx_module_t**            mx_btls;
    /**< array of available BTL modules */

    int32_t                                 mx_free_list_num;
    /**< initial size of free lists */

    int32_t                                 mx_free_list_max;
    /**< maximum size of free lists */

    int32_t                                 mx_max_posted_recv;
    /**< number of posted receives on each NIC */

    int32_t                                 mx_free_list_inc;
    /**< number of elements to alloc when growing free lists */

    int32_t                                 mx_support_sharedmem;
    /**< true if we want to activate the MX support for shared memory */

    opal_list_t mx_procs;  /**< list of mx proc structures */

    int32_t                                 mx_filter;
    int32_t                                 mx_timeout;
    int32_t                                 mx_connection_retries;

    ompi_free_list_t mx_send_eager_frags;      /**< free list of mx eager send fragments */
    ompi_free_list_t mx_send_user_frags;       /**< free list of mx user send fragments */

    ompi_free_list_t mx_recv_frags;            /**< free list of mx recv fragments */

    opal_list_t      mx_pending_acks;          /**< queue of pending sends */

    opal_mutex_t     mx_lock;                  /**< lock for accessing module state */

    char* mx_mpool_name;                       /**< name of memory pool */ 
}; 
typedef struct mca_btl_mx_component_t mca_btl_mx_component_t;

OMPI_MODULE_DECLSPEC extern mca_btl_mx_component_t mca_btl_mx_component;

/**
 * BTL Module Interface
 */
struct mca_btl_mx_module_t {
    mca_btl_base_module_t  super;  /**< base BTL interface */
    mca_btl_base_recv_reg_t mx_reg[MCA_BTL_TAG_MAX]; 

    mx_endpoint_t      mx_endpoint;        /**<  */
    mx_endpoint_addr_t mx_endpoint_addr;   /**<  */
    opal_list_t        mx_peers;           /**<  */

    int32_t            mx_posted_request;  /**< number of posted MX request */
    opal_mutex_t       mx_lock;            /**< lock for accessing module state */
}; 
typedef struct mca_btl_mx_module_t mca_btl_mx_module_t;
extern mca_btl_mx_module_t mca_btl_mx_module;

/**
 * Register MX component parameters with the MCA framework
 */
extern int mca_btl_mx_component_open(void);

/**
 * Any final cleanup before being unloaded.
 */
extern int mca_btl_mx_component_close(void);

/**
 * MX component initialization.
 * 
 * @param num_btl_modules (OUT)           Number of BTLs returned in BTL array.
 * @param allow_multi_user_threads (OUT)  Flag indicating wether BTL supports user threads (TRUE)
 * @param have_hidden_threads (OUT)       Flag indicating wether BTL uses threads (TRUE)
 */
extern mca_btl_base_module_t** mca_btl_mx_component_init(
    int *num_btl_modules, 
    bool allow_multi_user_threads,
    bool have_hidden_threads
);


/**
 * MX component progress.
 */
extern int mca_btl_mx_component_progress(void);



/**
 * Cleanup any resources held by the BTL.
 * 
 * @param btl  BTL instance.
 * @return     OMPI_SUCCESS or error status on failure.
 */

extern int mca_btl_mx_finalize(
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

extern int mca_btl_mx_add_procs(
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

extern int mca_btl_mx_del_procs(
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
 * @param descriptor (IN)  Description of the data to be transfered
 * @param tag (IN)         The tag value used to notify the peer.
 */

extern int mca_btl_mx_send(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* btl_peer,
    struct mca_btl_base_descriptor_t* descriptor, 
    mca_btl_base_tag_t tag
);


/**
 * Register a callback function that is called on receipt
 * of a fragment.
 *
 * @param btl (IN)     BTL module
 * @return             Status indicating if registration was successful
 *
 */

extern int mca_btl_mx_register(
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

extern mca_btl_base_descriptor_t* mca_btl_mx_alloc(
    struct mca_btl_base_module_t* btl, 
    size_t size); 


/**
 * Return a segment allocated by this BTL.
 *
 * @param btl (IN)      BTL module
 * @param descriptor (IN)  Allocated descriptor.
 */

extern int mca_btl_mx_free(
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

mca_btl_base_descriptor_t* mca_btl_mx_prepare_src(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* peer,
    struct mca_mpool_base_registration_t*,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size
);

extern mca_btl_base_descriptor_t* mca_btl_mx_prepare_dst( 
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
