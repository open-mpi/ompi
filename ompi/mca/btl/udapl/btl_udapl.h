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
#include "class/ompi_free_list.h"
#include "class/ompi_bitmap.h"
#include "opal/event/event.h"
#include "mca/pml/pml.h"
#include "mca/btl/btl.h"
#include "mca/btl/base/base.h"
#include "opal/util/output.h"
#include "mca/mpool/mpool.h" 
#include "mca/btl/btl.h"
#include "btl_udapl_endpoint.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


/**
 * uDAPL BTL component.
 */

struct mca_btl_udapl_component_t {
    mca_btl_base_component_1_0_0_t super;  /**< base BTL component */ 
    
    size_t  udapl_num_btls; /**< number of hcas available to the uDAPL component */
    size_t  udapl_max_btls; /**< maximum number of supported hcas */
    struct  mca_btl_udapl_module_t **udapl_btls; /**< array of available BTL modules */
    size_t  udapl_num_mru;
    size_t  udapl_evd_qlen;
    size_t  udapl_eager_frag_size;
    size_t  udapl_max_frag_size;
    char*   udapl_port_name; 
    int32_t udapl_num_repost;
    int32_t udapl_num_high_priority; /**< number of receive descriptors at high priority */
    int     udapl_debug;        /**< turn on debug output */

    int udapl_free_list_num;   /**< initial size of free lists */
    int udapl_free_list_max;   /**< maximum size of free lists */
    int udapl_free_list_inc;   /**< number of elements to alloc when growing free lists */
    opal_list_t udapl_procs;   /**< list of udapl proc structures */
    opal_mutex_t udapl_lock;   /**< lock for accessing module state */
    char* udapl_mpool_name;    /**< name of memory pool */ 
    bool leave_pinned;      /**< pin memory on first use and leave pinned */
}; 
typedef struct mca_btl_udapl_component_t mca_btl_udapl_component_t;

extern mca_btl_udapl_component_t mca_btl_udapl_component;



/**
 * BTL Module Interface
 */
struct mca_btl_udapl_module_t {
    mca_btl_base_module_t  super;  /**< base BTL interface */
    mca_btl_base_recv_reg_t udapl_reg[256]; 
    mca_btl_udapl_addr_t udapl_addr;

    /* interface handle */
    DAT_IA_HANDLE udapl_ia;

    /* event dispatchers - default, data transfer, connection negotiation */
    DAT_EVD_HANDLE udapl_evd_dflt;
    DAT_EVD_HANDLE udapl_evd_dto;
    DAT_EVD_HANDLE udapl_evd_conn;

    /* free list of fragment descriptors */
    ompi_free_list_t udapl_frag_eager;
    ompi_free_list_t udapl_frag_max;
    ompi_free_list_t udapl_frag_user;

    /* number of send/recv tokens */
#if 0
    int32_t udapl_num_send_tokens;
    int32_t udapl_max_send_tokens;
    int32_t udapl_num_recv_tokens;
    int32_t udapl_max_recv_tokens;
    int32_t udapl_num_repost;
#endif

    /* lock for accessing module state */
    opal_list_t udapl_pending; /**< list of pending send descriptors */
    opal_list_t udapl_repost; /**< list of pending fragments */
    opal_list_t udapl_mru_reg; /**< list of most recently used registrations */
    opal_mutex_t udapl_lock;
}; 
typedef struct mca_btl_udapl_module_t mca_btl_udapl_module_t;
extern mca_btl_udapl_module_t mca_btl_udapl_module;


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
 * @param descriptor (IN)  Description of the data to be transfered
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


/** 
 * Acquire a send token - queue the fragment if none available
 */

#define MCA_BTL_UDAPL_ACQUIRE_TOKEN(btl, frag)                                                  \
do {                                                                                            \
    /* queue the descriptor if there are no send tokens */                                      \
    if(OPAL_THREAD_ADD32(&udapl_btl->udapl_num_send_tokens, -1) < 0) {                          \
        OPAL_THREAD_LOCK(&udapl_btl->udapl_lock);                                               \
        opal_list_append(&udapl_btl->udapl_pending, (opal_list_item_t*)frag);                   \
        OPAL_THREAD_UNLOCK(&udapl_btl->udapl_lock);                                             \
        OPAL_THREAD_ADD32(&udapl_btl->udapl_num_send_tokens, 1);                                \
        return OMPI_SUCCESS;                                                                    \
    }                                                                                           \
} while (0)                                                                                     \

/**
 * Return send token and dequeue and pending fragments 
 */

#define MCA_BTL_UDAPL_RETURN_TOKEN(btl)                                                         \
do {                                                                                            \
   OPAL_THREAD_ADD32( &btl->udapl_num_send_tokens, 1 );                                         \
   if(opal_list_get_size(&btl->udapl_pending)) {                                                \
       mca_btl_udapl_frag_t* frag;                                                              \
       OPAL_THREAD_LOCK(&btl->udapl_lock);                                                      \
       frag = (mca_btl_udapl_frag_t*)opal_list_remove_first(&btl->udapl_pending);               \
       OPAL_THREAD_UNLOCK(&btl->udapl_lock);                                                    \
       if(NULL != frag) {                                                                       \
           switch(frag->type) {                                                                 \
           case MCA_BTL_UDAPL_SEND:                                                             \
               mca_btl_udapl_send(&btl->super, frag->endpoint, &frag->base, frag->hdr->tag);    \
               break;                                                                           \
           case MCA_BTL_UDAPL_PUT:                                                              \
               mca_btl_udapl_put(&btl->super, frag->endpoint, &frag->base);                     \
               break;                                                                           \
           case MCA_BTL_UDAPL_GET:                                                              \
               mca_btl_udapl_get(&btl->super, frag->endpoint, &frag->base);                     \
               break;                                                                           \
           }                                                                                    \
       }                                                                                        \
    }                                                                                           \
} while (0)


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
