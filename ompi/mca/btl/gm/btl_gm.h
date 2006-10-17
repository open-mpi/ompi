
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
#ifndef MCA_BTL_GM_H
#define MCA_BTL_GM_H

/* Standard system includes */
#include <sys/types.h>
#include <string.h>
#include <gm.h>

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
#include "btl_gm_endpoint.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

#define GM_BUFFER_SIZE   7
#define GM_BUFFER_LENGTH gm_max_length_for_size(GM_BUFFER_SIZE)

/**
 * Myrinet (GM) BTL component.
 */

struct mca_btl_gm_component_t {
    mca_btl_base_component_1_0_1_t super;  /**< base BTL component */ 
    
    size_t  gm_num_btls; /**< number of hcas available to the GM component */
    size_t  gm_max_btls; /**< maximum number of supported hcas */
    struct  mca_btl_gm_module_t **gm_btls; /**< array of available BTL modules */
    size_t  gm_max_ports;  /**< maximum number of ports per board */
    size_t  gm_max_boards; /**< maximum number of boards */
    size_t  gm_eager_frag_size;
    size_t  gm_max_frag_size;
    char*   gm_port_name; 
    int32_t gm_num_repost;
    int32_t gm_num_high_priority; /**< number of receive descriptors at high priority */
    int     gm_debug;        /**< turn on debug output */

    int gm_free_list_num;   /**< initial size of free lists */
    int gm_free_list_max;   /**< maximum size of free lists */
    int gm_free_list_inc;   /**< number of elements to alloc when growing free lists */
    opal_list_t gm_procs;   /**< list of gm proc structures */
    opal_mutex_t gm_lock;   /**< lock for accessing module state */
    char* gm_mpool_name;    /**< name of memory pool */ 
    bool leave_pinned;      /**< pin memory on first use and leave pinned */
}; 
typedef struct mca_btl_gm_component_t mca_btl_gm_component_t;

OMPI_MODULE_DECLSPEC extern mca_btl_gm_component_t mca_btl_gm_component;

/**
 * BTL Module Interface
 */
struct mca_btl_gm_module_t {
    mca_btl_base_module_t  super;  /**< base BTL interface */
    mca_btl_base_recv_reg_t gm_reg[256]; 

    /* local port handle/address */
    struct gm_port *port;
    mca_btl_gm_addr_t gm_addr;

    /* free list of fragment descriptors */
    ompi_free_list_t gm_frag_eager;
    ompi_free_list_t gm_frag_max;
    ompi_free_list_t gm_frag_user;

    /* number of send/recv tokens */
    int32_t gm_num_send_tokens;
    int32_t gm_max_send_tokens;
    int32_t gm_num_recv_tokens;
    int32_t gm_max_recv_tokens;
    int32_t gm_num_repost;

    /* lock for accessing module state */
    opal_list_t gm_pending; /**< list of pending send descriptors */
    opal_list_t gm_repost; /**< list of pending fragments */

#if OMPI_ENABLE_PROGRESS_THREADS
    opal_thread_t gm_thread;
    bool gm_progress;
#endif
    mca_btl_base_module_error_cb_fn_t error_cb; 
}; 
typedef struct mca_btl_gm_module_t mca_btl_gm_module_t;
extern mca_btl_gm_module_t mca_btl_gm_module;


/**
 * Register GM component parameters with the MCA framework
 */
extern int mca_btl_gm_component_open(void);

/**
 * Any final cleanup before being unloaded.
 */
extern int mca_btl_gm_component_close(void);

/**
 * GM component initialization.
 * 
 * @param num_btl_modules (OUT)           Number of BTLs returned in BTL array.
 * @param allow_multi_user_threads (OUT)  Flag indicating wether BTL supports user threads (TRUE)
 * @param have_hidden_threads (OUT)       Flag indicating wether BTL uses threads (TRUE)
 */
extern mca_btl_base_module_t** mca_btl_gm_component_init(
    int *num_btl_modules, 
    bool allow_multi_user_threads,
    bool have_hidden_threads
);


/**
 * GM component progress.
 */
extern int mca_btl_gm_component_progress(void);



/**
 * Cleanup any resources held by the BTL.
 * 
 * @param btl  BTL instance.
 * @return     OMPI_SUCCESS or error status on failure.
 */

extern int mca_btl_gm_finalize(
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

extern int mca_btl_gm_add_procs(
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

extern int mca_btl_gm_del_procs(
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

extern int mca_btl_gm_send(
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
                                                                                                    
extern int mca_btl_gm_put(
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
                                                                                                    
extern int mca_btl_gm_get(
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

extern int mca_btl_gm_register(
    struct mca_btl_base_module_t* btl, 
    mca_btl_base_tag_t tag, 
    mca_btl_base_module_recv_cb_fn_t cbfunc, 
    void* cbdata); 


/**
 * Register a callback function that is called on error..
 *
 * @param btl (IN)     BTL module
 * @return             Status indicating if cleanup was successful
 */

int mca_btl_gm_register_error_cb(
    struct mca_btl_base_module_t* btl,
    mca_btl_base_module_error_cb_fn_t cbfunc
);

/**
 * Register a callback function that is called on error.
 *
 * @param btl (IN)     BTL module
 * @return             Status indicating if registration was successful
 *
 */

extern int mca_btl_gm_register_error_cb(
    struct mca_btl_base_module_t* btl, 
    mca_btl_base_module_error_cb_fn_t cbfunc); 
    
/**
 * Allocate a descriptor with a segment of the requested size.
 * Note that the BTL layer may choose to return a smaller size
 * if it cannot support the request.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */

extern mca_btl_base_descriptor_t* mca_btl_gm_alloc(
    struct mca_btl_base_module_t* btl, 
    size_t size); 


/**
 * Return a segment allocated by this BTL.
 *
 * @param btl (IN)      BTL module
 * @param descriptor (IN)  Allocated descriptor.
 */

extern int mca_btl_gm_free(
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

mca_btl_base_descriptor_t* mca_btl_gm_prepare_src(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* peer,
    struct mca_mpool_base_registration_t*,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size
);

extern mca_btl_base_descriptor_t* mca_btl_gm_prepare_dst( 
    struct mca_btl_base_module_t* btl, 
    struct mca_btl_base_endpoint_t* peer,
    struct mca_mpool_base_registration_t*,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size); 


/** 
 * Acquire a send token - queue the fragment if none available
 */

#define MCA_BTL_GM_ACQUIRE_TOKEN_NL(btl, frag)                                                  \
do {                                                                                            \
    /* queue the descriptor if there are no send tokens */                                      \
    if(OPAL_THREAD_ADD32(&gm_btl->gm_num_send_tokens, -1) < 0) {                                \
        opal_list_append(&gm_btl->gm_pending, (opal_list_item_t*)frag);                         \
        OPAL_THREAD_ADD32(&gm_btl->gm_num_send_tokens, 1);                                      \
        return OMPI_SUCCESS;                                                                    \
    }                                                                                           \
} while (0)                                                                                     \


#define MCA_BTL_GM_ACQUIRE_TOKEN(btl, frag)                                                     \
do {                                                                                            \
    /* queue the descriptor if there are no send tokens */                                      \
    if(OPAL_THREAD_ADD32(&gm_btl->gm_num_send_tokens, -1) < 0) {                                \
        opal_list_append(&gm_btl->gm_pending, (opal_list_item_t*)frag);                         \
        OPAL_THREAD_ADD32(&gm_btl->gm_num_send_tokens, 1);                                      \
        OPAL_THREAD_UNLOCK(&mca_btl_gm_component.gm_lock);                                      \
        return OMPI_SUCCESS;                                                                    \
    }                                                                                           \
} while (0)                                                                                     \

/**
 * Return send token and dequeue and pending fragments 
 * mca_btl_gm_component.gm_lock is already held.
 */

#define MCA_BTL_GM_RETURN_TOKEN(btl)                                                            \
do {                                                                                            \
   OPAL_THREAD_ADD32( &btl->gm_num_send_tokens, 1 );                                            \
   if(opal_list_get_size(&btl->gm_pending)) {                                                   \
       mca_btl_gm_frag_t* frag;                                                                 \
       frag = (mca_btl_gm_frag_t*)opal_list_remove_first(&btl->gm_pending);                     \
       if(NULL != frag) {                                                                       \
           switch(frag->type) {                                                                 \
           case MCA_BTL_GM_SEND:                                                                \
           case MCA_BTL_GM_EAGER:                                                               \
               mca_btl_gm_send_nl(&btl->super, frag->endpoint, &frag->base, frag->hdr->tag);    \
               break;                                                                           \
           case MCA_BTL_GM_PUT:                                                                 \
               mca_btl_gm_put_nl(&btl->super, frag->endpoint, &frag->base);                     \
               break;                                                                           \
           case MCA_BTL_GM_GET:                                                                 \
               mca_btl_gm_get_nl(&btl->super, frag->endpoint, &frag->base);                     \
               break;                                                                           \
           }                                                                                    \
       }                                                                                        \
    }                                                                                           \
} while (0)


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
