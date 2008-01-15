/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_BTL_ELAN_H
#define MCA_BTL_ELAN_H

#include "ompi_config.h"


/* Standard system includes */
#include <sys/types.h>
#include <string.h>
#include <stdio.h>

/* Open MPI includes */
#include "ompi/class/ompi_free_list.h"
#include "ompi/class/ompi_bitmap.h"
#include "opal/event/event.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "opal/util/output.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/mca/btl/base/btl_base_error.h"

#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h" 
#include "btl_elan_endpoint.h" 
#include "btl_elan_frag.h"

#include "elan3/elan3.h"
#include "elan/elan.h"

BEGIN_C_DECLS

/**
 * ELAN BTL component.
 */

struct mca_btl_elan_component_t {
    mca_btl_base_component_1_0_1_t          super;  /**< base BTL component */ 

    uint32_t                                ib_max_btls;
    /**< maximum number of hcas available to the ELAN component */
	
    uint32_t                                elan_num_btls;
    /**< number of hcas available to the ELAN component */

    struct mca_btl_elan_module_t            **elan_btls;
    /**< array of available BTL modules */

    int                                     elan_free_list_num;
    /**< initial size of free lists */

    int                                     elan_free_list_max;
    /**< maximum size of free lists */

    int                                     elan_free_list_inc;
    /**< number of elements to alloc when growing free lists */

    int                                     elan_max_posted_recv;
    /**< number of pre-posted receives */

    /* free list of fragment descriptors */
    ompi_free_list_t                        elan_frag_eager;
    ompi_free_list_t                        elan_frag_max;
    ompi_free_list_t                        elan_frag_user;

    opal_list_t                             elan_procs;
    /**< list of elan proc structures */

    opal_mutex_t                            elan_lock;
    /**< lock for accessing module state */
	

    char* elan_mpool_name; 
    /**< name of memory pool */ 

    char* elanidmap_file;  /**< name of the ELANIDMAP file */

    bool leave_pinned;
    /**< pin memory on first use and leave pinned */
	
}; 
    typedef struct mca_btl_elan_component_t mca_btl_elan_component_t;

    OMPI_MODULE_DECLSPEC extern mca_btl_elan_component_t mca_btl_elan_component;


/**
 * BTL Module Interface
 */

struct mca_btl_elan_module_t {
    mca_btl_base_module_t  super;  /**< base BTL interface */
    ELAN_STATE     *state;
    ELAN_BASE      *base;
    ELAN_TPORT     *tport;          /* What we actually use for moving messages */
    ELAN_QUEUE	   *queue;
    ELAN_GROUP     *group;          /* The group with everyone in      */
    unsigned int   elan_vp;      /**< elan vpid, not ompi vpid */
    unsigned int   elan_nvp;     /**< total # of elan vpid */
    opal_mutex_t   elan_lock;
    opal_list_t    recv_list;  /* list of pending receives. */
    opal_list_t    send_list;  /* list of posted sends */
    opal_list_t    rdma_list;  /* list of posted receives */
    struct bufdesc_t *    tportFIFOHead;
    struct bufdesc_t *    tportFIFOTail;
    struct mca_mpool_base_module_t* elan_mpool;
}; 
typedef struct mca_btl_elan_module_t mca_btl_elan_module_t;
extern mca_btl_elan_module_t mca_btl_elan_module;

struct bufdesc_t {
    ELAN_EVENT          * eve;       
    struct mca_btl_elan_frag_t * frag;
    struct bufdesc_t    * next;
};
typedef struct bufdesc_t bufdesc_t;

/**
 * Register ELAN component parameters with the MCA framework
 */
extern int mca_btl_elan_component_open(void);

/**
 * Any final cleanup before being unloaded.
 */
extern int mca_btl_elan_component_close(void);

/**
 * ELAN component initialization.
 * 
 * @param num_btl_modules (OUT)           Number of BTLs returned in BTL array.
 * @param allow_multi_user_threads (OUT)  Flag indicating wether BTL supports user threads (TRUE)
 * @param have_hidden_threads (OUT)       Flag indicating wether BTL uses threads (TRUE)
 */
extern mca_btl_base_module_t**
mca_btl_elan_component_init( int* num_btl_modules, 
                             bool allow_multi_user_threads,
                             bool have_hidden_threads );

/**
 * ELAN component progress.
 */
extern int mca_btl_elan_component_progress(void);

/**
 * Cleanup any resources held by the BTL.
 * 
 * @param btl  BTL instance.
 * @return     OMPI_SUCCESS or error status on failure.
 */
extern void cancel_elanRx( mca_btl_elan_module_t* elan_btl );

extern int mca_btl_elan_finalize( struct mca_btl_base_module_t* btl );

extern int mca_btl_elan_ft_event(int state);

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

extern int mca_btl_elan_add_procs( struct mca_btl_base_module_t* btl,
                                   size_t nprocs,
                                   struct ompi_proc_t **procs,
                                   struct mca_btl_base_endpoint_t** peers,
                                   ompi_bitmap_t* reachable );

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

extern int mca_btl_elan_del_procs( struct mca_btl_base_module_t* btl,
                                   size_t nprocs,
                                   struct ompi_proc_t **procs,
                                   struct mca_btl_base_endpoint_t** peers );

/**
 * Initiate an asynchronous send.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transfered
 * @param tag (IN)         The tag value used to notify the peer.
 */

extern int mca_btl_elan_send( struct mca_btl_base_module_t* btl,
                              struct mca_btl_base_endpoint_t* btl_peer,
                              struct mca_btl_base_descriptor_t* descriptor, 
                              mca_btl_base_tag_t tag );

/**
 * Initiate an asynchronous put.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */

extern int mca_btl_elan_put( struct mca_btl_base_module_t* btl,
                             struct mca_btl_base_endpoint_t* btl_peer,
                             struct mca_btl_base_descriptor_t* decriptor );

/**
 * Initiate an asynchronous get.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */

extern int mca_btl_elan_get( struct mca_btl_base_module_t* btl,
                             struct mca_btl_base_endpoint_t* btl_peer,
                             struct mca_btl_base_descriptor_t* decriptor );

/**
 * Allocate a descriptor with a segment of the requested size.
 * Note that the BTL layer may choose to return a smaller size
 * if it cannot support the request.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */

extern mca_btl_base_descriptor_t*
mca_btl_elan_alloc( struct mca_btl_base_module_t* btl,
                    struct mca_btl_base_endpoint_t* peer,
                    uint8_t order,
                    size_t size,
                    uint32_t flags );

/**
 * Return a segment allocated by this BTL.
 *
 * @param btl (IN)      BTL module
 * @param descriptor (IN)  Allocated descriptor.
 */

extern int mca_btl_elan_free( struct mca_btl_base_module_t* btl, 
                              mca_btl_base_descriptor_t* des );

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

mca_btl_base_descriptor_t*
mca_btl_elan_prepare_src( struct mca_btl_base_module_t* btl,
                          struct mca_btl_base_endpoint_t* peer,
                          struct mca_mpool_base_registration_t*,
                          struct ompi_convertor_t* convertor,
                          uint8_t order,
                          size_t reserve,
                          size_t* size,
                          uint32_t flags );

extern mca_btl_base_descriptor_t*
mca_btl_elan_prepare_dst( struct mca_btl_base_module_t* btl, 
                          struct mca_btl_base_endpoint_t* peer,
                          struct mca_mpool_base_registration_t*,
                          struct ompi_convertor_t* convertor,
                          uint8_t order,
                          size_t reserve,
                          size_t* size,
                          uint32_t flags );

END_C_DECLS

#endif  /* MCA_BTL_ELAN_H */

