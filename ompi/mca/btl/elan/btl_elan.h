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
    mca_btl_elan_frag_t recv_frag;
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

extern int mca_btl_elan_finalize( struct mca_btl_base_module_t* btl );

extern int mca_btl_elan_ft_event(int state);

END_C_DECLS

#endif  /* MCA_BTL_ELAN_H */

