
/*
 * Copyright (c) 2004-2009 The University of Tennessee and The University
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
#ifndef MCA_PTL_SICORTEX_H
#define MCA_PTL_SICORTEX_H

/* Standard system includes */
#include <sys/types.h>
#include <string.h>

/* Open MPI includes */
#include "opal/class/opal_bitmap.h"
#include "opal/event/event.h"
#include "orte/util/show_help.h"
#include "ompi/class/ompi_free_list.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/mca/mpool/mpool.h" 
#include "ompi/mca/btl/btl.h"


#include <unistd.h>
#include <linux/sicortex/scdma_hw.h>
#include <linux/sicortex/scdma.h>
#include <assert.h>


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

#define MCA_BTL_HAS_MPOOL 0
#define SICORTEX_PORTS_NUM 3
#define BD_SIZE 64*1024
/*
 * Infiniband (SICORTEX) BTL component.
 */

enum eventcodes { EVENT_SEND = (SCDMA_HW_EVENT_TYPE_LIMIT + 1),
		  EVENT_SEND_IM,
                  EVENT_SEND_PUT_ALIGN,
		  EVENT_SEND_IM_PUT_POST,
		  EVENT_PUT };

struct peer_struct_t {
    uint64_t context_id;
    uint32_t route_handles[3];
    uint32_t ports[3];
};
typedef struct peer_struct_t peer_struct_t;


struct mca_btl_sicortex_component_t {
    mca_btl_base_component_2_0_0_t          super;  /**< base BTL component */ 
    
    size_t                                sicortex_num_btls;
    /**< number of hcas available to the SICORTEX component */

    size_t                                sicortex_max_btls;

    struct mca_btl_sicortex_module_t      **sicortex_btls;
    /**< array of available BTL modules */

    int sicortex_free_list_num;
    /**< initial size of free lists */

    int sicortex_free_list_max;
    /**< maximum size of free lists */

    int sicortex_free_list_inc;
    /**< number of elements to alloc when growing free lists */

    opal_list_t sicortex_procs;
    /**< list of sicortex proc structures */

    opal_mutex_t sicortex_lock;
    /**< lock for accessing module state */

    ompi_free_list_t 	sicortex_frag_eager;
    ompi_free_list_t 	sicortex_frag_max;
    ompi_free_list_t 	sicortex_frag_user;

    char* sicortex_mpool_name; 
    /**< name of memory pool */ 

    bool leave_pinned;
    /**< pin memory on first use and leave pinned */

}; 
typedef struct mca_btl_sicortex_component_t mca_btl_sicortex_component_t;

OMPI_MODULE_DECLSPEC extern mca_btl_sicortex_component_t mca_btl_sicortex_component;

/**
 * BTL Module Interface
 */
struct mca_btl_sicortex_module_t {
    mca_btl_base_module_t  super;  

    scdma_context_t 	*ctx;
    uint64_t 		localContext;
    struct  		mca_btl_base_endpoint_t** peers;
    uint32_t   		bds_size;
    uint32_t  		bds_limit;
    uint32_t  		bds_head;
    int 		handle_count;
    opal_mutex_t 	sicortex_lock;
     
#if MCA_BTL_HAS_MPOOL
    struct mca_mpool_base_module_t* sicortex_mpool;
#endif
}; 
typedef struct mca_btl_sicortex_module_t mca_btl_sicortex_module_t;
extern mca_btl_sicortex_module_t mca_btl_sicortex_module;


/**
 * Register SICORTEX component parameters with the MCA framework
 */
extern int mca_btl_sicortex_component_open(void);

/**
 * Any final cleanup before being unloaded.
 */
extern int mca_btl_sicortex_component_close(void);

/**
 * SICORTEX component initialization.
 * 
 * @param num_btl_modules (OUT)           Number of BTLs returned in BTL array.
 * @param allow_multi_user_threads (OUT)  Flag indicating wether BTL supports user threads (TRUE)
 * @param have_hidden_threads (OUT)       Flag indicating wether BTL uses threads (TRUE)
 */
extern mca_btl_base_module_t** mca_btl_sicortex_component_init(
    int *num_btl_modules, 
    bool allow_multi_user_threads,
    bool have_hidden_threads
);


/**
 * SICORTEX component progress.
 */
extern int mca_btl_sicortex_component_progress(void);






#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
