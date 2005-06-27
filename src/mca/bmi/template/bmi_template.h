
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
#ifndef MCA_PTL_TEMPLATE_H
#define MCA_PTL_TEMPLATE_H

/* Standard system includes */
#include <sys/types.h>
#include <string.h>

/* Open MPI includes */
#include "class/ompi_free_list.h"
#include "class/ompi_bitmap.h"
#include "event/event.h"
#include "mca/pml/pml.h"
#include "mca/bmi/bmi.h"
#include "mca/bmi/base/base.h"
#include "util/output.h"
#include "mca/mpool/mpool.h" 
#include "mca/bmi/bmi.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

#define MCA_BMI_HAS_MPOOL 1

/**
 * Infiniband (TEMPLATE) BMI component.
 */

struct mca_bmi_template_component_t {
    mca_bmi_base_component_1_0_0_t          super;  /**< base BMI component */ 
    
    uint32_t                                template_num_bmis;
    /**< number of hcas available to the TEMPLATE component */

    struct mca_bmi_template_module_t       *template_bmis;
    /**< array of available BMI modules */

    int template_free_list_num;
    /**< initial size of free lists */

    int template_free_list_max;
    /**< maximum size of free lists */

    int template_free_list_inc;
    /**< number of elements to alloc when growing free lists */

    ompi_list_t template_procs;
    /**< list of template proc structures */

    ompi_mutex_t template_lock;
    /**< lock for accessing module state */

    char* template_mpool_name; 
    /**< name of memory pool */ 

    bool leave_pinned;
    /**< pin memory on first use and leave pinned */
}; 
typedef struct mca_bmi_template_component_t mca_bmi_template_component_t;

extern mca_bmi_template_component_t mca_bmi_template_component;



/**
 * BMI Module Interface
 */
struct mca_bmi_template_module_t {
    mca_bmi_base_module_t  super;  /**< base BMI interface */
    mca_bmi_base_recv_reg_t template_reg[256]; 

    /* free list of fragment descriptors */
    ompi_free_list_t template_frag_eager;
    ompi_free_list_t template_frag_max;
    ompi_free_list_t template_frag_user;

    /* lock for accessing module state */
    ompi_mutex_t template_lock;

#if MCA_BMI_HAS_MPOOL
    struct mca_mpool_base_module_t* template_mpool;
#endif
}; 
typedef struct mca_bmi_template_module_t mca_bmi_template_module_t;
extern mca_bmi_template_module_t mca_bmi_template_module;


/**
 * Register TEMPLATE component parameters with the MCA framework
 */
extern int mca_bmi_template_component_open(void);

/**
 * Any final cleanup before being unloaded.
 */
extern int mca_bmi_template_component_close(void);

/**
 * TEMPLATE component initialization.
 * 
 * @param num_bmi_modules (OUT)           Number of BMIs returned in BMI array.
 * @param allow_multi_user_threads (OUT)  Flag indicating wether BMI supports user threads (TRUE)
 * @param have_hidden_threads (OUT)       Flag indicating wether BMI uses threads (TRUE)
 */
extern mca_bmi_base_module_t** mca_bmi_template_component_init(
    int *num_bmi_modules, 
    bool allow_multi_user_threads,
    bool have_hidden_threads
);


/**
 * TEMPLATE component progress.
 */
extern int mca_bmi_template_component_progress(void);



/**
 * Cleanup any resources held by the BMI.
 * 
 * @param bmi  BMI instance.
 * @return     OMPI_SUCCESS or error status on failure.
 */

extern int mca_bmi_template_finalize(
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

extern int mca_bmi_template_add_procs(
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

extern int mca_bmi_template_del_procs(
    struct mca_bmi_base_module_t* bmi,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_bmi_base_endpoint_t** peers
);


/**
 * Initiate an asynchronous send.
 *
 * @param bmi (IN)         BMI module
 * @param endpoint (IN)    BMI addressing information
 * @param descriptor (IN)  Description of the data to be transfered
 * @param tag (IN)         The tag value used to notify the peer.
 */

extern int mca_bmi_template_send(
    struct mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* bmi_peer,
    struct mca_bmi_base_descriptor_t* descriptor, 
    mca_bmi_base_tag_t tag
);


/**
 * Initiate an asynchronous put.
 *
 * @param bmi (IN)         BMI module
 * @param endpoint (IN)    BMI addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */
                                                                                                    
extern int mca_bmi_template_put(
    struct mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* bmi_peer,
    struct mca_bmi_base_descriptor_t* decriptor
);


/**
 * Initiate an asynchronous get.
 *
 * @param bmi (IN)         BMI module
 * @param endpoint (IN)    BMI addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */
                                                                                                    
extern int mca_bmi_template_get(
    struct mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* bmi_peer,
    struct mca_bmi_base_descriptor_t* decriptor
);

/**
 * Register a callback function that is called on receipt
 * of a fragment.
 *
 * @param bmi (IN)     BMI module
 * @return             Status indicating if registration was successful
 *
 */

extern int mca_bmi_template_register(
    struct mca_bmi_base_module_t* bmi, 
    mca_bmi_base_tag_t tag, 
    mca_bmi_base_module_recv_cb_fn_t cbfunc, 
    void* cbdata); 
    
/**
 * Allocate a descriptor with a segment of the requested size.
 * Note that the BMI layer may choose to return a smaller size
 * if it cannot support the request.
 *
 * @param bmi (IN)      BMI module
 * @param size (IN)     Request segment size.
 */

extern mca_bmi_base_descriptor_t* mca_bmi_template_alloc(
    struct mca_bmi_base_module_t* bmi, 
    size_t size); 


/**
 * Return a segment allocated by this BMI.
 *
 * @param bmi (IN)      BMI module
 * @param descriptor (IN)  Allocated descriptor.
 */

extern int mca_bmi_template_free(
    struct mca_bmi_base_module_t* bmi, 
    mca_bmi_base_descriptor_t* des); 
    

/**
 * Prepare a descriptor for send/rdma using the supplied
 * convertor. If the convertor references data that is contigous,
 * the descriptor may simply point to the user buffer. Otherwise,
 * this routine is responsible for allocating buffer space and
 * packing if required.
 *
 * @param bmi (IN)          BMI module
 * @param endpoint (IN)     BMI peer addressing
 * @param convertor (IN)    Data type convertor
 * @param reserve (IN)      Additional bytes requested by upper layer to precede user data
 * @param size (IN/OUT)     Number of bytes to prepare (IN), number of bytes actually prepared (OUT) 
*/

mca_bmi_base_descriptor_t* mca_bmi_template_prepare_src(
    struct mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* peer,
    struct mca_mpool_base_registration_t*,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size
);

extern mca_bmi_base_descriptor_t* mca_bmi_template_prepare_dst( 
    struct mca_bmi_base_module_t* bmi, 
    struct mca_bmi_base_endpoint_t* peer,
    struct mca_mpool_base_registration_t*,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size); 


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
