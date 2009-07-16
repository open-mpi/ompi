/*
 * Copyright (c) 2009      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_BTL_PCIE_H
#define MCA_BTL_PCIE_H

#include "ompi_config.h"

#include <sys/types.h>
#include <string.h>

#include "opal/align.h"
#include "opal/event/event.h"
#include "opal/util/output.h"
#include "opal/class/opal_bitmap.h"

#include "orte/util/proc_info.h"

#include "ompi/class/ompi_free_list.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/mca/mpool/mpool.h" 
#include "ompi/mca/pml/pml.h"

#include "btl_pcie_ddriver.h"
#include "btl_pcie_frag.h"
#include "btl_pcie_fifo.h"

BEGIN_C_DECLS

#define MCA_BTL_HAS_MPOOL 1

/**
 * PCIE BTL component.
 */

struct mca_btl_pcie_component_t {
    /** BTL base component */
    mca_btl_base_component_1_0_1_t super;

    /* ***** Configuration information ***** */
 
    /** initial size of free lists */
    int pcie_free_list_num;

    /** maximum size of free lists */
    int pcie_free_list_max;

    /** number of elements to alloc when growing free lists */
    int pcie_free_list_inc;

    /** name of send/recv memory pool */ 
    char* pcie_send_mpool_name; 

    /** name of put/get memory pool */
    char *pcie_dma_mpool_name;

    /** Number of entries in the send/recv queue structure */
    int pcie_recv_queue_len;

    /* **** Component data ***** */

    /** array of available modules */
    struct mca_btl_pcie_module_t *pcie_btls;

    /** Number of initialized pcie_btl modules */
    uint32_t pcie_num_btls;

    /** list of pcie proc structures, created during add_procs */
    opal_list_t pcie_procs;

    /** lock for accessing component state */
    opal_mutex_t pcie_lock;
}; 
typedef struct mca_btl_pcie_component_t mca_btl_pcie_component_t;

OMPI_MODULE_DECLSPEC extern mca_btl_pcie_component_t mca_btl_pcie_component;

/**
 * BTL Module Interface
 */
struct mca_btl_pcie_module_t {
    mca_btl_base_module_t  super;  /**< base BTL interface */

    bool active;

    mca_btl_base_recv_reg_t pcie_reg[MCA_BTL_TAG_MAX]; 

    /** name of the pcie device */
    char *lcl_dev_name;

    /** Free list of communication buffers in the SMA region */
    ompi_free_list_t pcie_sma_buf_eager;
    ompi_free_list_t pcie_sma_buf_max;
    
    /** Free list of bounce fragments, normal user memory */
    ompi_free_list_t pcie_frag_eager;
    ompi_free_list_t pcie_frag_max;
    
    /* free list of DMA fragments */
    ompi_free_list_t pcie_frag_dma;

    /* single receive fragment to handle upcalls on message reception.
       This will need to be a free list if multiple receive callbacks
       could be triggered at the same time, which will happen if the
       code goes MT hot. */
    mca_btl_pcie_frag_recv_t pcie_recv_frag;
    
    /* lock for accessing module state */
    opal_mutex_t pcie_lock;

    /* mpool for allocating the members of pcie_sma_buf* */
    struct mca_mpool_base_module_t* pcie_mpool;
    /* mpool for RDMA pinning */
    struct mca_mpool_base_module_t* rdma_mpool;
    
    /* Endpoint associated with this module (there's a one-to-one
       mapping of modules and endpoints, since a device can only
       handle one endpoint at a time */
    struct mca_btl_base_endpoint_t* endpoint;
}; 
typedef struct mca_btl_pcie_module_t mca_btl_pcie_module_t;
extern mca_btl_pcie_module_t mca_btl_pcie_module;

struct mca_btl_pcie_reg_t {
    mca_mpool_base_registration_t base;
    AXON_memory_region_handle handle;
};
typedef struct mca_btl_pcie_reg_t mca_btl_pcie_reg_t;

struct mca_btl_pcie_modex_info_t {
    char hostname[ORTE_MAX_HOSTNAME_SIZE];
    char devicename[PATH_MAX];
};
typedef struct mca_btl_pcie_modex_info_t mca_btl_pcie_modex_info_t;
#define MCA_BTL_PCIE_MODEX_INFO_HTON(h)
#define MCA_BTL_PCIE_MODEX_INFO_NTOH(h)


/**
 * Register TEMPLATE component parameters with the MCA framework
 */
extern int mca_btl_pcie_component_open(void);

/**
 * Any final cleanup before being unloaded.
 */
extern int mca_btl_pcie_component_close(void);

/**
 * TEMPLATE component initialization.
 * 
 * @param num_btl_modules (OUT)           Number of BTLs returned in BTL array.
 * @param allow_multi_user_threads (OUT)  Flag indicating wether BTL supports user threads (TRUE)
 * @param have_hidden_threads (OUT)       Flag indicating wether BTL uses threads (TRUE)
 */
extern mca_btl_base_module_t** mca_btl_pcie_component_init(
    int *num_btl_modules, 
    bool allow_multi_user_threads,
    bool have_hidden_threads
);


/**
 * TEMPLATE component progress.
 */
extern int mca_btl_pcie_component_progress(void);



/**
 * Cleanup any resources held by the BTL.
 * 
 * @param btl  BTL instance.
 * @return     OMPI_SUCCESS or error status on failure.
 */

extern int mca_btl_pcie_finalize(
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

extern int mca_btl_pcie_add_procs(
    struct mca_btl_base_module_t* btl,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_btl_base_endpoint_t** peers,
    opal_bitmap_t* reachable
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

extern int mca_btl_pcie_del_procs(
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

extern int mca_btl_pcie_send(
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
                                                                                                    
extern int mca_btl_pcie_put(
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
                                                                                                    
extern int mca_btl_pcie_get(
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

extern int mca_btl_pcie_register(
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

extern mca_btl_base_descriptor_t* mca_btl_pcie_alloc(
     struct mca_btl_base_module_t* btl,
     struct mca_btl_base_endpoint_t* endpoint,
     uint8_t order,
     size_t size,
     uint32_t flags);


/**
 * Return a segment allocated by this BTL.
 *
 * @param btl (IN)      BTL module
 * @param descriptor (IN)  Allocated descriptor.
 */

extern int mca_btl_pcie_free(
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

mca_btl_base_descriptor_t* mca_btl_pcie_prepare_src(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_mpool_base_registration_t* registration,
    struct opal_convertor_t* convertor,
    uint8_t order,
    size_t reserve,
    size_t* size,
    uint32_t flags
);

extern mca_btl_base_descriptor_t* mca_btl_pcie_prepare_dst( 
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_mpool_base_registration_t* registration,
    struct opal_convertor_t* convertor,
    uint8_t order,
    size_t reserve,
    size_t* size,
    uint32_t flags);
                                                           
 /**
  * Fault Tolerance Event Notification Function
  * @param state Checkpoint Stae
  * @return OMPI_SUCCESS or failure status
  */
int mca_btl_pcie_ft_event(int state);

char* ompi_btl_pcie_cfg_get_local_device(char* hostname, int core);
char* ompi_btl_pcie_cfg_get_matching_device(char* remote_hostname,
					     char* remote_device);


END_C_DECLS

#endif /* #ifndef MCA_BTL_PCIE_H */
