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
#ifndef MCA_BMI_SELF_H
#define MCA_BMI_SELF_H

#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "class/ompi_free_list.h"
#include "class/ompi_bitmap.h"
#include "class/ompi_fifo.h"
#include "event/event.h"
#include "mca/pml/pml.h"
#include "mca/bmi/bmi.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


/**
 * Shared Memory (SELF) BMI module.
 */
struct mca_bmi_self_component_t {
    mca_bmi_base_component_1_0_0_t super;  /**< base BMI component */
    int free_list_num;                     /**< initial size of free lists */
    int free_list_max;                     /**< maximum size of free lists */
    int free_list_inc;                     /**< number of elements to alloc when growing free lists */
    ompi_mutex_t self_lock;
    ompi_free_list_t self_frags_eager;     /**< free list of self first */
    ompi_free_list_t self_frags_send;      /**< free list of self second */
    ompi_free_list_t self_frags_rdma;      /**< free list of self second */
    mca_bmi_base_registration_t self_reg[256];
};
typedef struct mca_bmi_self_component_t mca_bmi_self_component_t;
extern mca_bmi_self_component_t mca_bmi_self_component;

/**
 * Register shared memory module parameters with the MCA framework
 */
extern int mca_bmi_self_component_open(void);

/**
 * Any final cleanup before being unloaded.
 */
extern int mca_bmi_self_component_close(void);

/**
 * SELF module initialization.
 * 
 * @param num_bmis (OUT)                  Number of BMIs returned in BMI array.
 * @param enable_progress_threads (IN)    Flag indicating whether BMI is allowed to have progress threads
 * @param enable_mpi_threads (IN)         Flag indicating whether BMI must support multilple simultaneous invocations from different threads
 *
 */
extern mca_bmi_base_module_t** mca_bmi_self_component_init(
    int *num_bmis, 
    bool enable_progress_threads,
    bool enable_mpi_threads
);

extern mca_bmi_base_module_t mca_bmi_self;


/**
 * Cleanup any resources held by the BMI.
 * 
 * @param bmi  BMI instance.
 * @return     OMPI_SUCCESS or error status on failure.
 */

extern int mca_bmi_self_finalize(
    struct mca_bmi_base_module_t* bmi
);


/**
 * PML->BMI notification of change in the process list.
 * PML->BMI Notification that a receive fragment has been matched.
 * Called for message that is send from process with the virtual
 * address of the shared memory segment being different than that of
 * the receiver.
 * 
 * @param bmi (IN)
 * @param proc (IN)  
 * @param peer (OUT)
 * @return     OMPI_SUCCESS or error status on failure.
 * 
 */

extern int mca_bmi_self_add_procs(
    struct mca_bmi_base_module_t* bmi,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_bmi_base_endpoint_t** peers,
    struct ompi_bitmap_t* reachability
);

                                                                                                               
/**
 * PML->BMI notification of change in the process list.
 * PML->BMI Notification that a receive fragment has been matched.
 * Called for message that is send from process with the virtual
 * address of the shared memory segment being the same as that of
 * the receiver.
 * 
 * @param bmi (IN)
 * @param proc (IN)  
 * @param peer (OUT)
 * @return     OMPI_SUCCESS or error status on failure.
 * 
 */

extern int mca_bmi_self_add_procs_same_base_addr(
    struct mca_bmi_base_module_t* bmi,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_bmi_base_endpoint_t** peers,
    ompi_bitmap_t* reachability
);

                                                                                                               
/**
 * PML->BMI notification of change in the process list.
 *
 * @param bmi (IN)     BMI instance
 * @param proc (IN)    Peer process
 * @param peer (IN)    Peer addressing information.
 * @return             Status indicating if cleanup was successful
 *
 */
extern int mca_bmi_self_del_procs(
    struct mca_bmi_base_module_t* bmi,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_bmi_base_endpoint_t **peers
);


/**
 * Register a callback function that is called on receipt
 * of a fragment.
 *
 * @param bmi (IN)     BMI module
 * @return             Status indicating if cleanup was successful
 *
 * When the process list changes, the PML notifies the BMI of the
 * change, to provide the opportunity to cleanup or release any
 * resources associated with the peer.
 */

extern int mca_bmi_self_register(
    struct mca_bmi_base_module_t* bmi,
    mca_bmi_base_tag_t tag,
    mca_bmi_base_module_recv_cb_fn_t cbfunc,
    void* cbdata
);
                                                                                                                   

/**
 * Allocate a segment.
 *
 * @param bmi (IN)      BMI module
 * @param size (IN)     Request segment size.
 */
extern mca_bmi_base_descriptor_t* mca_bmi_self_alloc(
    struct mca_bmi_base_module_t* bmi,
    size_t size
);

/**
 * Return a segment allocated by this BMI.
 *
 * @param bmi (IN)      BMI module
 * @param segment (IN)  Allocated segment.
 */
extern int mca_bmi_self_free(
    struct mca_bmi_base_module_t* bmi,
    mca_bmi_base_descriptor_t* segment
);
                                                                                                                   
/**
 * Pack data
 *
 * @param bmi (IN)      BMI module
 * @param peer (IN)     BMI peer addressing
 */
struct mca_bmi_base_descriptor_t* mca_bmi_self_prepare_src(
    struct mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* endpoint,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size
);
                                                                                                            
/**
 * Prepare data for RDMA
 *
 * @param bmi (IN)      BMI module
 * @param peer (IN)     BMI peer addressing
 */
struct mca_bmi_base_descriptor_t* mca_bmi_self_prepare_dst(
    struct mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* endpoint,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size
);
                                                                                                            
/**
 * Initiate a send to the peer.
 *
 * @param bmi (IN)      BMI module
 * @param peer (IN)     BMI peer addressing
 */
extern int mca_bmi_self_send(
    struct mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* endpoint,
    struct mca_bmi_base_descriptor_t* descriptor,
    mca_bmi_base_tag_t tag
);

/**
 * Initiate a put to the peer.
 *
 * @param bmi (IN)      BMI module
 * @param peer (IN)     BMI peer addressing
 */
                                                                                                            
extern int mca_bmi_self_rdma(
    struct mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* endpoint,
    struct mca_bmi_base_descriptor_t* descriptor
);
                                                                                                            


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

