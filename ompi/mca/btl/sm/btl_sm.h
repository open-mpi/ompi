
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
#ifndef MCA_BTL_SM_H
#define MCA_BTL_SM_H

#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "class/ompi_free_list.h"
#include "class/ompi_bitmap.h"
#include "class/ompi_fifo.h"
#include "event/event.h"
#include "mca/pml/pml.h"
#include "mca/btl/btl.h"
#include "mca/btl/base/base.h" 

#include "mca/mpool/mpool.h"
#include "mca/common/sm/common_sm_mmap.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


/*
 * Shared Memory resource managment
 */
struct mca_btl_sm_module_resource_t {
    /* base control structures */
    mca_common_sm_file_header_t segment_header;

    /* fifo queues - offsets relative to the base of the share memory
     * segment will be stored here */
    volatile ompi_fifo_t **fifo;
};
typedef struct mca_btl_sm_module_resource_t mca_btl_sm_module_resource_t;
extern mca_btl_sm_module_resource_t mca_btl_sm_module_resource;

#define SM_CONNECTED 1
#define SM_CONNECTED_SAME_BASE_ADDR  2
#define SM_CONNECTED_DIFFERENT_BASE_ADDR  3

#if OMPI_ENABLE_PROGRESS_THREADS == 1
#define DATA (char)0
#define DONE (char)1
#endif

typedef mca_btl_base_recv_reg_t mca_btl_sm_recv_reg_t;


/**
 * Shared Memory (SM) BTL module.
 */
struct mca_btl_sm_component_t {
    mca_btl_base_component_1_0_0_t super;  /**< base BTL component */
    int sm_free_list_num;              /**< initial size of free lists */
    int sm_free_list_max;              /**< maximum size of free lists */
    int sm_free_list_inc;              /**< number of elements to alloc when growing free lists */
    int sm_max_procs;                  /**< upper limit on the number of processes using the shared memory pool */
    int sm_extra_procs;                /**< number of extra procs to allow */
    char* sm_mpool_name;               /**< name of shared memory pool module */
    mca_mpool_base_module_t* sm_mpool; /**< shared memory pool */
    void* sm_mpool_base;               /**< base address of shared memory pool */
    size_t eager_limit;                /**< first fragment size */
    size_t max_frag_size;              /**< maximum (second and beyone) fragment size */
    ompi_mutex_t sm_lock;
    char* sm_resouce_ctl_file;         /**< name of shared memory file used 
                                            to coordinate resource usage */
    mca_common_sm_mmap_t *mmap_file;   /**< description of mmap'ed file */
    mca_btl_sm_module_resource_t *sm_ctl_header;  /* control header in
                                                     shared memory */
    ompi_fifo_t **fifo;    /**< cached copy of the pointer to the 2D
                             fifo array.  The address in the shared
                             memory segment sm_ctl_header is a relative,
                             but this one, in process private memory, is
                             a real virtual address */
    size_t size_of_cb_queue; /**< size of each circular buffer queue array */
    size_t cb_lazy_free_freq; /**< frequency of lazy free */
    ssize_t *sm_offset;        /**< offset to be applied to shared memory
                              addresses, per local process value */
    int *sm_proc_connect;    /* scratch array used by the 0'th btl to
                              * set indicate sm connectivty.  Used by
                              * the 1'st btl */
    size_t num_smp_procs;      /**< current number of smp procs on this
                              host */
    int num_smp_procs_same_base_addr;  /* number of procs with same
                                          base shared memory virtual
                                          address as this process */
    int num_smp_procs_different_base_addr;  /* number of procs with
                                               different base shared memory
                                               virtual address as this
                                               process */
    int *list_smp_procs_same_base_addr;  /* number of procs with same 
                                            base shared memory virtual 
                                            address as this process */
    int *list_smp_procs_different_base_addr;  /* number of procs with different 
                                            base shared memory virtual 
                                            address as this process */
    uint32_t my_smp_rank;    /**< My SMP process rank.  Used for accessing
			      *   SMP specfic data structures. */
    ompi_free_list_t sm_frags1;    /**< free list of sm first */
    ompi_free_list_t sm_frags2;    /**< free list of sm second */
    ompi_free_list_t sm_first_frags_to_progress;  /**< list of first
                                                    fragments that are
                                                    awaiting resources */
    struct mca_btl_base_endpoint_t **sm_peers;

#if OMPI_ENABLE_PROGRESS_THREADS == 1
    char sm_fifo_path[PATH_MAX];   /**< path to fifo used to signal this process */
    int  sm_fifo_fd;               /**< file descriptor corresponding to opened fifo */
    ompi_thread_t sm_fifo_thread;
#endif
};
typedef struct mca_btl_sm_component_t mca_btl_sm_component_t;
extern mca_btl_sm_component_t mca_btl_sm_component;

/**
 * Register shared memory module parameters with the MCA framework
 */
extern int mca_btl_sm_component_open(void);

/**
 * Any final cleanup before being unloaded.
 */
extern int mca_btl_sm_component_close(void);

/**
 * SM module initialization.
 * 
 * @param num_btls (OUT)                  Number of BTLs returned in BTL array.
 * @param enable_progress_threads (IN)    Flag indicating whether BTL is allowed to have progress threads
 * @param enable_mpi_threads (IN)         Flag indicating whether BTL must support multilple simultaneous invocations from different threads
 *
 */
extern mca_btl_base_module_t** mca_btl_sm_component_init(
    int *num_btls, 
    bool enable_progress_threads,
    bool enable_mpi_threads
);

/**
 * shared memory component progress.
 */
extern int mca_btl_sm_component_progress(void);

/**
 * SM BTL Interface
 */
struct mca_btl_sm_t {
    mca_btl_base_module_t  super;       /**< base BTL interface */
    bool btl_inited;  /**< flag indicating if btl has been inited */
    mca_btl_sm_recv_reg_t sm_reg[256];
};
typedef struct mca_btl_sm_t mca_btl_sm_t;

extern mca_btl_sm_t mca_btl_sm[2];


/**
 * Cleanup any resources held by the BTL.
 * 
 * @param btl  BTL instance.
 * @return     OMPI_SUCCESS or error status on failure.
 */

extern int mca_btl_sm_finalize(
    struct mca_btl_base_module_t* btl
);


/**
 * PML->BTL notification of change in the process list.
 * PML->BTL Notification that a receive fragment has been matched.
 * Called for message that is send from process with the virtual
 * address of the shared memory segment being different than that of
 * the receiver.
 * 
 * @param btl (IN)
 * @param proc (IN)  
 * @param peer (OUT)
 * @return     OMPI_SUCCESS or error status on failure.
 * 
 */

extern int mca_btl_sm_add_procs(
    struct mca_btl_base_module_t* btl,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_btl_base_endpoint_t** peers,
    struct ompi_bitmap_t* reachability
);

                                                                                                               
/**
 * PML->BTL notification of change in the process list.
 * PML->BTL Notification that a receive fragment has been matched.
 * Called for message that is send from process with the virtual
 * address of the shared memory segment being the same as that of
 * the receiver.
 * 
 * @param btl (IN)
 * @param proc (IN)  
 * @param peer (OUT)
 * @return     OMPI_SUCCESS or error status on failure.
 * 
 */

extern int mca_btl_sm_add_procs_same_base_addr(
    struct mca_btl_base_module_t* btl,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_btl_base_endpoint_t** peers,
    ompi_bitmap_t* reachability
);

                                                                                                               
/**
 * PML->BTL notification of change in the process list.
 *
 * @param btl (IN)     BTL instance
 * @param proc (IN)    Peer process
 * @param peer (IN)    Peer addressing information.
 * @return             Status indicating if cleanup was successful
 *
 */
extern int mca_btl_sm_del_procs(
    struct mca_btl_base_module_t* btl,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_btl_base_endpoint_t **peers
);


/**
 * Register a callback function that is called on receipt
 * of a fragment.
 *
 * @param btl (IN)     BTL module
 * @return             Status indicating if cleanup was successful
 *
 * When the process list changes, the PML notifies the BTL of the
 * change, to provide the opportunity to cleanup or release any
 * resources associated with the peer.
 */

extern int mca_btl_sm_register(
    struct mca_btl_base_module_t* btl,
    mca_btl_base_tag_t tag,
    mca_btl_base_module_recv_cb_fn_t cbfunc,
    void* cbdata
);
                                                                                                                   

/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */
extern mca_btl_base_descriptor_t* mca_btl_sm_alloc(
    struct mca_btl_base_module_t* btl,
    size_t size
);

/**
 * Return a segment allocated by this BTL.
 *
 * @param btl (IN)      BTL module
 * @param segment (IN)  Allocated segment.
 */
extern int mca_btl_sm_free(
    struct mca_btl_base_module_t* btl,
    mca_btl_base_descriptor_t* segment
);
                                                                                                                   
/**
 * Pack data
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
struct mca_btl_base_descriptor_t* mca_btl_sm_prepare_src(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    mca_mpool_base_registration_t* registration,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size
);
                                                                                                            
/**
 * Initiate a send to the peer.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
extern int mca_btl_sm_send(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor,
    mca_btl_base_tag_t tag
);

/**
 * Data structure used to hold information that will be exchanged with
 * all other procs at startup.  !!!!! This is only temporary, until the
 * registry is complete
 */
#define MCA_BTL_SM_MAX_HOSTNAME_LEN  128
typedef struct mca_btl_sm_exchange{
    char host_name[MCA_BTL_SM_MAX_HOSTNAME_LEN];
}mca_btl_sm_exchange_t;

#if OMPI_ENABLE_PROGRESS_THREADS == 1
void mca_btl_sm_component_event_thread(opal_object_t*);
#endif
                                                                                                              
#if OMPI_ENABLE_PROGRESS_THREADS == 1 
#define MCA_BTL_SM_SIGNAL_PEER(peer) \
{ \
    unsigned char cmd = DATA; \
    if(write(peer->fifo_fd, &cmd, sizeof(cmd)) != sizeof(cmd)) { \
        ompi_output(0, "mca_btl_sm_send: write fifo failed: errno=%d\n", errno); \
    } \
}
#else
#define MCA_BTL_SM_SIGNAL_PEER(peer)
#endif


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

