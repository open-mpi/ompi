/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_SM_H
#define MCA_PTL_SM_H

#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "mem/free_list.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "ptl_sm_mmap.h"


/**
 * Shared Memory (SM) PTL module.
 */
struct mca_ptl_sm_module_1_0_0_t {
    mca_ptl_base_module_1_0_0_t super;   /**< base PTL module */
    int sm_min_alloc;                    /**< min size of shared memory allocation */
    int sm_max_alloc;                    /**< max size of shared memory allocation */
    int sm_free_list_num;                /**< initial size of free lists */
    int sm_free_list_max;                /**< maximum size of free lists */
    int sm_free_list_inc;                /**< number of elements to alloc when growing free lists */
    void* sm_base_addr;                  /**< base address of mmaped region */
    lam_free_list_t sm_send_requests;    /**< free list of sm send requests -- sendreq + sendfrag */
    lam_free_list_t sm_send_frags;       /**< free list of sm send fragments */
    lam_free_list_t sm_recv_frags;       /**< free list of sm recv fragments */
    lam_allocator_t sm_allocator;        /**< shared memory allocator */
    char sm_mmap_file[PATH_MAX];         /**< full path to backing file */
    mca_ptl_sm_mmap_t *sm_mmap;
    lam_mutex_t sm_lock;
};
typedef struct mca_ptl_sm_module_1_0_0_t mca_ptl_sm_module_1_0_0_t;
typedef struct mca_ptl_sm_module_1_0_0_t mca_ptl_sm_module_t;
extern mca_ptl_sm_module_1_0_0_t mca_ptl_sm_module;

/**
 * Register shared memory module parameters with the MCA framework
 */
extern int mca_ptl_sm_module_open(void);

/**
 * Any final cleanup before being unloaded.
 */
extern int mca_ptl_sm_module_close(void);

/**
 * SM module initialization.
 * 
 * @param num_ptls (OUT)                  Number of PTLs returned in PTL array.
 * @param allow_multi_user_threads (OUT)  Flag indicating wether PTL supports user threads (TRUE)
 * @param have_hidden_threads (OUT)       Flag indicating wether PTL uses threads (TRUE)
 *
 */
extern mca_ptl_t** mca_ptl_sm_module_init(
    int *num_ptls, 
    bool *allow_multi_user_threads,
    bool *have_hidden_threads
);

/**
 * shared memory module control.
 */
extern int mca_ptl_sm_module_control(
    int param,
    void* value,
    size_t size
);

/**
 * shared memory module progress.
 */
extern int mca_ptl_sm_module_progress(
   mca_ptl_tstamp_t tstamp
);

/**
 * SM PTL Interface
 */
struct mca_ptl_sm_t {
    mca_ptl_t  super;       /**< base PTL interface */
};
typedef struct mca_ptl_sm_t mca_ptl_sm_t;

extern mca_ptl_sm_t mca_ptl_sm;


/**
 * Cleanup any resources held by the PTL.
 * 
 * @param ptl  PTL instance.
 * @return     LAM_SUCCESS or error status on failure.
 */

extern int mca_ptl_sm_finalize(
    struct mca_ptl_t* ptl
);


/**
 * PML->PTL notification of change in the process list.
 * 
 * @param ptl (IN)
 * @param proc (IN)  
 * @param peer (OUT)
 * @return     LAM_SUCCESS or error status on failure.
 * 
 */

extern int mca_ptl_sm_add_procs(
    struct mca_ptl_t* ptl,
    size_t nprocs,
    struct lam_proc_t **procs,
    struct mca_ptl_base_peer_t** peers,
    lam_bitmap_t* reachability
);

                                                                                                               
/**
 * PML->PTL notification of change in the process list.
 *
 * @param ptl (IN)     PTL instance
 * @param proc (IN)    Peer process
 * @param peer (IN)    Peer addressing information.
 * @return             Status indicating if cleanup was successful
 *
 */
extern int mca_ptl_sm_del_procs(
    struct mca_ptl_t* ptl,
    size_t nprocs,
    struct lam_proc_t **procs,
    struct mca_ptl_base_peer_t **peers
);

/**
 * PML->PTL Allocate a send request from the PTL modules free list.
 *
 * @param ptl (IN)       PTL instance
 * @param request (OUT)  Pointer to allocated request.
 * @return               Status indicating if allocation was successful.
 *
 */
extern int mca_ptl_sm_request_alloc(
    struct mca_ptl_t* ptl,
    struct mca_ptl_base_send_request_t**
);

/**
 * PML->PTL Return a send request to the PTL modules free list.
 *
 * @param ptl (IN)       PTL instance
 * @param request (IN)   Pointer to allocated request.
 *
 */
extern void mca_ptl_sm_request_return(
    struct mca_ptl_t* ptl,
    struct mca_ptl_base_send_request_t*
);

/**
 * PML->PTL Notification that a receive fragment has been matched.
 *
 * @param ptl (IN)          PTL instance
 * @param recv_frag (IN)    Receive fragment
 *
 */
extern void mca_ptl_sm_matched(
    struct mca_ptl_t* ptl,
    struct mca_ptl_base_recv_frag_t* frag
);
                                                                                                 
/**
 * PML->PTL Initiate a send of the specified size.
 *
 * @param ptl (IN)               PTL instance
 * @param ptl_base_peer (IN)     PTL peer addressing
 * @param send_request (IN/OUT)  Send request (allocated by PML via mca_ptl_base_request_alloc_fn_t)
 * @param size (IN)              Number of bytes PML is requesting PTL to deliver
 * @param flags (IN)             Flags that should be passed to the peer via the message header.
 * @param request (OUT)          LAM_SUCCESS if the PTL was able to queue one or more fragments
 */
extern int mca_ptl_sm_send(
    struct mca_ptl_t* ptl,
    struct mca_ptl_base_peer_t* ptl_peer,
    struct mca_ptl_base_send_request_t*,
    size_t offset,
    size_t *size,
    int flags
);

#endif

