/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
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
#include "class/ompi_free_list.h"
#include "class/ompi_bitmap.h"
#include "class/ompi_fifo.h"
#include "event/event.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/mpool/mpool.h"
#include "mca/common/sm/common_sm_mmap.h"
#include "mca/ptl/sm/src/ptl_sm_peer.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Shared Memory resource managment
 */
struct mca_ptl_sm_module_resource_t {
    /* base control structures */
    mca_common_sm_file_header_t segment_header;

    /* fifo queues - offsets relative to the base of the share memory
     * segment will be stored here */
    volatile ompi_fifo_t **fifo;
};
typedef struct mca_ptl_sm_module_resource_t mca_ptl_sm_module_resource_t;
extern mca_ptl_sm_module_resource_t mca_ptl_sm_module_resource;

#define SM_CONNECTED 1
#define SM_CONNECTED_SAME_BASE_ADDR  2
#define SM_CONNECTED_DIFFERENT_BASE_ADDR  3

#if OMPI_HAVE_THREADS == 1
#define DATA (char)0
#define DONE (char)1
#endif
/**
 * Shared Memory (SM) PTL module.
 */
struct mca_ptl_sm_component_t {
    mca_ptl_base_component_1_0_0_t super;  /**< base PTL component */
    int sm_first_frag_free_list_num;       /**< initial size of free lists */
    int sm_first_frag_free_list_max;       /**< maximum size of free lists */
    int sm_first_frag_free_list_inc;       /**< number of elements to alloc when growing free lists */
    int sm_second_frag_free_list_num;       /**< initial size of free lists */
    int sm_second_frag_free_list_max;       /**< maximum size of free lists */
    int sm_second_frag_free_list_inc;       /**< number of elements to alloc when growing free lists */
    int sm_max_procs;                      /**< upper limit on the number of processes using the shared memory pool */
    int sm_extra_procs;                  /**< number of extra procs to allow */
    char* sm_mpool_name;                  /**< name of shared memory pool module */
    mca_mpool_base_module_t* sm_mpool; /**< shared memory pool */
    void* sm_mpool_base;                  /**< base address of shared memory pool */
    size_t first_fragment_size;            /**< first fragment size */
    size_t max_fragment_size;            /**< maximum (second and
                                             beyone) fragment size */
    size_t fragment_alignment;       /**< fragment alignment */
    ompi_mutex_t sm_lock;
    char* sm_resouce_ctl_file;     /**< name of shared memory file used 
                                            to coordinate resource usage */
    mca_common_sm_mmap_t *mmap_file;     /**< description of mmap'ed
                                           file */
    mca_ptl_sm_module_resource_t *sm_ctl_header;  /* control header in
                                                     shared memory */
    ompi_fifo_t **fifo;    /**< cached copy of the pointer to the 2D
                             fifo array.  The address in the shared
                             memory segment sm_ctl_header is a relative,
                             but this one, in process private memory, is
                             a real virtual address */
    size_t size_of_cb_queue; /**< size of each circular buffer queue array */
    size_t cb_lazy_free_freq; /**< frequency of lazy free */
    size_t *sm_offset;        /**< offset to be applied to shared memory
                              addresses, per local process value */
    int *sm_proc_connect;    /* scratch array used by the 0'th ptl to
                              * set indicate sm connectivty.  Used by
                              * the 1'st ptl */
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
    int my_smp_rank;    /**< My SMP process rank.  Used for accessing
                         *   SMP specfic data structures. */
    ompi_free_list_t sm_first_frags;    /**< free list of sm first
                                             fragments */
    ompi_free_list_t sm_second_frags;   /**< free list of sm second
                                              and above fragments */
    ompi_free_list_t sm_send_requests;    /**< free list of sm send requests -- sendreq + sendfrag */
    ompi_free_list_t sm_first_frags_to_progress;  /**< list of first
                                                    fragments that are
                                                    awaiting resources */
    ompi_mutex_t sm_pending_ack_lock;
    ompi_list_t sm_pending_ack; /**< list of fragmnent that need to be
                                   acked */

    struct mca_ptl_base_peer_t **sm_peers;
#if OMPI_HAVE_THREADS == 1
    char sm_fifo_path[PATH_MAX];   /**< path to fifo used to signal this process */
    int  sm_fifo_fd;               /**< file descriptor corresponding to opened fifo */
    ompi_thread_t sm_fifo_thread;
#endif
};
typedef struct mca_ptl_sm_component_t mca_ptl_sm_component_t;
extern mca_ptl_sm_component_t mca_ptl_sm_component;

/**
 * Register shared memory module parameters with the MCA framework
 */
extern int mca_ptl_sm_component_open(void);

/**
 * Any final cleanup before being unloaded.
 */
extern int mca_ptl_sm_component_close(void);

/**
 * SM module initialization.
 * 
 * @param num_ptls (OUT)                  Number of PTLs returned in PTL array.
 * @param allow_multi_user_threads (OUT)  Flag indicating wether PTL supports user threads (TRUE)
 * @param have_hidden_threads (OUT)       Flag indicating wether PTL uses threads (TRUE)
 *
 */
extern mca_ptl_base_module_t** mca_ptl_sm_component_init(
    int *num_ptls, 
    bool *allow_multi_user_threads,
    bool *have_hidden_threads
);

/**
 * shared memory component control.
 */
extern int mca_ptl_sm_component_control(
    int param,
    void* value,
    size_t size
);

/**
 * shared memory component progress.
 */
extern int mca_ptl_sm_component_progress(
   mca_ptl_tstamp_t tstamp
);

/**
 * SM PTL Interface
 */
struct mca_ptl_sm_t {
    mca_ptl_base_module_t  super;       /**< base PTL interface */

    bool ptl_inited;  /**< flag indicating if ptl has been inited */
};
typedef struct mca_ptl_sm_t mca_ptl_sm_t;

extern mca_ptl_sm_t mca_ptl_sm[2];


/**
 * Cleanup any resources held by the PTL.
 * 
 * @param ptl  PTL instance.
 * @return     OMPI_SUCCESS or error status on failure.
 */

extern int mca_ptl_sm_finalize(
    struct mca_ptl_base_module_t* ptl
);


/**
 * PML->PTL notification of change in the process list.
 * PML->PTL Notification that a receive fragment has been matched.
 * Called for message that is send from process with the virtual
 * address of the shared memory segment being different than that of
 * the receiver.
 * 
 * @param ptl (IN)
 * @param proc (IN)  
 * @param peer (OUT)
 * @return     OMPI_SUCCESS or error status on failure.
 * 
 */

extern int mca_ptl_sm_add_procs(
    struct mca_ptl_base_module_t* ptl,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_ptl_base_peer_t** peers,
    struct ompi_bitmap_t* reachability
);

                                                                                                               
/**
 * PML->PTL notification of change in the process list.
 * PML->PTL Notification that a receive fragment has been matched.
 * Called for message that is send from process with the virtual
 * address of the shared memory segment being the same as that of
 * the receiver.
 * 
 * @param ptl (IN)
 * @param proc (IN)  
 * @param peer (OUT)
 * @return     OMPI_SUCCESS or error status on failure.
 * 
 */

extern int mca_ptl_sm_add_procs_same_base_addr(
    struct mca_ptl_base_module_t* ptl,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_ptl_base_peer_t** peers,
    ompi_bitmap_t* reachability
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
    struct mca_ptl_base_module_t* ptl,
    size_t nprocs,
    struct ompi_proc_t **procs,
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
    struct mca_ptl_base_module_t* ptl,
    struct mca_pml_base_send_request_t*
);

/**
 * PML->PTL Return a send request to the PTL modules free list.
 *
 * @param ptl (IN)       PTL instance
 * @param request (IN)   Pointer to allocated request.
 *
 */
extern void mca_ptl_sm_request_return(
    struct mca_ptl_base_module_t* ptl,
    struct mca_pml_base_send_request_t*
);

/**
 * PML->PTL Notification that a receive fragment has been matched.
 * Called for message that is send from process with the virtual
 * address of the shared memory segment being the same as that of
 * the receiver.
 *
 * @param ptl (IN)          PTL instance
 * @param recv_frag (IN)    Receive fragment
 *
 */
extern void mca_ptl_sm_matched_same_base_addr(
    struct mca_ptl_base_module_t* ptl,
    struct mca_ptl_base_recv_frag_t* frag
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
    struct mca_ptl_base_module_t* ptl,
    size_t nprocs,
    struct ompi_proc_t **procs,
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
    struct mca_ptl_base_module_t* ptl,
    struct mca_pml_base_send_request_t*
);

/**
 * PML->PTL Return a send request to the PTL modules free list.
 *
 * @param ptl (IN)       PTL instance
 * @param request (IN)   Pointer to allocated request.
 *
 */
extern void mca_ptl_sm_request_return(
    struct mca_ptl_base_module_t* ptl,
    struct mca_pml_base_send_request_t*
);

/**
 * PML->PTL Notification that a receive fragment has been matched.
 * Called for message that is send from process with the virtual
 * address of the shared memory segment being different than that of
 * the receiver.
 *
 * @param ptl (IN)          PTL instance
 * @param recv_frag (IN)    Receive fragment
 *
 */
extern void mca_ptl_sm_matched(
    struct mca_ptl_base_module_t* ptl,
    struct mca_ptl_base_recv_frag_t* frag
);

/**
 * PML->PTL Notification that a receive fragment has been matched.
 * Called for message that is send from process with the virtual
 * address of the shared memory segment being the same as that of
 * the receiver.
 *
 * @param ptl (IN)          PTL instance
 * @param recv_frag (IN)    Receive fragment
 *
 */
extern void mca_ptl_sm_matched_common_base_addr(
    struct mca_ptl_base_module_t* ptl,
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
 * @param request (OUT)          OMPI_SUCCESS if the PTL was able to queue one or more fragments
 */
extern int mca_ptl_sm_send(
    struct mca_ptl_base_module_t* ptl,
    struct mca_ptl_base_peer_t* ptl_peer,
    struct mca_pml_base_send_request_t*,
    size_t offset,
    size_t size,
    int flags
);

/**
 * PML->PTL send second and subsequent fragments
 *
 * @param ptl (IN)               PTL instance
 * @param ptl_base_peer (IN)     PTL peer addressing
 * @param send_request (IN/OUT)  Send request (allocated by PML via mca_ptl_base_request_alloc_fn_t)
 * @param size (IN)              Number of bytes PML is requesting PTL to deliver
 * @param flags (IN)             Flags that should be passed to the peer via the message header.
 * @param request (OUT)          OMPI_SUCCESS if the PTL was able to queue one or more fragments
 */
extern int mca_ptl_sm_send_continue(
    struct mca_ptl_base_module_t* ptl,
    struct mca_ptl_base_peer_t* ptl_peer,
    struct mca_pml_base_send_request_t*,
    size_t offset,
    size_t size,
    int flags
);

/**
 * Data structure used to hold information that will be exchanged with
 * all other procs at startup.  !!!!! This is only temporary, until the
 * registry is complete
 */
#define MCA_PTL_SM_MAX_HOSTNAME_LEN  128
typedef struct mca_ptl_sm_exchange{
    char host_name[MCA_PTL_SM_MAX_HOSTNAME_LEN];
}mca_ptl_sm_exchange_t;

#if OMPI_HAVE_THREADS == 1
void mca_ptl_sm_component_event_thread(ompi_object_t*);
#endif
                                                                                                              
#if OMPI_HAVE_THREADS == 1 
#define MCA_PTL_SM_SIGNAL_PEER(peer) \
{ \
    unsigned char cmd = DATA; \
    if(write(peer->fifo_fd, &cmd, sizeof(cmd)) != sizeof(cmd)) { \
        ompi_output(0, "mca_ptl_sm_send: write fifo failed: errno=%d\n", errno); \
    } \
}
#else
#define MCA_PTL_SM_SIGNAL_PEER(peer)
#endif


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

