/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_ELAN_H
#define MCA_PTL_ELAN_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "class/ompi_free_list.h"
#include "event/event.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"

#define MCA_PTL_ELAN_STATISTICS 0

#include "elan.h"
#include "init.h"

struct mca_ptl_elan_state_t;

extern struct mca_ptl_elan_state_t mca_ptl_elan_global_state;

/**
 * ELAN PTL Interface
 */
struct mca_ptl_elan_t {

    /**< The elan progress related interface */
    mca_ptl_t   super;          /**< base PTL interface */

    /**< The following are elan-related control structures */
    ELAN_RAIL   *ptl_elan_rail;     /**< Pointer to this Rail */
    ELAN_CTX    *ptl_elan_ctx;      /**< Elan ctx of this rail */

    int         ptl_ni_local;   /**< PTL NI local rank */
    int         ptl_ni_total;   /**< PTL NI total */

    int         elan_sten_size; /**< sten packet len */
    int         elan_rdma_size; /**< qdma packet length */
    int         elan_qdma_size; /**< qdma packet length */

    int         sten_total;     /**< total sten descriptors */
    int         rdma_total;     /**< total rdma descriptors */
    int         qdma_total;     /**< total rdma descriptors */
                                
    int         sten_num;       /**< num of outstanding sten packets */
    int         rdma_num;       /**< num of outstanding rdma packets */
    int         qdma_num;       /**< num of outstanding rdma packets */
                                
    int         max_num_dmas;   /**< total rdma descriptors */
                                
    ompi_list_t elan_stens;     /**< used elan sten descriptors*/
    ompi_list_t elan_dmas;      /**< used elan dma descriptors*/
    ompi_list_t elan_rdmas;     /**< used elan rdma descriptors */
    ompi_list_t elan_frags;     /**< used elan fragments */

    ompi_free_list_t elan_dmas_free;   /**< free elan dma descriptors*/
    ompi_free_list_t elan_stens_free;  /**< free elan sten descriptors*/
    ompi_free_list_t elan_rdmas_free;  /**< free elan rdma descriptors */
    ompi_free_list_t elan_frags_free;  /**< free elan rdma fragments */

#if MCA_PTL_ELAN_STATISTICS        /* some statistics */
    size_t      ptl_bytes_sent;
    size_t      ptl_bytes_recv;
#endif

};
typedef struct mca_ptl_elan_t mca_ptl_elan_t;
extern mca_ptl_elan_t mca_ptl_elan;

/**
 * ELAN PTL module.
 */
struct mca_ptl_elan_module_1_0_0_t {

    mca_ptl_base_module_1_0_0_t super;       /**< base PTL module */

    int   elan_free_list_num;     /**< initial size of free lists */
    int   elan_free_list_max;     /**< maximum size of free lists */
    int   elan_free_list_inc;     /**< # to alloc when growing lists */

    /* 
     * We create our own simplified structure for managing elan state
     * although libelan already provides one. We do not need
     * all that tport, group structures.
     */
    struct mca_ptl_elan_t **elan_ptls;  /**< array of available PTLs */
    size_t elan_num_ptls;               /**< number of ptls activated */

    ompi_list_t elan_reqs;        /**< all elan requests */
    ompi_list_t elan_prog_events; /**< events in progress */
    ompi_list_t elan_comp_events; /**< events completed, but to reclaim */
    ompi_list_t  elan_procs;       /**< elan proc's */
    ompi_list_t  elan_pending_acks;  

    ompi_free_list_t elan_events_free;/**< free events */
    ompi_free_list_t elan_reqs_free;  /**< all elan requests */

    ompi_event_t elan_send_event;  /**< event structure for sends */
    ompi_event_t elan_recv_event;  /**< event structure for recvs */

    struct mca_ptl_elan_proc_t *elan_local; 

    ompi_mutex_t elan_lock;        /**< lock for module state */
};
typedef struct mca_ptl_elan_module_1_0_0_t mca_ptl_elan_module_1_0_0_t;

struct mca_ptl_elan_send_frag_t;
struct mca_ptl_elan_recv_frag_t;

extern mca_ptl_elan_module_1_0_0_t mca_ptl_elan_module;

/**
 * Register ELAN module parameters with the MCA framework
 */
extern int  mca_ptl_elan_module_open (void);

/**
 * Any final cleanup before being unloaded.
 */
extern int  mca_ptl_elan_module_close (void);

/**
 * ELAN module initialization.
 * 
 * @param num_ptls (OUT)
 *        Number of PTLs returned in PTL array.
 * @param allow_multi_user_threads (OUT)  
 *        Flag indicating wether PTL supports user threads (TRUE)
 * @param have_hidden_threads (OUT)       
 *        Flag indicating whether PTL uses threads (TRUE)
 *
 */
extern mca_ptl_t **mca_ptl_elan_module_init (int *num_ptls,
                                             bool * multi_user_threads,
                                             bool * have_hidden_threads);

/**
 * ELAN module control.
 */
extern int  mca_ptl_elan_module_control (int param,
                                         void *value,
                                         size_t size);

/**
 * ELAN module progress.
 */
extern int  mca_ptl_elan_module_progress (mca_ptl_tstamp_t tstamp);

/**
 * Cleanup any resources held by the PTL.
 * 
 * @param ptl  PTL instance.
 * @return     OMPI_SUCCESS or error status on failure.
 */
extern int  mca_ptl_elan_finalize (struct mca_ptl_t *ptl);


/**
 * PML->PTL notification of change in the process list.
 * 
 * @param ptl  (IN)
 * @param proc (IN)  
 * @param peer (OUT)
 * @return     OMPI_SUCCESS or error status on failure.
 */
extern int  
mca_ptl_elan_add_proc (struct mca_ptl_t *ptl,
		       size_t nprocs,
		       struct ompi_proc_t **ompi_proc,
		       struct mca_ptl_base_peer_t **peer_ret,
		       ompi_bitmap_t* reachable);

/**
 * PML->PTL notification of change in the process list.
 *
 * @param ptl  (IN)     PTL instance
 * @param proc (IN)    Peer process
 * @param peer (IN)    Peer addressing information.
 * @return             Status indicating if cleanup was successful
 */
extern int  
mca_ptl_elan_del_proc (struct mca_ptl_t *ptl, 
		       size_t nprocs,
		       struct ompi_proc_t ** procs, 
		       struct mca_ptl_base_peer_t **ptl_peer);

/**
 * PML->PTL Allocate a send request from the PTL modules free list.
 *
 * @param ptl (IN)       PTL instance
 * @param request (OUT)  Pointer to allocated request.
 * @return               Status indicating if allocation was successful.
 */
extern int  
mca_ptl_elan_req_alloc (struct mca_ptl_t *ptl, 
			struct mca_pml_base_send_request_t **);

/**
 * PML->PTL Return a send request to the PTL modules free list.
 *
 * @param ptl (IN)       PTL instance
 * @param request (IN)   Pointer to allocated request.
 */
extern void mca_ptl_elan_req_return (struct mca_ptl_t *ptl,
                                     struct mca_pml_base_send_request_t *);

/**
 * PML->PTL Notification that a receive fragment has been matched.
 *
 * @param ptl (IN)          PTL instance
 * @param recv_frag (IN)    Receive fragment
 */
extern void mca_ptl_elan_matched (struct mca_ptl_t *ptl,
                                  struct mca_ptl_base_recv_frag_t *frag);

/**
 * PML->PTL Initiate a put of the specified size.
 *
 * @param ptl (IN)               PTL instance
 * @param ptl_base_peer (IN)     PTL peer addressing
 * @param send_request (IN/OUT)  Send request (allocated by PML via 
 *        mca_ptl_base_request_alloc_fn_t)
 * @param size (IN)              
 *        Number of bytes PML is requesting PTL to deliver
 * @param flags (IN)             
 *        Flags that should be passed to the peer via the message header.
 * @param request (OUT)          
 *        OMPI_SUCCESS if the PTL was able to queue one or more fragments
 */
extern int  
mca_ptl_elan_put (struct mca_ptl_t* ptl, 
		  struct mca_ptl_base_peer_t* ptl_base_peer, 
		  struct mca_pml_base_send_request_t* request,
		  size_t offset,
		  size_t size,
		  int flags);

/**
 * PML->PTL Initiate a get of the specified size.
 *
 * @param ptl (IN)               PTL instance
 * @param ptl_base_peer (IN)     PTL peer addressing
 * @param send_request (IN/OUT)  Send request (allocated by PML via 
 *        mca_ptl_base_request_alloc_fn_t)
 * @param size (IN)              
 *        Number of bytes PML is requesting PTL to deliver
 * @param flags (IN)             
 *        Flags that should be passed to the peer via the message header.
 * @param request (OUT)          
 *        OMPI_SUCCESS if the PTL was able to queue one or more fragments
 */
extern int  
mca_ptl_elan_get (struct mca_ptl_t* ptl, 
		  struct mca_ptl_base_peer_t* ptl_base_peer, 
		  struct mca_pml_base_recv_request_t* request,
		  size_t offset,
		  size_t size,
		  int flags);

/**
 * Return a recv fragment to the modules free list.
 *
 * @param ptl (IN)   PTL instance
 * @param frag (IN)  ELAN receive fragment
 */
extern void mca_ptl_elan_recv_frag_return (struct mca_ptl_t *ptl,
                                           struct mca_ptl_elan_recv_frag_t
                                           *frag);

/**
 * Return a send fragment to the modules free list.
 *
 * @param ptl (IN)   PTL instance
 * @param frag (IN)  ELAN send fragment
 */
extern void mca_ptl_elan_send_frag_return (struct mca_ptl_t *ptl,
                                           struct mca_ptl_elan_send_frag_t
                                           *frag);

#endif
