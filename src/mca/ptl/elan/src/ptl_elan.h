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
#ifndef MCA_PTL_ELAN_H
#define MCA_PTL_ELAN_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "class/ompi_free_list.h"
#include "event/event.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"

#include "elan.h"
#include "init.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
struct mca_ptl_elan_state_t;
struct ompi_ptl_elan_queue_ctrl_t;
extern struct mca_ptl_elan_state_t mca_ptl_elan_global_state;
extern struct ompi_ptl_elan_cmdq_space_t ptl_elan_cmdq_space;

/**
 * ELAN PTL Interface
 */
struct mca_ptl_elan_module_t {

    /**< The elan progress related interface */
    mca_ptl_base_module_t   super;          /**< base PTL interface */

    /**< The following are elan-related control structures */
    ELAN_RAIL   *ptl_elan_rail; /**< Pointer to this Rail */
    ELAN_CTX    *ptl_elan_ctx;  /**< Elan ctx of this rail */

    int         ptl_ni_local;   /**< PTL NI local rank */
    int         ptl_ni_total;   /**< PTL NI total */

    /* common elan structures, each ptl keeps a copy */

    unsigned int elan_vp;      /**< elan vpid, not ompi vpid */
    unsigned int elan_nvp;     /**< total # of elan vpid */
    ompi_list_t  send_frags;   /**< outstanding send/put/get */
    ompi_list_t  recv_frags;   /**< outstanding recv's */
    ompi_list_t  pending_acks;

    struct ompi_ptl_elan_comp_queue_t  *comp;  /**< completion queue */
    struct ompi_ptl_elan_queue_ctrl_t  *queue; /**< Queue ctrl struct*/
    struct ompi_ptl_elan_putget_ctrl_t *putget;/**< putget ctrl struct */
};
typedef struct mca_ptl_elan_module_t mca_ptl_elan_module_t;
extern mca_ptl_elan_module_t mca_ptl_elan_module;

/**
 * ELAN PTL module.
 */
struct mca_ptl_elan_component_t {
    mca_ptl_base_component_t super; /**< base PTL component */
    size_t          num_modules;    /**< number of ptls activated */
    size_t          free_list_num;    /**< min number of list items */
    size_t          free_list_max;    /**< max number of list items*/
    size_t          free_list_inc;    /**< inc for each grow */

    /* We create our own simplified structure for managing elan state
     * although libelan already provides one. We do not need
     * all those tport, group, atomic, shmem and NIC threads support.
     */
    struct mca_ptl_elan_state_t    *elan_ctrl; 
    struct mca_ptl_elan_proc_t     *elan_local; 
    struct mca_ptl_elan_module_t  **modules; /**< available PTL modules */
    struct ompi_ptl_elan_thread_t **recv_threads; /**< send-related threads */
    struct ompi_ptl_elan_thread_t **send_threads; /**< recv-related threads*/

    ompi_mutex_t elan_lock;                  /**< lock for module state */
    ompi_list_t  elan_procs;                 /**< elan proc's */
    ompi_free_list_t elan_recv_frags_free;
};
typedef struct mca_ptl_elan_component_t mca_ptl_elan_component_t;

struct mca_ptl_elan_send_frag_t;
struct mca_ptl_elan_recv_frag_t;
extern mca_ptl_elan_component_t mca_ptl_elan_component;

/**
 * Register ELAN module parameters with the MCA framework
 */
extern int  mca_ptl_elan_component_open (void);

/**
 * Any final cleanup before being unloaded.
 */
extern int  mca_ptl_elan_component_close (void);

/**
 * ELAN module initialization.
 * 
 * @param num_ptl_modules (OUT)
 *        Number of PTLs returned in PTL array.
 * @param allow_multi_user_threads (OUT)  
 *        Flag indicating wether PTL supports user threads (TRUE)
 * @param have_hidden_threads (OUT)       
 *        Flag indicating whether PTL uses threads (TRUE)
 *
 */
extern mca_ptl_base_module_t **
mca_ptl_elan_component_init (int *num_ptl_modules,
                          bool * multi_user_threads,
                          bool * have_hidden_threads);

/**
 * ELAN module control.
 */
extern int  mca_ptl_elan_component_control (int param,
                                         void *value,
                                         size_t size);

/**
 * ELAN module progress.
 */
extern int  mca_ptl_elan_component_progress (mca_ptl_tstamp_t tstamp);

/**
 * Cleanup any resources held by the PTL.
 * 
 * @param ptl  PTL instance.
 * @return     OMPI_SUCCESS or error status on failure.
 */
extern int  mca_ptl_elan_finalize (struct mca_ptl_base_module_t *ptl);


/**
 * PML->PTL notification of change in the process list.
 * 
 * @param ptl  (IN)
 * @param proc (IN)  
 * @param peer (OUT)
 * @return     OMPI_SUCCESS or error status on failure.
 */
extern int  
mca_ptl_elan_add_procs (struct mca_ptl_base_module_t *ptl,
		       size_t nprocs,
		       struct ompi_proc_t **procs,
		       struct mca_ptl_base_peer_t **peers,
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
mca_ptl_elan_del_procs (struct mca_ptl_base_module_t *ptl, 
		       size_t nprocs,
		       struct ompi_proc_t ** procs, 
		       struct mca_ptl_base_peer_t **peers);

/**
 * PML->PTL acquire and initialize a send desc 
 *
 * @param ptl (IN)       PTL instance
 * @param request (OUT)  Pointer to allocated request.
 * @return               Status indicating if allocation was successful.
 */
extern int
mca_ptl_elan_req_init (struct mca_ptl_base_module_t *ptl, 
		       struct mca_pml_base_send_request_t *req);

/**
 * PML->PTL free the cached desc 
 *
 * @param ptl (IN)       PTL instance
 * @param request (IN)   Pointer to allocated request.
 */
extern void mca_ptl_elan_req_fini (struct mca_ptl_base_module_t *ptl,
                                   struct mca_pml_base_send_request_t *);

/**
 * PML->PTL Notification that a receive fragment has been matched.
 *
 * @param ptl (IN)          PTL instance
 * @param recv_frag (IN)    Receive fragment
 */
extern void mca_ptl_elan_matched (struct mca_ptl_base_module_t *ptl,
                                  struct mca_ptl_base_recv_frag_t *frag);

/**
 * PML->PTL Initiate an isend operation 
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
mca_ptl_elan_isend (struct mca_ptl_base_module_t* ptl, 
		    struct mca_ptl_base_peer_t* ptl_base_peer, 
		    struct mca_pml_base_send_request_t* request,
		    size_t offset,
		    size_t size,
		    int flags);

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
mca_ptl_elan_put (struct mca_ptl_base_module_t* ptl, 
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
mca_ptl_elan_get (struct mca_ptl_base_module_t* ptl, 
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
extern void mca_ptl_elan_recv_frag_return (struct mca_ptl_base_module_t *ptl,
                                           struct mca_ptl_elan_recv_frag_t
                                           *frag);

/**
 * Return a send fragment to the modules free list.
 *
 * @param ptl (IN)   PTL instance
 * @param frag (IN)  ELAN send fragment
 */
extern void mca_ptl_elan_send_frag_return (struct mca_ptl_base_module_t *ptl,
                                           struct mca_ptl_elan_send_frag_t
                                           *frag);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
