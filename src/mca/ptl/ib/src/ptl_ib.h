/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_IB_H
#define MCA_PTL_IB_H

/* Standard system includes */
#include <sys/types.h>
#include <string.h>

/* Open MPI includes */
#include "class/ompi_free_list.h"
#include "event/event.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "util/output.h"

/* InfiniBand VAPI includes */
#include "ptl_ib_vapi.h"
#include "ptl_ib_addr.h"
#include "ptl_ib_proc.h"
#include "ptl_ib_peer.h"
#include "ptl_ib_priv.h"

/* Other IB ptl includes */
#include "ptl_ib_sendreq.h"
#include "ptl_ib_recvfrag.h"
#include "ptl_ib_sendfrag.h"

/**
 * IB PTL component.
 */

struct mca_ptl_ib_component_t {
    mca_ptl_base_component_1_0_0_t          super; 
    /**< base PTL component */

    struct mca_ptl_ib_module_t              **ib_ptl_modules;
    /**< array of available PTLs */

    uint32_t                                ib_num_ptl_modules;
    /**< number of ptl modules actually used */

    uint32_t                                ib_max_ptl_modules;         
    /**< maximum number of ptls */

    int                                     ib_free_list_num;
    /**< initial size of free lists */

    int                                     ib_free_list_max;
    /**< maximum size of free lists */

    int                                     ib_free_list_inc;
    /**< number of elements to alloc when growing free lists */

    ompi_free_list_t                        ib_send_requests;
    /**< free list of ib send requests -- sendreq + IB */

    ompi_free_list_t                        ib_send_frags;
    /**< free list of ib send fragments */

    ompi_free_list_t                        ib_recv_frags;
    /**< free list of ib recv fragments */

    ompi_list_t                             ib_procs;
    /**< list of ib proc structures */

    ompi_event_t                            ib_send_event;
    /**< event structure for sends */

    ompi_event_t                            ib_recv_event;
    /**< event structure for recvs */

    ompi_mutex_t                            ib_lock;
    /**< lock for accessing module state */

    uint32_t                                ib_num_hcas;
    /* number of hcas available to the IB component */
};
typedef struct mca_ptl_ib_component_t mca_ptl_ib_component_t;
struct mca_ptl_ib_recv_frag_t;

extern mca_ptl_ib_component_t mca_ptl_ib_component;

/**
 * IB PTL Interface
 */
struct mca_ptl_ib_module_t {
    mca_ptl_base_module_t                   super;  
    /**< base PTL interface */

    mca_ptl_ib_state_t                      *ib_state;
    /* IB state holds info about queue handles, HCA handles,
     * protection domain etc. which are private to this module */

    ompi_free_list_t                        send_free;
    /**< free list of send buffer descriptors */

    ompi_free_list_t                        recv_free;
    /**< free list of recv buffer descriptors */

    ompi_list_t                             pending_send_frags;
    /**< list of all pending send fragments */

    ompi_list_t                             pending_recv_frags;
    /**< list of all pending recv fragments */
};

typedef struct mca_ptl_ib_module_t mca_ptl_ib_module_t;

extern mca_ptl_ib_module_t mca_ptl_ib_module;

/**
 * Register IB component parameters with the MCA framework
 */
extern int mca_ptl_ib_component_open(void);

/**
 * Any final cleanup before being unloaded.
 */
extern int mca_ptl_ib_component_close(void);

/**
 * IB component initialization.
 * 
 * @param num_ptl_modules (OUT)                  Number of PTLs returned in PTL array.
 * @param allow_multi_user_threads (OUT)  Flag indicating wether PTL supports user threads (TRUE)
 * @param have_hidden_threads (OUT)       Flag indicating wether PTL uses threads (TRUE)
 *
 *  (1) read interface list from kernel and compare against component parameters
 *      then create a PTL instance for selected interfaces
 *  (2) setup IB listen socket for incoming connection attempts
 *  (3) publish PTL addressing info 
 *
 */
extern mca_ptl_base_module_t** mca_ptl_ib_component_init(
    int *num_ptl_modules, 
    bool *allow_multi_user_threads,
    bool *have_hidden_threads
);

/**
 * IB component control.
 */
extern int mca_ptl_ib_component_control(
    int param,
    void* value,
    size_t size
);

/**
 * IB component progress.
 */
extern int mca_ptl_ib_component_progress(
   mca_ptl_tstamp_t tstamp
);



/**
 * Cleanup any resources held by the PTL.
 * 
 * @param ptl  PTL instance.
 * @return     OMPI_SUCCESS or error status on failure.
 */

extern int mca_ptl_ib_finalize(
    struct mca_ptl_base_module_t* ptl
);


/**
 * PML->PTL notification of change in the process list.
 * 
 * @param ptl (IN)
 * @param nprocs (IN)     Number of processes
 * @param procs (IN)      Set of processes
 * @param peers (OUT)     Set of (optional) peer addressing info.
 * @param peers (IN/OUT)  Set of processes that are reachable via this PTL.
 * @return     OMPI_SUCCESS or error status on failure.
 * 
 */

extern int mca_ptl_ib_add_procs(
    struct mca_ptl_base_module_t* ptl,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_ptl_base_peer_t** peers,
    ompi_bitmap_t* reachable
);

/**
 * PML->PTL notification of change in the process list.
 *
 * @param ptl (IN)     PTL instance
 * @param nproc (IN)   Number of processes.
 * @param procs (IN)   Set of processes.
 * @param peers (IN)   Set of peer data structures.
 * @return             Status indicating if cleanup was successful
 *
 */
extern int mca_ptl_ib_del_procs(
    struct mca_ptl_base_module_t* ptl,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_ptl_base_peer_t** peers
);

/**
 * PML->PTL Initialize a send request for TCP cache.
 *
 * @param ptl (IN)       PTL instance
 * @param request (IN)   Pointer to allocated request.
 *
 **/
extern int mca_ptl_ib_request_init(
        struct mca_ptl_base_module_t* ptl,
        struct mca_pml_base_send_request_t*
        );

/**
 * PML->PTL Cleanup a send request that is being removed from the cache.
 *
 * @param ptl (IN)       PTL instance
 * @param request (IN)   Pointer to allocated request.
 *
 **/
extern void mca_ptl_ib_request_fini(
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
extern void mca_ptl_ib_request_return(
    struct mca_ptl_base_module_t* ptl,
    struct mca_pml_base_send_request_t*
);

/**
 * PML->PTL Notification that a receive fragment has been matched.
 *
 * @param ptl (IN)          PTL instance
 * @param recv_frag (IN)    Receive fragment
 *
 */
extern void mca_ptl_ib_matched(
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
extern int mca_ptl_ib_send(
    struct mca_ptl_base_module_t* ptl,
    struct mca_ptl_base_peer_t* ptl_peer,
    struct mca_pml_base_send_request_t*,
    size_t offset,
    size_t size,
    int flags
);

/**
 * Return a recv fragment to the modules free list.
 *
 * @param ptl (IN)   PTL instance
 * @param frag (IN)  IB receive fragment
 *
 */
extern void mca_ptl_ib_recv_frag_return(
    struct mca_ptl_base_module_t* ptl,
    struct mca_ptl_ib_recv_frag_t* frag
);


/**
 * Return a send fragment to the modules free list.
 *
 * @param ptl (IN)   PTL instance
 * @param frag (IN)  IB send fragment
 *
 */
extern void mca_ptl_ib_send_frag_return(
    struct mca_ptl_base_module_t* ptl,
    struct mca_ptl_ib_send_frag_t*
);

#endif
