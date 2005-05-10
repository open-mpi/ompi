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
#ifndef MCA_PTL_PORTALS_H
#define MCA_PTL_PORTALS_H

#include <portals3.h>

#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "class/ompi_bitmap.h"
#include "class/ompi_free_list.h"
#include "class/ompi_proc_table.h"

/* need this type for the compat header */
struct mca_ptl_portals_component_t;
typedef struct mca_ptl_portals_component_t mca_ptl_portals_component_t;


/**
 * Portals PTL component.
 */
struct mca_ptl_portals_component_t {
    /** base PTL component */
    mca_ptl_base_component_1_0_0_t super;

    /** output channel for debugging.  Value settings when using
     * output_verbose:
     *
     *  - 0 : critical user information
     *  - 10: initialization / shutdown diagnostic information
     *  - 20: general execution diagnostic information
     *  - 99: useful only to developers
     */
    int portals_output;

#if PTL_PORTALS_UTCP
    /** ethernet interface to use - only has meaning with utcp
        reference */
    char *portals_ifname;
#endif

    /** Number of currently active portals modules */
    uint32_t portals_num_modules;
    /** List of currently available modules */
    struct mca_ptl_portals_module_t **portals_modules;

    /** initial size of free lists */
    int portals_free_list_init_num;
    /** max size of free lists */
    int portals_free_list_max_num;
    /** numer of elements to grow free lists */
    int portals_free_list_inc_num;

    /** free list of portals send fragments */
    ompi_free_list_t portals_send_frags;
    /** free list of portals recv fragments */
    ompi_free_list_t portals_recv_frags;

    /** queue of pending sends */
    ompi_list_t portals_pending_acks;

    /** lock for accessing component */
    ompi_mutex_t portals_lock;
};

struct mca_ptl_portals_recv_frag_t;
struct mca_ptl_portals_send_frag_t;

extern mca_ptl_portals_component_t mca_ptl_portals_component;

/**
 * Register Portals module parameters with the MCA framework
 */
extern int mca_ptl_portals_component_open(void);

/**
 * Any final cleanup before being unloaded.
 */
extern int mca_ptl_portals_component_close(void);

/**
 * MCA->PTL Intializes the PTL component and creates specific PTL
 * module(s).
 *
 * @param num_ptls (OUT) Returns the number of ptl instances created, or 0
 *                       if the transport is not available.
 *
 * @param allow_multi_user_threads (OUT) Indicated wether this component can
 * run at MPI_THREAD_MULTIPLE or not.
 *
 * @param have_hidden_threads (OUT) Whether this component uses
 * hidden threads (e.g., progress threads) or not.
 *
 * @return Array of pointers to PTL modules, or NULL if the transport
 *         is not available.
 *
 * During component initialization, the PTL component should discover
 * the physical devices that are available for the given transport,
 * and create a PTL instance to represent each device. Any addressing
 * information required by peers to reach the device should be published
 * during this function via the mca_base_modex_send() interface.
 *
 */

extern mca_ptl_base_module_t** mca_ptl_portals_component_init(
    int *num_ptls, 
    bool enable_progress_threads,
    bool enable_mpi_threads
);


/**
 * MCA->PTL Called to dynamically change a component parameter.
 *
 * @param flag (IN)   Parameter to change.
 * @param value (IN)  Optional parameter value.
 *
 * @return            OMPI_SUCCESS or error code on failure.
 *
 * The only supported parameter is currently MCA_PTL_ENABLE,
 * which can be used by the PML to enable/disable forwarding
 * by the PTL.
 */

extern int mca_ptl_portals_component_control(
    int param,
    void* value,
    size_t size
);

/**
 * MCA->PTL Called to progress outstanding requests for
 * non-threaded polling environments.
 *
 * @param tstamp     Current time.
 * @return           OMPI_SUCCESS or error code on failure.
 */

extern int mca_ptl_portals_component_progress(
   mca_ptl_tstamp_t tstamp
);

/**
 * Portals PTL module.
 */
struct mca_ptl_portals_module_t {
    mca_ptl_base_module_t super;         /**< base PTL module interface */

    /* number of mds for first frags */
    int first_frag_num_entries;
    /* size of each md for first frags */
    int first_frag_entry_size;
    /* size for event queue */
    int first_frag_queue_size;

    /* frag receive data */
    bool frag_queues_created;
    /* frag receive event queue */
    ptl_handle_eq_t frag_eq_handle;

    /** our portals network interface */
    ptl_handle_ni_t ni_handle;
    /** the limits returned from PtlNIInit for interface */
    ptl_ni_limits_t limits;
};
typedef struct mca_ptl_portals_module_t mca_ptl_portals_module_t;


extern mca_ptl_portals_module_t mca_ptl_portals_module;

/**
 * Create/initialize the PORTALS PTL modules.
 * @return OMPI_SUCCESS or error status on failure.
 */

extern int mca_ptl_portals_module_init(void);


/**
 * Cleanup any resources held by the PTL.
 * 
 * @param ptl  PTL instance.
 * @return     OMPI_SUCCESS or error status on failure.
 */

extern int mca_ptl_portals_finalize(
    struct mca_ptl_base_module_t* ptl
);


/**
 * PML->PTL notification of change in the process list.
 *
 * @param ptl (IN)            PTL instance
 * @param nprocs (IN)         Number of processes
 * @param procs (IN)          Set of processes
 * @param peer (OUT)          Set of (optional) mca_ptl_base_peer_t instances returned by PTL. * @param reachable (OUT)     Bitmask indicating set of peer processes that are reachable by this PTL.
 * @return                    OMPI_SUCCESS or error status on failure.
 *
 * The mca_ptl_base_module_add_procs_fn_t() is called by the PML to
 * determine the set of PTLs that should be used to reach each process.
 * Any addressing information exported by the peer via the mca_base_modex_send()
 * function should be available during this call via the corresponding
 * mca_base_modex_recv() function. The PTL may utilize this information to
 * determine reachability of each peer process.
 *
 * For each process that is reachable by the PTL, the bit corresponding to the index
 * into the proc array (nprocs) should be set in the reachable bitmask. The PML
 * provides the PTL the option to return a pointer to a data structure defined
 * by the PTL that is returned to the PTL on subsequent calls to the PTL data
 * transfer functions (e.g ptl_send). This may be used by the PTL to cache any addressing
 * or connection information (e.g. TCP socket, IP queue pair).
 */

extern int mca_ptl_portals_add_procs(
    struct mca_ptl_base_module_t* ptl,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_ptl_base_peer_t** peers,
    ompi_bitmap_t* reachable
);


/**
 * PML->PTL notification of change to the process list.
 *
 * @param ptl (IN)     PTL instance
 * @param nprocs (IN)  Number of processes
 * @param proc (IN)    Set of processes
 * @param peer (IN)    Set of peer addressing information.
 * @return             Status indicating if cleanup was successful
 *
 * When the process list changes, the PML notifies the PTL of the
 * change, to provide the opportunity to cleanup or release any
 * resources associated with the peer.
 */

extern int mca_ptl_portals_del_procs(
    struct mca_ptl_base_module_t* ptl,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_ptl_base_peer_t** peers
);


/**
 * PML->PTL Initialize a send request for use by the PTL.
 *
 * @param ptl (IN)       PTL instance
 * @param request (IN)   Pointer to allocated request.
 *
 * To reduce latency (number of required allocations), the PML allocates up
 * to ptl_cache_bytes of additional space contigous w/ the base send request.
 * This space may be used by the PTL for additional control information (e.g.
 * first fragment descriptor).
 *
 * The ptl_request_init() function is called by the PML when requests are
 * allocated to the PTLs cache. These requests will be cached by the PML
 * on completion and re-used by the same PTL w/out additional calls to
 * ptl_request_init().
 *
 * If the cache size is exceeded, the PML may pass requests to ptl_send/ptl_put
 * that have been taken from the global pool and have not been initialized by the
 * PTL. These requests will have the req_cached attribute set to false.
 *
 */

extern int mca_ptl_portals_request_init(
    struct mca_ptl_base_module_t* ptl,
    struct mca_ptl_base_send_request_t*
);


/**
 * PML->PTL Cleanup any resources that may have been associated with the
 *          request by the PTL.
 *
 * @param ptl (IN)       PTL instance
 * @param request (IN)   Pointer to allocated request.
 *
 * The ptl_request_fini function is called when the PML removes a request
 * from the PTLs cache (due to resource constraints).  This routine provides
 * the PTL the chance to cleanup/release any resources cached on the send
 * descriptor by the PTL.
 */
                                                                                              
extern void mca_ptl_portals_request_fini(
    struct mca_ptl_base_module_t* ptl,
    struct mca_ptl_base_send_request_t*
);


/**
 * PML->PTL Notification from the PML to the PTL that a receive
 * has been posted and matched against the indicated fragment.
 *
 * @param ptl (IN)       PTL instance
 * @param recv_frag      Matched fragment
 *
 * The ptl_matched() function is called by the PML when a fragment
 * is matched to a posted receive. This may occur during a call to
 * ptl_match() if the receive is matched, or at a later point in time
 * when a matching receive is posted.
 *
 * When this routine is called, the PTL is responsible for generating
 * an acknowledgment to the peer if the MCA_PTL_FLAGS_ACK
 * bit is set in the original fragment header. Additionally, the PTL
 * is responsible for transferring any data associated with the fragment
 * into the users buffer utilizing the datatype engine, and notifying
 * the PML that the fragment has completed via the ptl_recv_progress()
 * function.
 */

extern void mca_ptl_portals_matched(
    struct mca_ptl_base_module_t* ptl,
    struct mca_ptl_base_recv_frag_t* frag
);

/**
 * PML->PTL Initiate a send to the peer.
 *
 * @param ptl (IN)               PTL instance
 * @param ptl_base_peer (IN)     PTL peer addressing
 * @param request (IN)           Send request
 * @param offset                 Current offset into packed/contiguous buffer.
 * @param size (IN)              Number of bytes PML is requesting PTL to deliver,
 * @param flags (IN)             Flags that should be passed to the peer via the message header.
 * @param request (OUT)          OMPI_SUCCESS if the PTL was able to queue one or more fragments
 *
 * The PML implements a rendevouz protocol, with up to the PTL threshold
 * (ptl_first_frag_size) bytes of the message sent in eager send mode. The ptl_send()
 * function is called by the PML to initiate the send of the first message fragment.
 *
 * The PTL is responsible for updating the current data offset (req_offset) in the
 * request to reflect the actual number of bytes fragmented.  This may be less than
 * the requested size, due to resource constraints or datatype alighnment/offset. If
 * an acknowledgment is required, the MCA_PTL_FLAGS_ACK bit will be set in the
 * flags parameter. In this case, the PTL should not call ptl_send_progress() function
 * to indicate completion of the fragment until the ack is received. For all other
 * fragments ptl_send_progress() may be called based on local completion semantics.
 */

extern int mca_ptl_portals_send(
    struct mca_ptl_base_module_t* ptl,
    struct mca_ptl_base_peer_t* ptl_peer,
    struct mca_ptl_base_send_request_t*,
    size_t offset,
    size_t size,
    int flags
);



extern int mca_ptl_portals_module_enable(struct mca_ptl_portals_module_t *ptl,
                                         int value);

#endif
