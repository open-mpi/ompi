/*
 * $HEADER$
 */
/**
 * @file
 *
 * P2P Transport Layer (PTL)
 *
 * An MCA component type that allows the PML (mca_pml_t) to support a
 * variety of network transports concurrently. The PTL layer is
 * responsible for the reliable delivery of message fragments, while
 * the assignment and scheduling of fragments to PTLs is handled by
 * the upper layer.
 *
 * PTL Initialization:
 *
 * During library initialization, all available PTL components are
 * loaded and opened via their mca_base_open_component_fn_t
 * function. If possible, the component should load and open
 * regardless of wether the transport is available. This allows
 * parameters used by the component to be reported on by tools such as
 * ompi_info.
 * 
 * The mca_ptl_base_component_init_fn_t() is then called for each of the
 * components that are succesfully opened. The component init function may
 * return either:
 *
 * (1) a NULL list of PTL instances if the transport is not available,
 * (2) a list containing a single PTL instance, where the PTL provides
 *     a layer of abstraction over multiple physical devices (e.g. NICs),
 * (3) a list containing multiple PTL instances where each PTL instance
 *     corresponds to a single physical device.
 * 
 * If multiple network devices are available for a given transport,
 * the preferred approach is (3) above. In this case, the PML layer
 * will handle scheduling across the available resources, and
 * fail-over in the event of a PTL failure.  If the second approach is
 * used, and a single PTL instance abstracts multiple physical
 * devices, the PTL assumes all responsibility for scheduling/failover
 * within those devices.
 *
 * During module initialization, the module should post any addressing
 * information required by its peers. An example would be the TCP
 * listen port opened by the TCP module for incoming connection
 * requests. This information is published to peers via the
 * mca_base_modex_send() interface. Note that peer information will
 * not be available via mca_base_modex_recv() during the module's init
 * function. However, it is guaranteed to be available during PTL
 * selection.
 *
 * PTL Selection:
 *
 * The PML maintains a list of available PTL instances, sorted by
 * their exclusivity ranking. This is a relative ranking that is used
 * to select the set of PTLs that may be used to reach a given
 * destination.  The PTL modules are queried via their
 * mca_ptl_base_add_proc_fn_t() to determine if they are able to reach
 * a given destination.  The first PTL module that returns success is
 * selected. Subsequent PTL modules are queried only if they are at
 * the same exclusivity ranking.
 * 
 * An example of how this might be used:
 *
 * PTL         Exclusivity   Comments
 * --------    -----------   ------------------
 * LO              100       Selected exclusively for local process
 * SM               50       Selected exclusively for other processes on host
 * IB                0       Selected based on network reachability
 * IB                0       Selected based on network reachability
 * TCP               0       Selected based on network reachability
 * TCP               0       Selected based on network reachability
 *
 * When a PTL module is selected, it may choose to optionally return a
 * pointer to an an mca_ptl_base_peer_t data structure to the
 * PML. This pointer is cached by the PML and returned to the PTL on
 * subsequent data transfer calls to this specific process.  The
 * actual contents of the data structure are defined on a per PTL
 * basis, and are typically used to cache addressing or connection
 * information, such as the TCP socket used by the TCP PTL.
 *
 * Send Path:
 *
 * When multiple PTLs are available to reach a given destination, a
 * single request (that is large enough) will be split across the
 * available PTLs. The PML scheduler will determine the PTL to use for
 * the first fragment based on the relative latency ranking exported
 * by the PTLs.
 *
 * To minimize latency, and allow for derived types of the
 * mca_pml_base_send_request_t, the PML will call the selected PTLs
 * ptl_request_alloc() method to allocate the send request
 * descriptor. The PTL should return a derived type of
 * mca_pml_base_send_request_t, that contains space for both the base
 * send request and initial fragment descriptor
 * (mca_ptl_base_send_frag_t or derived type).  This approach allows
 * all of the descriptors required to initiate the send to be
 * allocated from a free list in a single operation.
 *
 * When the request is started, the PML will call the selected PTL's
 * ptl_send() method with up to ptl_first_frag_size bytes of the
 * request. The PTL should attempt to deliver up to the requested
 * number of bytes. The number of bytes actually fragmented and queued
 * for delivery must be updated on the send request to reflect the
 * current offset into the send buffer.
 *
 * If the request is larger than ptl_first_frag_size, the remainder of
 * the request will be scheduled across potentially multiple PTLs,
 * upon an acknowledgment from the peer that the request has been
 * matched on the receive side. The PTL must defer calling
 * ptl_send_progress() on the initial fragment until an acknowledment
 * is received, as this signals to the PML that the remaining
 * fragments may be scheduled. For further detail on the scheduling
 * algorithm, refer to the PML (mca_pml_t) documentation.
 *
 * As subsequent fragments are completed by the PTLs, the
 * ptl_send_progress() method should be called to update the status of
 * the send request. Note that each PTL is responsible for managing
 * the resources associated with send fragments and their allocation
 * from and return to internal caches/free lists.
 * 
 * Recv Path:
 *
 * The first fragment of a message is sent with a header type of
 * mca_ptl_base_match_header_t.  When a header of this type is
 * received, to minimize latency the PTL should call the ptl_match()
 * method as soon as entire header is available, potentially prior to
 * receiving any data associated with the first fragment. If a match
 * is made, the PML will call the ptl_matched() method of the
 * fragments PTL.
 *
 * The ptl_matched() method should generate, if required, an ack to
 * the source process. An ack is required if the
 * MCA_PTL_FLAGS_ACK_MATCHED bit is set by the source in the initial
 * message header.  The ack should contain a pointer to the matched
 * request, along with the pointer to the orignal send fragment
 * contained in the initial message header.
 *
 * On receipt of the ack, the source will schedule any remaining
 * fragments. The selected PTLs should generate the remaining
 * fragments with an mca_ptl_base_frag_header_t, which contains a
 * placeholder for a pointer to the matched receive request. This
 * allows the receiver to avoid calling the matching logic for
 * subsequent fragments. As fragments are completed, each PTL calls
 * their ptl_recv_progress() method to update the PML with the request
 * status.
 */

#ifndef MCA_PTL_H
#define MCA_PTL_H

#include "mca/mca.h"
#include "include/ompi.h"
#include "class/ompi_list.h"
#include "proc/proc.h"
#include "mca/pml/pml.h"

/*
 * PTL types
 */

struct mca_ptl_base_module_t;
struct mca_ptl_base_peer_t;
struct mca_ptl_base_fragment_t;
struct mca_pml_base_recv_request_t;
struct mca_pml_base_send_request_t;
struct mca_ptl_base_recv_frag_t;
struct mca_ptl_base_send_frag_t;
struct mca_ptl_base_match_header_t;

typedef uint64_t mca_ptl_sequence_t;
typedef uint64_t mca_ptl_tstamp_t;
typedef ompi_list_t mca_ptl_queue_t;

typedef enum { 
    MCA_PTL_ENABLE 
} mca_ptl_control_t;
                                                                                                            
/**
 * PTL flags 
 */
#define MCA_PTL_PUT 1 
#define MCA_PTL_GET 2

/*
 *  PTL component interface functions and datatype.
 */

/**
 * MCA->PTL Intializes the PTL component and creates specific PTL
 * module(s).
 *
 * @param num_ptls (OUT) Returns the number of ptl instances created, or 0
 *                       if the transport is not available.
 *
 * @param allow_multi_user_threads (OUT) Whether this component can
 * run at MPI_THREAD_MULTIPLE or not.
 *
 * @param have_hidden_threads (OUT) Whether this component may use
 * hidden threads (e.g., progress threads) or not.
 *
 * @return Array of pointers to PTL modules, or NULL if the transport  
 *         is not available.
 *
 * During component initialization, the PTL component should discover
 * the physical devices that are available for the given transport,
 * and a PTL instance created to represent each available device. Any
 * addressing information required by peers to reach the available
 * devices should be published during the component init via the
 * mca_base_modex_send() interface.
 */
typedef struct mca_ptl_base_module_t** (*mca_ptl_base_component_init_fn_t)(
    int *num_ptls, 
    bool *allow_multi_user_threads,
    bool *have_hidden_threads
);


/**
 * MCA->PTL Called to dynamically change a component parameter.
 *
 * @param flag (IN)   Parameter to change.
 * @param value (IN)  Optional parameter value.
 *
 * @return           OMPI_SUCCESS or error code on failure.
 */
typedef int (*mca_ptl_base_component_control_fn_t)(
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
typedef int (*mca_ptl_base_component_progress_fn_t)(
    mca_ptl_tstamp_t tstamp
);


/**
 *  PTL component descriptor. Contains component version information
 *  and component open/close/init functions.
 */

struct mca_ptl_base_component_1_0_0_t {
  mca_base_component_t ptlm_version;
  mca_base_component_data_1_0_0_t ptlm_data;

  mca_ptl_base_component_init_fn_t ptlm_init;
  mca_ptl_base_component_control_fn_t ptlm_control;
  mca_ptl_base_component_progress_fn_t ptlm_progress;
};
typedef struct mca_ptl_base_component_1_0_0_t mca_ptl_base_component_1_0_0_t;
typedef struct mca_ptl_base_component_1_0_0_t mca_ptl_base_component_t;


/*
 * PTL instance interface functions and datatype.
 */


/**
 * MCA->PTL Clean up any resources held by PTL instance before the 
 * module is unloaded.
 *  
 * @param ptl (IN)   PTL instance.
 *
 * Prior to unloading a PTL module, the MCA framework will call the PTL
 * finalize method for each PTL instance.
 * 
 */
typedef int (*mca_ptl_base_module_finalize_fn_t)(
    struct mca_ptl_base_module_t* ptl
);
                                                                                                         
/**
 * PML->PTL notification of change in the process list. 
 *
 * @param ptl (IN)            PTL instance
 * @param nprocs (IN)         Number of processes
 * @param procs (IN)          Set of processes
 * @param peer (OUT)          Set of (optional) mca_ptl_base_peer_t instances returned by PTL.
 * @param reachable (IN/OUT)  Bitmask indicating set of peer processes that are reachable by this PTL.
 * @return                    OMPI_SUCCESS or error status on failure.
 *
 * The mca_ptl_base_module_add_procs_fn_t() is called by the PML to determine
 * the set of PTLs that should be used to reach the specified process.
 * A return value of OMPI_SUCCESS indicates the PTL should be added to the
 * set used to reach the proc. The peers addressing information may be 
 * obtained by the PTL via the mca_base_modex_recv() function if required. 
 * The PTL may optionally return a pointer to a mca_ptl_base_peer_t data 
 * structure, to cache peer addressing or connection information.
 */
typedef int (*mca_ptl_base_module_add_procs_fn_t)(
    struct mca_ptl_base_module_t* ptl, 
    size_t nprocs,
    struct ompi_proc_t** procs, 
    struct mca_ptl_base_peer_t** peer,
    ompi_bitmap_t* reachable
);

/**
 * PML->PTL notification of change in the process list.
 *
 * @param ptl (IN)     PTL instance
 * @param nprocs (IN)  Number of processes
 * @param proc (IN)    Set of processes
 * @param peer (IN)    Set of peer addressing information.
 * @return             Status indicating if cleanup was successful
 *
 * If the process list shrinks, the PML will notify the PTL of the
 * change. Peer addressing information cached by the PML is provided
 * for cleanup by the PTL.
 */
typedef int (*mca_ptl_base_module_del_procs_fn_t)(
    struct mca_ptl_base_module_t* ptl, 
    size_t nprocs,
    struct ompi_proc_t** procs, 
    struct mca_ptl_base_peer_t**
);

/**
 * PML->PTL Initialize a send request for use by the PTL. 
 *
 * @param ptl (IN)       PTL instance
 * @param request (IN)   Pointer to allocated request.
 *
 * To reduce latency (number of required allocations), the PML allocates additional
 * space along w/ each request - that may be used by the PTL for additional control
 * information (e.g. first fragment descriptor). If the PTL intends to use this space
 * the ptl_cache_bytes attributes should be set to reflect the number of bytes needed
 * by the PTL on a per-request basis. This space is allocated contiguously along with
 * the mca_pml_base_send_request_t, w/ the space available to the PTL immediately 
 * following the request.
 * 
 * The init function is called the first time the request is ued by the PTL. On 
 * completion of the request - the PML will cache the request for later use by the
 * same PTL. When the request is re-used from the cache, the init function is NOT
 * called for subsequent sends. 
 */
typedef int (*mca_ptl_base_module_request_init_fn_t)(
    struct mca_ptl_base_module_t* ptl, 
    struct mca_pml_base_send_request_t* request
);


/**
 * PML->PTL Cleanup any resources that may have been associated with the
 *          request by the PTL.
 *
 * @param ptl (IN)       PTL instance
 * @param request (IN)   Pointer to allocated request.
 *
 * The fini function is called when the PML removes a request from the PTLs
 * cache (due to resource constraints) or the cache limit has been reached, prior 
 * to re-using the request for another PTL. This provides the PTL the chance to 
 * cleanup/release any resources cached on the send descriptor by the PTL.
 */

typedef void (*mca_ptl_base_module_request_fini_fn_t)(
    struct mca_ptl_base_module_t* ptl, 
    struct mca_pml_base_send_request_t* request
);

/**
 * PML->PTL Initiate a send to the peer.
 *
 * @param ptl (IN)               PTL instance
 * @param ptl_base_peer (IN)     PTL peer addressing
 * @param request (IN)           Send request
 * @param offset                 Current offset into packed/contiguous buffer.
 * @param size (IN/OUT)          Number of bytes PML is requesting PTL to deliver,
 *                               PTL returns number of bytes sucessfully fragmented
 * @param flags (IN)             Flags that should be passed to the peer via the message header.
 * @param request (OUT)          OMPI_SUCCESS if the PTL was able to queue one or more fragments
 *
 * The PML implements a rendevouz protocol, with up to the PTL defined threshold
 * bytes of the message sent in eager send mode.
 *
 * If the PTL is unable to fragment the requested size, possibly due to resource
 * constraints or datatype alighnment/offset, it should return the number of bytes
 * actually fragmented in the size parameter.
 */
typedef int (*mca_ptl_base_module_send_fn_t)(
    struct mca_ptl_base_module_t* ptl,
    struct mca_ptl_base_peer_t* ptl_base_peer,
    struct mca_pml_base_send_request_t* request,
    size_t offset,
    size_t size,
    int flags
);
                                                                                                                       
/**
 * PML->PTL Initiate a put to the peer.
 *
 * @param ptl (IN)               PTL instance
 * @param ptl_base_peer (IN)     PTL peer addressing
 * @param request (IN)           Send request
 * @param offset                 Current offset into packed/contiguous buffer.
 * @param size (IN/OUT)          Number of bytes PML is requesting PTL to deliver,
 *                               PTL returns number of bytes sucessfully fragmented
 * @param flags (IN)             Flags that should be passed to the peer via the message header.
 * @param request (OUT)          OMPI_SUCCESS if the PTL was able to queue one or more fragments
 *
 * The PML implements a rendevouz protocol, with up to the PTL defined
 * threshold bytes of the message sent in eager send mode (via
 * mca_ptl_base_module_send_fn_t). On receipt of an acknowledgment
 * from the peer, the PML will schedule the remaining fragments. If
 * the PTL supports RDMA functionality, these subsequent transfers may
 * use RDMA put semantics.
 *
 * If the PTL is unable to fragment the requested size, possibly due
 * to resource constraints or datatype alighnment/offset, it should
 * return the number of bytes actually fragmented in the size
 * parameter.
 */
typedef int (*mca_ptl_base_module_put_fn_t)(
    struct mca_ptl_base_module_t* ptl,
    struct mca_ptl_base_peer_t* ptl_base_peer,
    struct mca_pml_base_send_request_t* request,
    size_t offset,
    size_t size,
    int flags
);


/**
 * PML->PTL Initiate a get from a peer.
 *
 * @param ptl (IN)               PTL instance
 * @param ptl_base_peer (IN)     PTL peer addressing
 * @param request (IN)           Recv request 
 * @param offset                 Current offset into packed/contiguous buffer.
 * @param size (IN/OUT)          Number of bytes PML is requesting PTL to pull from peer, 
 *                               PTL returns number of bytes sucessfully fragmented.
 * @param flags (IN)             
 * @param request (OUT)          OMPI_SUCCESS if the PTL was able to queue one or more fragments
 *
 * Initiate an RDMA get request to pull data from the peer. This is initiated
 * at the receiver side when a request is matched if the PTL indicates that it
 * supports RDMA get semantics.
 */

typedef int (*mca_ptl_base_module_get_fn_t)(
    struct mca_ptl_base_module_t* ptl, 
    struct mca_ptl_base_peer_t* ptl_base_peer, 
    struct mca_pml_base_recv_request_t* request,
    size_t offset,
    size_t size,
    int flags
);

/**
 * PTL->PML Notification from the PTL to the PML that a new fragment
 * has arrived and can be matched against posted receives.
 *
 * @param ptl (IN)       PTL instance
 * @param recv_frag      Receive fragment
 * @param header (IN)    Message header
 *
 * A fragment may be matched either when a new receive is posted,
 * or on receipt of a fragment from the network. In either case,
 * the PML will downcall into the PTL to provide a notification 
 * that the match was made.
 *
 * The message header used for matching is not required to be
 * contained within the receive fragment. However, if the match is 
 * not made, the matching code will copy the supplied header into the 
 * recv fragment so that the match can be made when the receive is posted.
 */
typedef bool (*mca_ptl_base_module_match_fn_t)(
    struct mca_ptl_base_module_t* ptl,
    struct mca_ptl_base_recv_frag_t* recv_frag,
    struct mca_ptl_base_match_header_t* header
);


/**
 * PML->PTL Notification from the PML to the PTL that a receive has 
 * been posted and matched against the indicated fragment.
 *
 * @param ptl (IN)       PTL instance
 * @param recv_frag      Matched fragment
 *
 */

typedef void (*mca_ptl_base_module_matched_fn_t)(
    struct mca_ptl_base_module_t* ptl, 
    struct mca_ptl_base_recv_frag_t* request
);

/**
 * PTL->PML Notification from the PTL to the PML that a fragment
 * has completed (e.g. been successfully delivered into users buffer)
 *
 * @param ptr(IN)               PTL instance
 * @param recv_request (IN)     Receive Request
 * @param bytes_received (IN)   Number of bytes received from peer.
 * @param bytes_delivered (IN)  Number of bytes delivered to application.
 */
typedef void (*mca_ptl_base_module_recv_progress_fn_t)(
    struct mca_ptl_base_module_t* ptl,
    struct mca_pml_base_recv_request_t* recv_request,
    size_t bytes_received,
    size_t bytes_delivered
);

/**
 * PTL->PML Notification from the PTL to the PML that a fragment
 * has completed (e.g. been successfully delivered to peer)
 *
 * @param ptr(IN)             PTL instance
 * @param send_request (IN)   Send Request
 * @param bytes_sent (IN)     Number of bytes sent to peer.
 */
typedef void (*mca_ptl_base_module_send_progress_fn_t)(
    struct mca_ptl_base_module_t* ptl,
    struct mca_pml_base_send_request_t* send_request,
    size_t bytes_sent
);

/**
 * PTL instance interface functions and attributes.
 */
struct mca_ptl_base_module_t {

    /* PTL common attributes */
    mca_ptl_base_component_t* ptl_component; /**< pointer back to the PTL component structure */
    size_t      ptl_cache_size;        /**< maximum size of request cache for this PTL */
    size_t      ptl_cache_bytes;       /**< number of bytes required by PTL for request cache */
    size_t      ptl_first_frag_size;   /**< maximum size of first fragment -- eager send */
    size_t      ptl_min_frag_size;     /**< threshold below which the PTL will not fragment */
    size_t      ptl_max_frag_size;     /**< maximum fragment size supported by the PTL */
    uint32_t    ptl_exclusivity;       /**< indicates this PTL should be used exclusively */
    uint32_t    ptl_latency;           /**< relative ranking of latency used to prioritize ptls */
    uint32_t    ptl_bandwidth;         /**< bandwidth (Mbytes/sec) supported by each endpoint */
    uint32_t    ptl_flags;             /**< flags (put/get...) */

    /* PML->PTL function table */
    mca_ptl_base_module_add_procs_fn_t        ptl_add_procs;
    mca_ptl_base_module_del_procs_fn_t        ptl_del_procs;
    mca_ptl_base_module_finalize_fn_t         ptl_finalize;
    mca_ptl_base_module_send_fn_t             ptl_send;
    mca_ptl_base_module_put_fn_t              ptl_put;
    mca_ptl_base_module_get_fn_t              ptl_get;
    mca_ptl_base_module_matched_fn_t          ptl_matched;
    mca_ptl_base_module_request_init_fn_t     ptl_request_init;
    mca_ptl_base_module_request_fini_fn_t     ptl_request_fini;

    /* PTL->PML function table - filled in by PML at init */
    mca_ptl_base_module_match_fn_t            ptl_match;
    mca_ptl_base_module_send_progress_fn_t    ptl_send_progress;
    mca_ptl_base_module_recv_progress_fn_t    ptl_recv_progress;

    /* Allow the canibalization of the PTL */
    struct mca_ptl_base_module_t*             ptl_stack;

    /* for use by PML only */
    struct mca_pml_base_ptl_t*                ptl_base;
};
typedef struct mca_ptl_base_module_t mca_ptl_base_module_t;

/*
 * Macro for use in modules that are of type ptl v1.0.0
 */
#define MCA_PTL_BASE_VERSION_1_0_0 \
  /* coll v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* ptl v1.0 */ \
  "ptl", 1, 0, 0

#endif /* OMPI_MCA_PTL_H */
