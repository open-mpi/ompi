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
 * function. The PTL open function should register any mca parameters
 * used to tune/adjust the behaviour of the ptl (mca_base_param_register_int(),
 * mca_base_param_register_string()). Note that the open function may fail
 * if the resources (e.g. shared libraries, etc) required by the network
 * transport are not available.
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
 * mca_base_modex_send() interface. Note that peer information is not
 * guaranteed to be available via mca_base_modex_recv() during the 
 * module's init function. However, it will be available during 
 * PTL selection (mca_ptl_base_add_proc_fn_t()).
 *
 * PTL Selection:
 *
 * The PML builds an ordered list of the available PTL instances sorted 
 * by their exclusivity ranking. This is a relative ranking that is used
 * to determine the set of PTLs that may be used to reach a given destination.  
 * During startup the PTL modules are queried via their 
 * mca_ptl_base_add_proc_fn_t() to determine if they are able to reach
 * a given destination.  The PTL module with the highest ranking that 
 * returns success is selected. Subsequent PTL modules are selected only 
 * if they have the same exclusivity ranking.
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
 * pointer to an an mca_ptl_base_peer_t data structure to the PML. 
 * This pointer is treated as an opaque handle by the PML and is
 * returned to the PTL on subsequent data transfer calls to the 
 * corresponding destination process.  The actual contents of the  
 * data structure are defined on a per PTL basis, and may be used to 
 * cache addressing or connection information, such as a TCP socket 
 * or IB queue pair.
 *
 * Send Path:
 *
 * When multiple PTLs are available to reach a given destination,
 * a single request (that is large enough) will be split across the
 * available PTLs.  For each destination process, the PML maintains two 
 * list of PTLs, one set of PTLs that exhibit the lowest latency, and 
 * a second set that are used for bulk data transfer. The set of low 
 * latency PTLs are used in a round-robin fashion to schedule the first 
 * fragment of a message, while the remainder of the message will be 
 * scheduled across the second set based on the bandwidth of the available
 * PTLs.
 *
 * The PML is responsible for managing the state (allocation, initialization, 
 * and release) of send request descriptors (mca_pml_base_send_request_t). 
 * However, to minimize the latency associated with allocating resources to 
 * a request, the PML provides the capability to cache send requests 
 * descriptors on a per-PTL basis. Each PTL exports two variables 
 * (ptl_cache_size and ptl_cache_bytes) that control this behaviour. The 
 * variable ptl_cache_size specifies the maximum size of the cache. If a 
 * request cannot be provided from the cache, a request descriptor from the 
 * global pool will be used instead, and the req_cached attribute of the 
 * request set to false. The request cache initially starts off empty and 
 * is grown by the PML up to the specified limit. The PTL variable, 
 * ptl_cache_bytes, can be used to specify that additional memory should be 
 * allocated by the PML in one contigous block along with the base send request 
 * (mca_pml_base_send_request_t) for use by the PTL. The PTLs ptl_request_init() 
 * method is then called to initialize this additional memory and associating 
 * any PTL specific resources with the request.  If a request is removed from 
 * the cache, the ptl_request_fini() method will be called to allow the PTL 
 * to release any resources associated with the request descriptor.
 *
 * When the request is started, the PML will call the selected PTL's
 * ptl_send() method with up to the PTL's threshold (ptl_first_frag_size) 
 * bytes of the request. The PTL should attempt to deliver the requested
 * number of bytes. However, this may not be possible due to resource
 * contraints or datatype alignment/offset. The PTL is responsible for
 * updating the number of bytes actually fragmented and queued for delivery 
 * on the send request (mca_pml_base_send_request.req_offset) to reflect 
 * the current offset into the send buffer.
 *
 * If the request is larger than ptl_first_frag_size, the remainder of
 * the request will be scheduled upon an acknowledgment from the peer 
 * that the request has been matched on the receive side. The PTL receiving
 * the fragment is responsible for generating an acknowledgment when the 
 * MCA_PTL_FLAGS_ACK bit is set in the flags field of the fragment 
 * header. The PTL receiving an ack is responsible for updating the
 * the send request descriptor to point to the matched recv descriptor 
 * and the destination buffer address at the remote process. The address of 
 * the recv descriptor is sent back in the header of subsequent fragments 
 * to avoid the cost of matching the additional fragments at the receiver 
 * while the remote address of the destination buffer may be used in 
 * subsequent data transfer operations to support RDMA put operations.
 * 
 * On receipt of an acknowedgment the PTL should call the ptl_send_progress() 
 * function to update the status (number of bytes delivered) of the send request. 
 * Note that although this function is associated with the PTL, it is provided/
 * set by the PML during initialization.
 *
 * If this was the initial fragment of a large message, the PML will schedule 
 * the remaining fragments during this callback. For subsequent fragments
 * the PML will call the PTLs ptl_put() interface function. Since the destination 
 * address in the remote process is available, RDMA put operations could be used 
 * if supported by the underlying network transport. Note that currently the PML 
 * makes no other distinction between ptl_send/ptl_put. 
 *
 * As subsequent fragments are completed by the PTLs, the ptl_send_progress() 
 * function should be called to update the status of request. Note that this
 * may be based on local completion semantics or could require a PTL specific
 * acknowledgment based on the underlying transfer protocol. Upon completion,
 * the PTL is responsible for managing all resources associated with send 
 * fragments and their return to internal caches/free lists.
 * 
 * Recv Path:
 *
 * The PML sets two additional callback functions on the PTL during
 * initialization. These callbacks are used by the PTL to notify the 
 * PML of receipt of the initial fragment of a new message (ptl_match) 
 * and to update the status of a pending receive as fragment(s) complete
 * (ptl_recv_progress).
 *
 * The first fragment of a message is sent with a header type of 
 * mca_ptl_base_match_header_t.  When a header of this type is received, 
 * the PTL should call the ptl_match() function as soon as the entire header 
 * is available, to determine if a matching receive has been posted. When a 
 * matching receive is posted the PTLs ptl_matched() function is called to 
 * process the fragment and if required generate an acknowledgment. Note that 
 * this call (ptl_matched()) may occur during the call to ptl_match() or at 
 * a later point in time if a matching recv has not yet been posted or MPI 
 * ordering constraints are not satisfied.
 *
 * Prior to calling ptl_matched(), the PML updates the recv fragment descriptor 
 * (mca_ptl_recv_frag_t) to point to the matching recv request. If the data 
 * associated with the fragment has been received prior to the ptl_matched() 
 * call, the PTL should utilize the datatype convertor associated with the 
 * recv fragment to copy the data into the users buffer. Note that the datatype 
 * convertor provides the capability to unpack the fragment at an arbitrary 
 * (e.g. fragment based) offset into the destination buffer. On completion of 
 * the data copy, the PTL should call the ptl_recv_progress() function, to update 
 * the request completion status.  
 *
 * If the initial fragment is matched prior to receiving any data associated
 * with the fragment, or in the case of subsequent fragments, the datatype 
 * convertor may be used to generate an iovec array of contiguous blocks 
 * pointing into the destination buffer, which can be used for zero-copy
 * receives if the underlying transport supports scatter/gather operations.
 *
 * The ptl_matched() function should additionally generate, if required, an 
 * ack to the source process. An ack is required if the MCA_PTL_FLAGS_ACK
 * bit is set by the source in the flags field of the initial message header.  
 * As described above, the generated ack should contain a pointer to the matched 
 * receive request, along with the pointer to the destination buffer. 
 *
 * On receipt of the ack, the source will schedule any remaining fragments. 
 * The selected PTLs should generate the remaining fragments with an 
 * mca_ptl_base_frag_header_t, which contains a placeholder for a pointer 
 * to the matched receive request. This allows the receiver to avoid calling the 
 * matching logic for subsequent fragments. On completion of these fragments,
 * the PTL should call the ptl_recv_progress() function to update the
 * request completion status. As fragments are completed, the PTL is responsible
 * for freeing any resources associated with recv fragment descriptors and/or
 * returning them to internal free lists/caches.
 *
 * Progress:
 *
 * By default, the library provides for polling based progress of outstanding
 * requests. The PTL component exports an interface function (ptlm_progress)
 * that is called in a polling mode by the PML during calls into the MPI
 * library. Note that the ptlm_progress() function is called on the PTL component
 * rather than each PTL instance. This implies that the PTL author is responsible
 * for iterating over the pending operations in each of the PTL modules associated 
 * with the component.
 * 
 * On platforms where threading support is provided, the library provides the
 * option of building with asynchronous threaded progress. In this case, the PTL 
 * author is responsible for providing a thread to progress pending operations.
 * A thread is associated with the PTL component/module such that transport specific 
 * functionality/APIs may be used to block the thread until a pending operation 
 * completes. This thread MUST NOT poll for completion as this would oversubscribe 
 * the CPU. 
 *
 * Note that in the threaded case the PML may choose to use a hybrid approach,
 * such that polling is implemented from the user thread for a fixed number of
 * cycles before relying on the background thread(s) to complete requests. If 
 * possible the PTL should support the use of both modes concurrently.
 *
 */

#ifndef MCA_PTL_H
#define MCA_PTL_H

#include "mca/mca.h"
#include "mca/pml/pml.h"
#include "include/types.h"

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
typedef struct ompi_list_t mca_ptl_queue_t;

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
 * @return            OMPI_SUCCESS or error code on failure.
 *
 * The only supported parameter is currently MCA_PTL_ENABLE,
 * which can be used by the PML to enable/disable forwarding
 * by the PTL.
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
 * Prior to unloading a PTL module, the MCA framework will call 
 * the PTL finalize method of the module. Any resources held by 
 * the PTL should be released and if required the memory corresponding
 * to the PTL module freed.
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
 * @param reachable (OUT)     Bitmask indicating set of peer processes that are reachable by this PTL.
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
typedef int (*mca_ptl_base_module_add_procs_fn_t)(
    struct mca_ptl_base_module_t* ptl, 
    size_t nprocs,
    struct ompi_proc_t** procs, 
    struct mca_ptl_base_peer_t** peer,
    struct ompi_bitmap_t* reachable
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
 * The ptl_request_fini function is called when the PML removes a request 
 * from the PTLs cache (due to resource constraints).  This routine provides 
 * the PTL the chance to cleanup/release any resources cached on the send 
 * descriptor by the PTL.
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
 * When the message exceeds the PTLs initial fragment size (ptl_first_frag_size),
 * the PML schedules the remainder of the message after an ack is received for
 * the first fragment. When the remaining fragments are scheduled the PML calls the 
 * the ptl_put() I/F function rather than ptl_send(), to indicate that the address of
 * the destination buffer at the remote process is available, allowing for an RDMA put
 * if supported by the underlying transport. 
 *
 * The PTL is responsible for updating the current data offset (req_offset) in the 
 * request to reflect the actual number of bytes fragmented.  This may be less than 
 * the requested size, due to resource constraints or datatype alighnment/offset. 
 * The PTL must call the ptl_send_progress() function to indicate completion of each
 * fragment.
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
 * PML->PTL Initiate a get from a peer. (NOT IMPLEMENTED)
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
 * The PML does NOT currently utilize this I/F.
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
 * The ptl_match() function is called by the PTL on receipt of an 
 * initial fragment of a new message.  The PML sets a default 
 * matching function on the PTL (ptl_match) when the PTL is initialized. 
 * This function attempts to match the header corresponding to the 
 * receive fragment to posted receives. When a match is made, the 
 * PTLs ptl_matched() function is called. Note that this may occur 
 * during the call to ptl_match(), or later in time if a matching receive 
 * has not yet been posted or the receive fragment is out-of-order.
 */
typedef bool (*mca_ptl_base_module_match_fn_t)(
    struct mca_ptl_base_module_t* ptl,
    struct mca_ptl_base_recv_frag_t* recv_frag,
    struct mca_ptl_base_match_header_t* header
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
 *
 * The PML sets this function pointer during module initialization 
 * to allow the PTL to make upcalls back into the PML as fragments 
 * complete.
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
 *
 * The PML sets this function pointer during module initialization 
 * to allow the PTL to make upcalls back into the PML as fragments 
 * complete.
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

    /* PTL->PML function table - filled in by PML during module init */
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
