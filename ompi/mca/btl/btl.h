/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
 * Bit Mover Interface (BTL)
 *
 *
 * BTL Initialization:
 *
 * During library initialization, all available BTL components are
 * loaded and opened via their mca_base_open_component_fn_t
 * function. The BTL open function should register any mca parameters
 * used to tune/adjust the behaviour of the BTL (mca_base_param_register_int(),
 * mca_base_param_register_string()). Note that the open function may fail
 * if the resources (e.g. shared libraries, etc) required by the network
 * transport are not available.
 *
 * The mca_btl_base_component_init_fn_t() is then called for each of the
 * components that are succesfully opened. The component init function may
 * return either:
 *
 * (1) a NULL list of BTL modules if the transport is not available,
 * (2) a list containing a single BTL module, where the BTL provides
 *     a layer of abstraction over multiple physical devices (e.g. NICs),
 * (3) a list containing multiple BTL modules where each BTL module
 *     corresponds to a single physical device.
 *
 * During module initialization, the module should post any addressing
 * information required by its peers. An example would be the TCP
 * listen port opened by the TCP module for incoming connection
 * requests. This information is published to peers via the
 * mca_pml_base_modex_send() interface. Note that peer information is not
 * guaranteed to be available via mca_pml_base_modex_recv() during the
 * module's init function. However, it will be available during
 * BTL selection (mca_btl_base_add_proc_fn_t()).
 *
 * BTL Selection:
 *
 * The upper layer builds an ordered list of the available BTL modules sorted
 * by their exclusivity ranking. This is a relative ranking that is used
 * to determine the set of BTLs that may be used to reach a given destination.
 * During startup the BTL modules are queried via their
 * mca_btl_base_add_proc_fn_t() to determine if they are able to reach
 * a given destination.  The BTL module with the highest ranking that
 * returns success is selected. Subsequent BTL modules are selected only
 * if they have the same exclusivity ranking.
 *
 * An example of how this might be used:
 *
 * BTL         Exclusivity   Comments
 * --------    -----------   ------------------
 * LO              100       Selected exclusively for local process
 * SM               50       Selected exclusively for other processes on host
 * IB                0       Selected based on network reachability
 * IB                0       Selected based on network reachability
 * TCP               0       Selected based on network reachability
 * TCP               0       Selected based on network reachability
 *
 * When a BTL module is selected, it may choose to optionally return a
 * pointer to an an mca_btl_base_endpoint_t data structure to the PML.
 * This pointer is treated as an opaque handle by the PML and is
 * returned to the BTL on subsequent data transfer calls to the
 * corresponding destination process.  The actual contents of the
 * data structure are defined on a per BTL basis, and may be used to
 * cache addressing or connection information, such as a TCP socket
 * or IB queue pair.
 *
 * Progress:
 *
 * By default, the library provides for polling based progress of outstanding
 * requests. The BTL component exports an interface function (btl_progress)
 * that is called in a polling mode by the PML during calls into the MPI
 * library. Note that the btlm_progress() function is called on the BTL component
 * rather than each BTL module. This implies that the BTL author is responsible
 * for iterating over the pending operations in each of the BTL modules associated
 * with the component.
 *
 * On platforms where threading support is provided, the library provides the
 * option of building with asynchronous threaded progress. In this case, the BTL
 * author is responsible for providing a thread to progress pending operations.
 * A thread is associated with the BTL component/module such that transport specific
 * functionality/APIs may be used to block the thread until a pending operation
 * completes. This thread MUST NOT poll for completion as this would oversubscribe
 * the CPU.
 *
 * Note that in the threaded case the PML may choose to use a hybrid approach,
 * such that polling is implemented from the user thread for a fixed number of
 * cycles before relying on the background thread(s) to complete requests. If
 * possible the BTL should support the use of both modes concurrently.
 *
 */

#include "opal/mca/mca.h"

#ifndef MCA_BTL_H
#define MCA_BTL_H

#include "ompi/types.h"
#include "ompi/mca/mpool/mpool.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * BTL types
 */

struct mca_btl_base_module_t;
struct mca_btl_base_endpoint_t;
struct mca_btl_base_descriptor_t;
struct mca_mpool_base_resources_t;
struct ompi_proc_t; 


/* send/recv operations require tag matching */
typedef uint8_t mca_btl_base_tag_t;

/* reserved tag values */
#define MCA_BTL_TAG_BTL        0
#define MCA_BTL_TAG_PML        1
#define MCA_BTL_TAG_OSC_RDMA   2
#define MCA_BTL_TAG_USR        3
#define MCA_BTL_TAG_MAX      255 /* 1 + highest allowed tag num */

/* prefered protocol */
#define MCA_BTL_FLAGS_SEND  0x1
#define MCA_BTL_FLAGS_PUT   0x2
#define MCA_BTL_FLAGS_GET   0x4
#define MCA_BTL_FLAGS_RDMA (MCA_BTL_FLAGS_GET|MCA_BTL_FLAGS_PUT)

/* btl can send directly from user buffer w/out registration */
#define MCA_BTL_FLAGS_SEND_INPLACE  0x8

/* btl transport is reliable */
#define MCA_BTL_FLAGS_NEED_ACK 0x10
#define MCA_BTL_FLAGS_NEED_CSUM 0x20

/* btl can report put/get completion before data hits the other side */
#define MCA_BTL_FLAGS_FAKE_RDMA 0x40

/* Default exclusivity levels */
#define MCA_BTL_EXCLUSIVITY_HIGH     64*1024   /* internal loopback */
#define MCA_BTL_EXCLUSIVITY_DEFAULT  1024      /* GM/IB/etc. */
#define MCA_BTL_EXCLUSIVITY_LOW      0         /* TCP used as a last resort */

/* error callback flags */
#define MCA_BTL_ERROR_FLAGS_FATAL 0x1

/**
 * Asynchronous callback function on completion of an operation.
 */

typedef void (*mca_btl_base_completion_fn_t)(
    struct mca_btl_base_module_t*,
    struct mca_btl_base_endpoint_t*,
    struct mca_btl_base_descriptor_t*,
    int status);


/**
 * Describes a region/segment of memory that is addressable 
 * by an BTL.
 */

struct mca_btl_base_segment_t {
    ompi_ptr_t seg_addr;
    uint32_t   seg_len;
#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
    uint8_t    seg_padding[4];
#endif
    union {
        uint32_t key32[2];
        uint64_t key64;
        uint8_t  key8[8];
    } seg_key;
};
typedef struct mca_btl_base_segment_t mca_btl_base_segment_t;

/**
 * A descriptor that holds the parameters to a send/put/get
 * operation along w/ a callback routine that is called on
 * completion of the request.
 */

struct mca_btl_base_descriptor_t {
    ompi_free_list_item_t super;  
    mca_btl_base_segment_t *des_src;
    size_t des_src_cnt;
    mca_btl_base_segment_t *des_dst;
    size_t des_dst_cnt;
    mca_btl_base_completion_fn_t des_cbfunc;
    void* des_cbdata;
    void* des_context; 
    int32_t des_flags;
};
typedef struct mca_btl_base_descriptor_t mca_btl_base_descriptor_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_btl_base_descriptor_t);

                                                                                                               
#define MCA_BTL_DES_FLAGS_DEREGISTER   0x0001
#define MCA_BTL_DES_FLAGS_PRIORITY 0x0002

/**
 * Maximum number of allowed segments in src/dst fields of a descriptor.
 */
#define MCA_BTL_DES_MAX_SEGMENTS 16


/* 
 *  BTL base header, stores the tag at a minimum 
 */ 
struct mca_btl_base_header_t{ 
    mca_btl_base_tag_t tag; 
}; 
typedef struct mca_btl_base_header_t mca_btl_base_header_t; 

#define MCA_BTL_BASE_HEADER_HTON(hdr)
#define MCA_BTL_BASE_HEADER_NTOH(hdr)

/*
 *  BTL component interface functions and datatype.
 */

/**
 * MCA->BTL Initializes the BTL component and creates specific BTL
 * module(s).
 *
 * @param num_btls (OUT) Returns the number of btl modules created, or 0
 *                       if the transport is not available.
 *
 * @param enable_progress_threads (IN) Whether this component is
 * allowed to run a hidden/progress thread or not.
 *
 * @param enable_mpi_threads (IN) Whether support for multiple MPI
 * threads is enabled or not (i.e., MPI_THREAD_MULTIPLE), which
 * indicates whether multiple threads may invoke this component
 * simultaneously or not.
 *
 * @return Array of pointers to BTL modules, or NULL if the transport  
 *         is not available.
 *
 * During component initialization, the BTL component should discover
 * the physical devices that are available for the given transport,
 * and create a BTL module to represent each device. Any addressing 
 * information required by peers to reach the device should be published 
 * during this function via the mca_pml_base_modex_send() interface. 
 *
 */

typedef struct mca_btl_base_module_t** (*mca_btl_base_component_init_fn_t)(
    int *num_btls, 
    bool enable_progress_threads,
    bool enable_mpi_threads
);

/**
 * MCA->BTL Called to progress outstanding requests for
 * non-threaded polling environments.
 *
 * @param tstamp     Current time.
 * @return           OMPI_SUCCESS or error code on failure.
 */

typedef int (*mca_btl_base_component_progress_fn_t)(void);


/**
 *  BTL component descriptor. Contains component version information
 *  and component open/close/init functions.
 */

struct mca_btl_base_component_1_0_1_t {
  mca_base_component_t btl_version;
  mca_base_component_data_1_0_0_t btl_data;
  mca_btl_base_component_init_fn_t btl_init;
  mca_btl_base_component_progress_fn_t btl_progress;
};
typedef struct mca_btl_base_component_1_0_1_t mca_btl_base_component_1_0_1_t;
typedef struct mca_btl_base_component_1_0_1_t mca_btl_base_component_t;

/* add the 1_0_0_t typedef for source compatability 
 *   we can do this safely because 1_0_0 components are the same as 
 *  1_0_1 components, the difference is in the btl module. 
 *  Fortunately the only difference in the module is an additional interface
 *  function added to 1_0_1. We can therefore safely treat an older module just
 *  just like the new one so long as we check the component version 
 *  prior to invoking the new interface function.
 */
typedef struct mca_btl_base_component_1_0_1_t mca_btl_base_component_1_0_0_t;



/*
 * BTL module interface functions and datatype.
 */

/**
 * MCA->BTL Clean up any resources held by BTL module 
 * before the module is unloaded.
 *  
 * @param btl (IN)   BTL module.
 *
 * Prior to unloading a BTL module, the MCA framework will call 
 * the BTL finalize method of the module. Any resources held by 
 * the BTL should be released and if required the memory corresponding
 * to the BTL module freed.
 * 
 */
typedef int (*mca_btl_base_module_finalize_fn_t)(
    struct mca_btl_base_module_t* btl
);
                                                                                                         
/**
 * PML->BTL notification of change in the process list. 
 *
 * @param btl (IN)            BTL module
 * @param nprocs (IN)         Number of processes
 * @param procs (IN)          Set of processes
 * @param endpoint (OUT)      Set of (optional) mca_btl_base_endpoint_t structures by BTL.
 * @param reachable (OUT)     Bitmask indicating set of peer processes that are reachable by this BTL.
 * @return                    OMPI_SUCCESS or error status on failure.
 *
 * The mca_btl_base_module_add_procs_fn_t() is called by the PML to 
 * determine the set of BTLs that should be used to reach each process.
 * Any addressing information exported by the peer via the mca_pml_base_modex_send()
 * function should be available during this call via the corresponding 
 * mca_pml_base_modex_recv() function. The BTL may utilize this information to 
 * determine reachability of each peer process. 
 *
 * For each process that is reachable by the BTL, the bit corresponding to the index 
 * into the proc array (nprocs) should be set in the reachable bitmask. The PML
 * provides the BTL the option to return a pointer to a data structure defined
 * by the BTL that is returned to the BTL on subsequent calls to the BTL data
 * transfer functions (e.g btl_send). This may be used by the BTL to cache any addressing 
 * or connection information (e.g. TCP socket, IP queue pair).
 */
typedef int (*mca_btl_base_module_add_procs_fn_t)(
    struct mca_btl_base_module_t* btl, 
    size_t nprocs,
    struct ompi_proc_t** procs, 
    struct mca_btl_base_endpoint_t** endpoints,
    struct ompi_bitmap_t* reachable
);

/**
 * Notification of change to the process list.
 *
 * @param btl (IN)     BTL module
 * @param nprocs (IN)  Number of processes
 * @param proc (IN)    Set of processes
 * @param peer (IN)    Set of peer addressing information.
 * @return             Status indicating if cleanup was successful
 *
 * When the process list changes, the PML notifies the BTL of the
 * change, to provide the opportunity to cleanup or release any
 * resources associated with the peer.
 */
typedef int (*mca_btl_base_module_del_procs_fn_t)(
    struct mca_btl_base_module_t* btl, 
    size_t nprocs,
    struct ompi_proc_t** procs, 
    struct mca_btl_base_endpoint_t**
);

/**
 * Callback function that is called asynchronously on receipt
 * of data by the transport layer. 
 * 
 */

typedef void (*mca_btl_base_module_recv_cb_fn_t)(
    struct mca_btl_base_module_t* btl, 
    mca_btl_base_tag_t tag,
    mca_btl_base_descriptor_t* descriptor,
    void* cbdata
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
typedef int (*mca_btl_base_module_register_fn_t)(
    struct mca_btl_base_module_t* btl, 
    mca_btl_base_tag_t tag,
    mca_btl_base_module_recv_cb_fn_t cbfunc,
    void* cbdata
);


/**
 * Callback function that is called asynchronously on receipt
 * of an error from the transport layer 
 * 
 */

typedef void (*mca_btl_base_module_error_cb_fn_t)(
        struct mca_btl_base_module_t* btl,
        int32_t flags                             
);


/**
 * Register a callback function that is called on receipt
 * of an error.
 *
 * @param btl (IN)     BTL module
 * @return             Status indicating if cleanup was successful
 *
 */
typedef int (*mca_btl_base_module_register_error_fn_t)(
    struct mca_btl_base_module_t* btl, 
    mca_btl_base_module_error_cb_fn_t cbfunc
);


/**
 * Allocate a descriptor with a segment of the requested size. 
 * Note that the BTL layer may choose to return a smaller size
 * if it cannot support the request.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */
typedef mca_btl_base_descriptor_t* (*mca_btl_base_module_alloc_fn_t)(
    struct mca_btl_base_module_t* btl,
    size_t size
);

/**
 * Return a descriptor allocated from this BTL via alloc/prepare.
 *
 * @param btl (IN)      BTL module
 * @param segment (IN)  Descriptor allocated from the BTL
 */
typedef int (*mca_btl_base_module_free_fn_t)(
    struct mca_btl_base_module_t* btl,
    mca_btl_base_descriptor_t* descriptor
);


/**
 * Prepare a descriptor for send/rdma using the supplied
 * convertor. If the convertor references data that is contigous,
 * the descriptor may simply point to the user buffer. Otherwise,
 * this routine is responsible for allocating buffer space and
 * packing if required.
 *
 * @param btl (IN)          BTL module
 * @param endpoint (IN)     BTL peer addressing
 * @param convertor (IN)    Data type convertor
 * @param reserve (IN)      Additional bytes requested by upper layer to precede user data
 * @param size (IN/OUT)     Number of bytes to prepare (IN), number of bytes actually prepared (OUT)
 */
typedef struct mca_btl_base_descriptor_t* (*mca_btl_base_module_prepare_fn_t)(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    mca_mpool_base_registration_t* registration,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size
);

/**
 * Initiate an asynchronous send.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transfered
 * @param tag (IN)         The tag value used to notify the peer.
 */
typedef int (*mca_btl_base_module_send_fn_t)(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor,
    mca_btl_base_tag_t tag
);


/**
 * Initiate an asynchronous put. 
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */

typedef int (*mca_btl_base_module_put_fn_t)(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor
);

/**
 * Initiate an asynchronous get.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 *
 */

typedef int (*mca_btl_base_module_get_fn_t)(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor
);


/**
 * Diagnostic dump of btl state.
 *
 * @param btl (IN)         BTL module
 */

typedef void (*mca_btl_base_module_dump_fn_t)(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    int verbose
);


/**
 * BTL module interface functions and attributes.
 */
struct mca_btl_base_module_t {

    /* BTL common attributes */
    mca_btl_base_component_t* btl_component; /**< pointer back to the BTL component structure */
    size_t      btl_eager_limit;      /**< maximum size of first fragment -- eager send */
    size_t      btl_min_send_size;    /**< threshold below which the BTL should not fragment */
    size_t      btl_max_send_size;    /**< maximum send fragment size supported by the BTL */
    size_t      btl_min_rdma_size;    /**< threshold below which the BTL should not fragment */
    size_t      btl_max_rdma_size;    /**< maximum rdma fragment size supported by the BTL */
    uint32_t    btl_exclusivity;      /**< indicates this BTL should be used exclusively */
    uint32_t    btl_latency;          /**< relative ranking of latency used to prioritize btls */
    uint32_t    btl_bandwidth;        /**< bandwidth (Mbytes/sec) supported by each endpoint */
    uint32_t    btl_flags;            /**< flags (put/get...) */

    /* BTL function table */
    mca_btl_base_module_add_procs_fn_t      btl_add_procs;
    mca_btl_base_module_del_procs_fn_t      btl_del_procs;
    mca_btl_base_module_register_fn_t       btl_register;
    mca_btl_base_module_finalize_fn_t       btl_finalize;

    mca_btl_base_module_alloc_fn_t       btl_alloc;
    mca_btl_base_module_free_fn_t        btl_free;
    mca_btl_base_module_prepare_fn_t     btl_prepare_src;
    mca_btl_base_module_prepare_fn_t     btl_prepare_dst;
    mca_btl_base_module_send_fn_t        btl_send;
    mca_btl_base_module_put_fn_t         btl_put;
    mca_btl_base_module_get_fn_t         btl_get;
    mca_btl_base_module_dump_fn_t        btl_dump; /* diagnostics */
   
    /* the mpool associated with this btl (optional) */ 
    mca_mpool_base_module_t*             btl_mpool; 
    /* register a default error handler */ 
    mca_btl_base_module_register_error_fn_t btl_register_error;
    
};
typedef struct mca_btl_base_module_t mca_btl_base_module_t;

/*
 * Macro for use in modules that are of type btl v1.0.1
 */
#define MCA_BTL_BASE_VERSION_1_0_1 \
  /* coll v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* btl v1.0 */ \
  "btl", 1, 0, 1


/*
 * Macro for use in modules that are of type btl v1.0.0
 *  alows older btl sources to compile..
 */
#define MCA_BTL_BASE_VERSION_1_0_0 \
  /* coll v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* btl v1.0 */ \
  "btl", 1, 0, 0

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OMPI_MCA_BTL_H */
