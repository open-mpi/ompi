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
 * Bit Mover Interface (BMI)
 *
 *
 * BMI Initialization:
 *
 * During library initialization, all available BMI components are
 * loaded and opened via their mca_base_open_component_fn_t
 * function. The BMI open function should register any mca parameters
 * used to tune/adjust the behaviour of the BMI (mca_base_param_register_int(),
 * mca_base_param_register_string()). Note that the open function may fail
 * if the resources (e.g. shared libraries, etc) required by the network
 * transport are not available.
 *
 * The mca_bmi_base_component_init_fn_t() is then called for each of the
 * components that are succesfully opened. The component init function may
 * return either:
 *
 * (1) a NULL list of BMI modules if the transport is not available,
 * (2) a list containing a single BMI module, where the BMI provides
 *     a layer of abstraction over multiple physical devices (e.g. NICs),
 * (3) a list containing multiple BMI modules where each BMI module
 *     corresponds to a single physical device.
 * 
 * During module initialization, the module should post any addressing
 * information required by its peers. An example would be the TCP
 * listen port opened by the TCP module for incoming connection
 * requests. This information is published to peers via the
 * mca_base_modex_send() interface. Note that peer information is not
 * guaranteed to be available via mca_base_modex_recv() during the 
 * module's init function. However, it will be available during 
 * BMI selection (mca_bmi_base_add_proc_fn_t()).
 *
 * BMI Selection:
 *
 * The upper layer builds an ordered list of the available BMI modules sorted 
 * by their exclusivity ranking. This is a relative ranking that is used
 * to determine the set of BMIs that may be used to reach a given destination.  
 * During startup the BMI modules are queried via their 
 * mca_bmi_base_add_proc_fn_t() to determine if they are able to reach
 * a given destination.  The BMI module with the highest ranking that 
 * returns success is selected. Subsequent BMI modules are selected only 
 * if they have the same exclusivity ranking.
 * 
 * An example of how this might be used:
 *
 * BMI         Exclusivity   Comments
 * --------    -----------   ------------------
 * LO              100       Selected exclusively for local process
 * SM               50       Selected exclusively for other processes on host
 * IB                0       Selected based on network reachability
 * IB                0       Selected based on network reachability
 * TCP               0       Selected based on network reachability
 * TCP               0       Selected based on network reachability
 *
 * When a BMI module is selected, it may choose to optionally return a
 * pointer to an an mca_bmi_base_endpoint_t data structure to the PML. 
 * This pointer is treated as an opaque handle by the PML and is
 * returned to the BMI on subsequent data transfer calls to the 
 * corresponding destination process.  The actual contents of the  
 * data structure are defined on a per BMI basis, and may be used to 
 * cache addressing or connection information, such as a TCP socket 
 * or IB queue pair.
 *
 * Progress:
 *
 * By default, the library provides for polling based progress of outstanding
 * requests. The BMI component exports an interface function (bmim_progress)
 * that is called in a polling mode by the PML during calls into the MPI
 * library. Note that the bmim_progress() function is called on the BMI component
 * rather than each BMI module. This implies that the BMI author is responsible
 * for iterating over the pending operations in each of the BMI modules associated 
 * with the component.
 * 
 * On platforms where threading support is provided, the library provides the
 * option of building with asynchronous threaded progress. In this case, the BMI 
 * author is responsible for providing a thread to progress pending operations.
 * A thread is associated with the BMI component/module such that transport specific 
 * functionality/APIs may be used to block the thread ubmil a pending operation 
 * completes. This thread MUST NOT poll for completion as this would oversubscribe 
 * the CPU. 
 *
 * Note that in the threaded case the PML may choose to use a hybrid approach,
 * such that polling is implemented from the user thread for a fixed number of
 * cycles before relying on the background thread(s) to complete requests. If 
 * possible the BMI should support the use of both modes concurrebmiy.
 *
 */

#include "mca/mca.h"

#ifndef MCA_BMI_H
#define MCA_BMI_H

#include "include/types.h"
#include "class/ompi_free_list.h"

/*
 * BMI types
 */

struct mca_bmi_base_module_t;
struct mca_bmi_base_endpoint_t;
struct mca_bmi_base_descriptor_t;


/* send/recv operations require tag matching */
typedef uint8_t mca_bmi_base_tag_t;

/* reserved tag values */
#define MCA_BMI_TAG_BMI  0
#define MCA_BMI_TAG_PML  1
#define MCA_BMI_TAG_USR  2

/* prefered protocol */
#define MCA_BMI_FLAGS_SEND  1
#define MCA_BMI_FLAGS_RDMA  2


typedef void (*mca_bmi_base_completion_fn_t)(
    struct mca_bmi_base_module_t*,
    struct mca_bmi_base_endpoint_t*,
    struct mca_bmi_base_descriptor_t*,
    int status);


/**
 * Describes a region/segment of memory that is addressable 
 * by an BMI.
 */

struct mca_bmi_base_segment_t {
    ompi_ptr_t seg_addr;
    uint32_t   seg_len;
    union {
        uint32_t key32[2];
        uint64_t key64;
        uint8_t  key8[8];
    } seg_key;
};
typedef struct mca_bmi_base_segment_t mca_bmi_base_segment_t;

/**
 * A descriptor that holds the parameters to a send/put/get
 * operation along w/ a callback routine that is called on
 * completion of the request.
 */

struct mca_bmi_base_descriptor_t {
    ompi_free_list_item_t super;  
    mca_bmi_base_segment_t *des_src;
    size_t des_src_cnt;
    mca_bmi_base_segment_t *des_dst;
    size_t des_dst_cnt;
    mca_bmi_base_completion_fn_t des_cbfunc;
    void* des_cbdata;
    int32_t des_flags;
    ompi_ptr_t user_data; 
};
typedef struct mca_bmi_base_descriptor_t mca_bmi_base_descriptor_t;

OBJ_CLASS_DECLARATION(mca_bmi_base_descriptor_t);

                                                                                                               
#define MCA_BMI_DES_FLAGS_PINNED   0x0001
#define MCA_BMI_DES_FLAGS_PRIORITY 0x0002

/**
 * Maximum number of allowed segments
 */
#define MCA_BMI_DES_MAX_SEGMENTS 16


/* 
 *  BMI base header, stores the tag at a minimum 
 */ 
struct mca_bmi_base_header_t{ 
    mca_bmi_base_tag_t tag; 
}; typedef struct mca_bmi_base_header_t mca_bmi_base_header_t; 

/*
 *  BMI component interface functions and datatype.
 */

/**
 * MCA->BMI Ibmializes the BMI component and creates specific BMI
 * module(s).
 *
 * @param num_bmis (OUT) Returns the number of bmi modules created, or 0
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
 * @return Array of pointers to BMI modules, or NULL if the transport  
 *         is not available.
 *
 * During component initialization, the BMI component should discover
 * the physical devices that are available for the given transport,
 * and create a BMI module to represent each device. Any addressing 
 * information required by peers to reach the device should be published 
 * during this function via the mca_base_modex_send() interface. 
 *
 */

typedef struct mca_bmi_base_module_t** (*mca_bmi_base_component_init_fn_t)(
    int *num_bmis, 
    bool enable_progress_threads,
    bool enable_mpi_threads
);

/**
 * MCA->BMI Called to progress outstanding requests for
 * non-threaded polling environments.
 *
 * @param tstamp     Current time.
 * @return           OMPI_SUCCESS or error code on failure.
 */

typedef int (*mca_bmi_base_component_progress_fn_t)(void);


/**
 *  BMI component descriptor. Contains component version information
 *  and component open/close/init functions.
 */

struct mca_bmi_base_component_1_0_0_t {
  mca_base_component_t bmi_version;
  mca_base_component_data_1_0_0_t bmi_data;
  mca_bmi_base_component_init_fn_t bmi_init;
  mca_bmi_base_component_progress_fn_t bmi_progress;
};
typedef struct mca_bmi_base_component_1_0_0_t mca_bmi_base_component_1_0_0_t;
typedef struct mca_bmi_base_component_1_0_0_t mca_bmi_base_component_t;


/*
 * BMI module interface functions and datatype.
 */

/**
 * MCA->BMI Clean up any resources held by BMI module 
 * before the module is unloaded.
 *  
 * @param bmi (IN)   BMI module.
 *
 * Prior to unloading a BMI module, the MCA framework will call 
 * the BMI finalize method of the module. Any resources held by 
 * the BMI should be released and if required the memory corresponding
 * to the BMI module freed.
 * 
 */
typedef int (*mca_bmi_base_module_finalize_fn_t)(
    struct mca_bmi_base_module_t* bmi
);
                                                                                                         
/**
 * PML->BMI notification of change in the process list. 
 *
 * @param bmi (IN)            BMI module
 * @param nprocs (IN)         Number of processes
 * @param procs (IN)          Set of processes
 * @param endpoint (OUT)      Set of (optional) mca_bmi_base_endpoint_t structures by BMI.
 * @param reachable (OUT)     Bitmask indicating set of peer processes that are reachable by this BMI.
 * @return                    OMPI_SUCCESS or error status on failure.
 *
 * The mca_bmi_base_module_add_procs_fn_t() is called by the PML to 
 * determine the set of BMIs that should be used to reach each process.
 * Any addressing information exported by the peer via the mca_base_modex_send()
 * function should be available during this call via the corresponding 
 * mca_base_modex_recv() function. The BMI may utilize this information to 
 * determine reachability of each peer process. 
 *
 * For each process that is reachable by the BMI, the bit corresponding to the index 
 * into the proc array (nprocs) should be set in the reachable bitmask. The PML
 * provides the BMI the option to return a pointer to a data structure defined
 * by the BMI that is returned to the BMI on subsequent calls to the BMI data
 * transfer functions (e.g bmi_send). This may be used by the BMI to cache any addressing 
 * or connection information (e.g. TCP socket, IP queue pair).
 */
typedef int (*mca_bmi_base_module_add_procs_fn_t)(
    struct mca_bmi_base_module_t* bmi, 
    size_t nprocs,
    struct ompi_proc_t** procs, 
    struct mca_bmi_base_endpoint_t** endpoints,
    struct ompi_bitmap_t* reachable
);

/**
 * Notification of change to the process list.
 *
 * @param bmi (IN)     BMI module
 * @param nprocs (IN)  Number of processes
 * @param proc (IN)    Set of processes
 * @param peer (IN)    Set of peer addressing information.
 * @return             Status indicating if cleanup was successful
 *
 * When the process list changes, the PML notifies the BMI of the
 * change, to provide the opportunity to cleanup or release any
 * resources associated with the peer.
 */
typedef int (*mca_bmi_base_module_del_procs_fn_t)(
    struct mca_bmi_base_module_t* bmi, 
    size_t nprocs,
    struct ompi_proc_t** procs, 
    struct mca_bmi_base_endpoint_t**
);

/**
 * Callback function that is called asynchronously on receipt
 * of data from the transport layer.
 */

typedef void (*mca_bmi_base_module_recv_cb_fn_t)(
    struct mca_bmi_base_module_t* bmi, 
    mca_bmi_base_tag_t tag,
    mca_bmi_base_descriptor_t* descriptor,
    void* cbdata
);


/* holds the recv call back function to be called by the bmi on 
 * a receive. 
 */ 
struct mca_bmi_base_registration_t {  
    mca_bmi_base_module_recv_cb_fn_t cbfunc; 
    void* cbdata; 
}; 
typedef struct mca_bmi_base_registration_t mca_bmi_base_registration_t; 


/**
 * Register a callback function that is called on receipt
 * of a fragment.
 *
 * @param bmi (IN)     BMI module
 * @return             Status indicating if cleanup was successful
 *
 * When the process list changes, the PML notifies the BMI of the
 * change, to provide the opportunity to cleanup or release any
 * resources associated with the peer.
 */
typedef int (*mca_bmi_base_module_register_fn_t)(
    struct mca_bmi_base_module_t* bmi, 
    mca_bmi_base_tag_t tag,
    mca_bmi_base_module_recv_cb_fn_t cbfunc,
    void* cbdata
);


/**
 * Allocate a segment.
 *
 * @param bmi (IN)      BMI module
 * @param size (IN)     Request segment size.
 */
typedef mca_bmi_base_descriptor_t* (*mca_bmi_base_module_alloc_fn_t)(
    struct mca_bmi_base_module_t* bmi,
    size_t size
);

/**
 * Return a segment allocated by this BMI.
 *
 * @param bmi (IN)      BMI module
 * @param segment (IN)  Allocated segment.
 */
typedef int (*mca_bmi_base_module_free_fn_t)(
    struct mca_bmi_base_module_t* bmi,
    mca_bmi_base_descriptor_t* descriptor
);


/**
 * Pack data and return a descriptor that can be
 * used for send/put.
 *
 * @param bmi (IN)      BMI module
 * @param peer (IN)     BMI peer addressing
 */
typedef struct mca_bmi_base_descriptor_t* (*mca_bmi_base_module_prepare_fn_t)(
    struct mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* endpoint,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size
);

/**
 * Initiate a send to the peer.
 *
 * @param bmi (IN)      BMI module
 * @param peer (IN)     BMI peer addressing
 */
typedef int (*mca_bmi_base_module_send_fn_t)(
    struct mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* endpoint,
    struct mca_bmi_base_descriptor_t* descriptor,
    mca_bmi_base_tag_t tag
);


/**
 * Initiate a put to the peer.
 *
 * @param bmi (IN)      BMI module
 * @param peer (IN)     BMI peer addressing
 */

typedef int (*mca_bmi_base_module_put_fn_t)(
    struct mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* endpoint,
    struct mca_bmi_base_descriptor_t* descriptor
);

/**
 * PML->BMI Initiate a get from a peer. 
 *
 * @param bmi (IN)      BMI module
 * @param peer (IN)     BMI peer addressing
 *
 */

typedef int (*mca_bmi_base_module_get_fn_t)(
    struct mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* endpoint,
    struct mca_bmi_base_descriptor_t* descriptor
);


/**
 * BMI module interface functions and attributes.
 */
struct mca_bmi_base_module_t {

    /* BMI common attributes */
    mca_bmi_base_component_t* bmi_component; /**< pointer back to the BMI component structure */
    size_t      bmi_eager_limit;      /**< maximum size of first fragment -- eager send */
    size_t      bmi_min_send_size;    /**< threshold below which the BMI should not fragment */
    size_t      bmi_max_send_size;    /**< maximum send fragment size supported by the BMI */
    size_t      bmi_min_rdma_size;    /**< threshold below which the BMI should not fragment */
    size_t      bmi_max_rdma_size;    /**< maximum rdma fragment size supported by the BMI */
    uint32_t    bmi_exclusivity;      /**< indicates this BMI should be used exclusively */
    uint32_t    bmi_latency;          /**< relative ranking of latency used to prioritize bmis */
    uint32_t    bmi_bandwidth;        /**< bandwidth (Mbytes/sec) supported by each endpoint */
    uint32_t    bmi_flags;            /**< flags (put/get...) */

    /* BMI function table */
    mca_bmi_base_module_add_procs_fn_t   bmi_add_procs;
    mca_bmi_base_module_del_procs_fn_t   bmi_del_procs;
    mca_bmi_base_module_register_fn_t    bmi_register;
    mca_bmi_base_module_finalize_fn_t    bmi_finalize;

    mca_bmi_base_module_alloc_fn_t       bmi_alloc;
    mca_bmi_base_module_free_fn_t        bmi_free;
    mca_bmi_base_module_prepare_fn_t     bmi_prepare_src;
    mca_bmi_base_module_prepare_fn_t     bmi_prepare_dst;
    mca_bmi_base_module_send_fn_t        bmi_send;
    mca_bmi_base_module_put_fn_t         bmi_put;
    mca_bmi_base_module_get_fn_t         bmi_get;
};
typedef struct mca_bmi_base_module_t mca_bmi_base_module_t;

/*
 * Macro for use in modules that are of type bmi v1.0.0
 */
#define MCA_BMI_BASE_VERSION_1_0_0 \
  /* coll v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* bmi v1.0 */ \
  "bmi", 1, 0, 0

#endif /* OMPI_MCA_BMI_H */
