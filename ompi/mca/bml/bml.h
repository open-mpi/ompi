/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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
 * BML Management Layer (BML)
 *
 */

#ifndef MCA_BML_H
#define MCA_BML_H

#include "mca/mca.h"
#include "mca/btl/btl.h"

#include "mca/bml/base/bml_base_btl.h"
#include "mca/bml/base/bml_base_endpoint.h" 

#include "include/types.h"
#include "class/ompi_free_list.h"
#include "mca/pml/pml.h"

/*
 * BML types
 */

struct ompi_proc_t; 
struct mca_bml_base_module_t;
struct mca_bml_base_endpoint_t;
struct mca_mpool_base_resources_t;

/*
 * Cached set of information for each btl 
 */

struct mca_bml_base_btl_t {
    int    btl_index;                             /**< index in endpoint array */
    int    btl_flags;                             /**< support for put/get? */
    double btl_weight;                            /**< BTL weight for scheduling */
    size_t btl_eager_limit;                       /**< BTL eager limit */
    size_t btl_min_send_size;                     /**< BTL min send size */
    size_t btl_max_send_size;                     /**< BTL max send size */
    size_t btl_min_rdma_size;                     /**< BTL min rdma size */
    size_t btl_max_rdma_size;                     /**< BTL max rdma size */
    struct mca_btl_base_module_t *btl;            /**< BTL module */
    struct mca_btl_base_endpoint_t* btl_endpoint; /**< BTL addressing info */
    struct mca_btl_base_descriptor_t* btl_cache;
        
    /* BTL function table */
    mca_btl_base_module_alloc_fn_t   btl_alloc;
    mca_btl_base_module_free_fn_t    btl_free;
    mca_btl_base_module_send_fn_t    btl_send;
    mca_btl_base_module_prepare_fn_t btl_prepare_src;
    mca_btl_base_module_prepare_fn_t btl_prepare_dst;
    mca_btl_base_module_put_fn_t     btl_put;
    mca_btl_base_module_get_fn_t     btl_get;
    mca_btl_base_component_progress_fn_t btl_progress; 
    
    mca_mpool_base_module_t*         btl_mpool; 
    
};
typedef struct mca_bml_base_btl_t mca_bml_base_btl_t;



/**
 * A dynamically growable array of mca_bml_base_btl_t instances.
 * Maintains an index into the array that is used for round-robin
 * scheduling across contents.
 */
struct mca_bml_base_btl_array_t {
    opal_object_t super;
    size_t arr_size;                     /**< number available */
    size_t arr_reserve;                  /**< size of allocated btl_proc array */
    size_t arr_index;                    /**< last used index*/
    mca_bml_base_btl_t* bml_btls;   /**< array of  bml btl's */
};
typedef struct mca_bml_base_btl_array_t mca_bml_base_btl_array_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_bml_base_btl_array_t);


/**
 * If required, reallocate (grow) the array to the indicate size.
 * 
 * @param array (IN)
 * @param size (IN)
 */
int mca_bml_base_btl_array_reserve(mca_bml_base_btl_array_t*, size_t);

static inline size_t mca_bml_base_btl_array_get_size(mca_bml_base_btl_array_t* array)
{
    return array->arr_size;
}

/**
 * Grow the array if required, and set the size.
 * 
 * @param array (IN)
 * @param size (IN)
 */
static inline void mca_bml_base_btl_array_set_size(mca_bml_base_btl_array_t* array, size_t size)
{
    if(array->arr_size > array->arr_reserve)
        mca_bml_base_btl_array_reserve(array, size);
    array->arr_size = size;
}

/**
 * Grow the array size by one and return the item at that index.
 * 
 * @param array (IN)
 */
static inline mca_bml_base_btl_t* mca_bml_base_btl_array_insert(mca_bml_base_btl_array_t* array)
{
#if OMPI_ENABLE_DEBUG
    if(array->arr_size >= array->arr_reserve) {
        opal_output(0, "mca_bml_base_btl_array_insert: invalid array index %d >= %d", 
            array->arr_size, array->arr_reserve);
        return 0;
    }
#endif
    return &array->bml_btls[array->arr_size++];
}

/**
 * Return an array item at the specified index.
 * 
 * @param array (IN)
 * @param index (IN)
 */
static inline mca_bml_base_btl_t* mca_bml_base_btl_array_get_index(mca_bml_base_btl_array_t* array, size_t index)
{
#if OMPI_ENABLE_DEBUG
    if(index >= array->arr_size) {
        opal_output(0, "mca_bml_base_btl_array_get_index: invalid array index %d >= %d",
            index, array->arr_size);
        return 0;
    }
#endif
    return &array->bml_btls[index];
}

/**
 * Return the next LRU index in the array.
 * 
 * @param array (IN)
 * @param index (IN)
 */
static inline mca_bml_base_btl_t* mca_bml_base_btl_array_get_next(mca_bml_base_btl_array_t* array)
{
    mca_bml_base_btl_t* bml_btl;
#if OMPI_ENABLE_DEBUG
    if(array->arr_size == 0) {
        opal_output(0, "mca_bml_base_btl_array_get_next: invalid array size");
        return 0;
    }
#endif
    bml_btl = &array->bml_btls[array->arr_index++];
    if(array->arr_index == array->arr_size) {
        array->arr_index = 0;
    }
    return bml_btl;
}

/**
 * Locate an element in the array
 * 
 * @param array (IN)
 * @param index (IN)
 */
static inline mca_bml_base_btl_t* mca_bml_base_btl_array_find(
    mca_bml_base_btl_array_t* array, struct mca_btl_base_module_t* btl)
{
    size_t i=0;   
    for(i=0; i<array->arr_size; i++) {
        if(array->bml_btls[i].btl == btl) {
            return &array->bml_btls[i];
        }
    }
    return NULL;
}



/**
 *  Structure associated w/ ompi_proc_t that contains the set
 *  of BTLs used to reach a destinationation
 */
struct mca_bml_base_endpoint_t {
    mca_pml_proc_t           super;
    struct ompi_proc_t       *btl_proc;         /**< back-pointer to ompi_proc_t */
    opal_mutex_t             btl_lock;          /**< lock to protect against concurrent access */
    int                      btl_flags;         /**< prefered method of accessing this peer */
    size_t                   btl_rdma_offset;   /**< max of min rdma size for available rmda btls */
    size_t                   btl_max_send_size; /**< min of max send size for available send btls */
    mca_bml_base_btl_array_t btl_eager;         /**< array of btls to use for first fragments */
    mca_bml_base_btl_array_t btl_send;          /**< array of btls to use for remaining fragments */
    mca_bml_base_btl_array_t btl_rdma;          /**< array of btls that support (prefer) rdma */
};
typedef struct mca_bml_base_endpoint_t mca_bml_base_endpoint_t;
                              
    
OMPI_COMP_EXPORT OBJ_CLASS_DECLARATION(mca_bml_base_endpoint_t);



static inline  void mca_bml_base_alloc(mca_bml_base_btl_t* bml_btl, mca_btl_base_descriptor_t** des, size_t size) { 
    *des = bml_btl->btl_alloc(bml_btl->btl, size);                                         
    if((*des) != NULL) { 
        (*des)->des_context = (void*) bml_btl;
    }
}

static inline void mca_bml_base_free(mca_bml_base_btl_t* bml_btl, mca_btl_base_descriptor_t* des) { 
    bml_btl->btl_free( bml_btl->btl, des );   
    /* The previous function is supposed to release the des object
     * so we should not touch it anymore.
     */
    /*des->des_context = NULL;*/
}

static inline int mca_bml_base_send(mca_bml_base_btl_t* bml_btl, mca_btl_base_descriptor_t* des, mca_btl_base_tag_t tag) { 
    des->des_context = (void*) bml_btl; 
    return bml_btl->btl_send(
                             bml_btl->btl,
                             bml_btl->btl_endpoint, 
                             des,
                             tag);
}

static inline int mca_bml_base_put(mca_bml_base_btl_t* bml_btl, mca_btl_base_descriptor_t* des) { 
    des->des_context = (void*) bml_btl; 
    return bml_btl->btl_put(
                             bml_btl->btl,
                             bml_btl->btl_endpoint, 
                             des);
}

static inline int mca_bml_base_get(mca_bml_base_btl_t* bml_btl, mca_btl_base_descriptor_t* des) { 
    des->des_context = (void*) bml_btl; 
    return bml_btl->btl_get(
                             bml_btl->btl,
                             bml_btl->btl_endpoint, 
                             des);
}


static inline void mca_bml_base_prepare_src(mca_bml_base_btl_t* bml_btl, 
                                            mca_mpool_base_registration_t* reg, 
                                            struct ompi_convertor_t* conv, 
                                            size_t reserve, 
                                            size_t *size, 
                                            mca_btl_base_descriptor_t** des) { 
    *des = bml_btl->btl_prepare_src(
                                   bml_btl->btl, 
                                   bml_btl->btl_endpoint, 
                                   reg, 
                                   conv,
                                   reserve,
                                   size
                                   );
    if((*des) != NULL) { 
        (*des)->des_context = (void*) bml_btl;
    }
}

static inline void mca_bml_base_prepare_dst(mca_bml_base_btl_t* bml_btl, 
                                            mca_mpool_base_registration_t* reg, 
                                            struct ompi_convertor_t* conv, 
                                            size_t reserve, 
                                            size_t *size, 
                                            mca_btl_base_descriptor_t** des) { 
    *des = bml_btl->btl_prepare_dst(
                                   bml_btl->btl, 
                                   bml_btl->btl_endpoint, 
                                   reg, 
                                   conv,
                                   reserve,
                                   size
                                   );
    if((*des) != NULL) { 
        (*des)->des_context = (void*) bml_btl;  
    }
}




#if OMPI_HAVE_THREAD_SUPPORT
#define MCA_BML_BASE_BTL_DES_ALLOC(bml_btl, des, alloc_size, seg_size)                   \
do  {                                                                                    \
     if(NULL != (des = bml_btl->btl_cache)) {                                            \
        /* atomically acquire the cached descriptor */                                   \
        if(opal_atomic_cmpset_ptr(&bml_btl->btl_cache, des, NULL) == 0) {                \
            bml_btl->btl_cache = NULL;                                                   \
        } else {                                                                         \
            des = bml_btl->btl_alloc(bml_btl->btl, alloc_size);                          \
        }                                                                                \
    } else {                                                                             \
            des = bml_btl->btl_alloc(bml_btl->btl, alloc_size);                          \
    }                                                                                    \
    des->des_src->seg_len = seg_size;                                                    \
    des->des_context = (void*) bml_btl;                                                  \
} while(0)                                                                               
#else
#define MCA_BML_BASE_BTL_DES_ALLOC(bml_btl, descriptor, alloc_size, seg_size)            \
do {                                                                                     \
    if(NULL != (descriptor = bml_btl->btl_cache)) {                                      \
        bml_btl->btl_cache = NULL;                                                       \
    } else {                                                                             \
        descriptor = bml_btl->btl_alloc(bml_btl->btl, alloc_size);                       \
    }                                                                                    \
    descriptor->des_src->seg_len = seg_size;                                             \
    descriptor->des_context = (void*) bml_btl;                                           \
} while(0)
#endif


/**
 * Return a descriptor
 */

#if OMPI_HAVE_THREAD_SUPPORT
#define MCA_BML_BASE_BTL_DES_RETURN( bml_btl, descriptor )                               \
do {                                                                                     \
    if(NULL == bml_btl->btl_cache) {                                                     \
        if(opal_atomic_cmpset_ptr(&bml_btl->btl_cache,NULL,descriptor) == 0) {           \
             bml_btl->btl_free(bml_btl->btl,descriptor);                                 \
        }                                                                                \
    } else {                                                                             \
        bml_btl->btl_free(bml_btl->btl,descriptor);                                      \
    }                                                                                    \
} while(0)
#else
#define MCA_BML_BASE_BTL_DES_RETURN(bml_btl, descriptor)                                 \
do {                                                                                     \
    if(NULL == bml_btl->btl_cache) {                                                     \
        bml_btl->btl_cache = descriptor;                                                 \
    } else {                                                                             \
        bml_btl->btl_free(bml_btl->btl,descriptor);                                      \
    }                                                                                    \
} while(0)
#endif


/*
 *  BML component interface functions and datatype.
 */

/**
 * MCA->BML Initializes the BML component and creates specific BML
 * module(s).
 *
 * @param num_bmls (OUT) Returns the number of bml modules created, or 0
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
 * @return Array of pointers to BML modules, or NULL if the transport  
 *         is not available.
 *
 * During component initialization, the BML component should discover
 * the physical devices that are available for the given transport,
 * and create a BML module to represent each device. Any addressing 
 * information required by peers to reach the device should be published 
 * during this function via the mca_base_modex_send() interface. 
 *
 */

typedef struct mca_bml_base_module_t* (*mca_bml_base_component_init_fn_t)(
                                                                          int* priority,
                                                                          bool enable_progress_threads,
                                                                          bool enable_mpi_threads
                                                                          );

/**
 * MCA->BML Called to progress outstanding requests for
 * non-threaded polling environments.
 *
 * @param tstamp     Current time.
 * @return           OMPI_SUCCESS or error code on failure.
 */

typedef int (*mca_bml_base_module_progress_fn_t)(void);


/**
 *  BML component descriptor. Contains component version information
 *  and component open/close/init functions.
 */

struct mca_bml_base_component_1_0_0_t {
  mca_base_component_t bml_version;
  mca_base_component_data_1_0_0_t bml_data;
  mca_bml_base_component_init_fn_t bml_init;
    
};
typedef struct mca_bml_base_component_1_0_0_t mca_bml_base_component_1_0_0_t;
typedef struct mca_bml_base_component_1_0_0_t mca_bml_base_component_t;


/*
 * BML module interface functions and datatype.
 */

/**
 * MCA->BML Clean up any resources held by BML module 
 * before the module is unloaded.
 *  
 * @param bml (IN)   BML module.
 *
 * Prior to unloading a BML module, the MCA framework will call 
 * the BML finalize method of the module. Any resources held by 
 * the BML should be released and if required the memory corresponding
 * to the BML module freed.
 * 
 */
typedef int (*mca_bml_base_module_finalize_fn_t)( void );
                                                                                                         
/**
 * PML->BML notification of change in the process list. 
 *
 * @param bml (IN)            BML module
 * @param nprocs (IN)         Number of processes
 * @param procs (IN)          Set of processes
 * @param endpoint (OUT)      Set of (optional) mca_bml_base_endpoint_t structures by BML.
 * @param reachable (OUT)     Bitmask indicating set of peer processes that are reachable by this BML.
 * @return                    OMPI_SUCCESS or error status on failure.
 *
 * The mca_bml_base_module_add_procs_fn_t() is called by the PML to 
 * determine the set of BMLs that should be used to reach each process.
 * Any addressing information exported by the peer via the mca_base_modex_send()
 * function should be available during this call via the corresponding 
 * mca_base_modex_recv() function. The BML may utilize this information to 
 * determine reachability of each peer process. 
 *
 * For each process that is reachable by the BML, the bit corresponding to the index 
 * into the proc array (nprocs) should be set in the reachable bitmask. The PML
 * provides the BML the option to return a pointer to a data structure defined
 * by the BML that is returned to the BML on subsequent calls to the BML data
 * transfer functions (e.g bml_send). This may be used by the BML to cache any addressing 
 * or connection information (e.g. TCP socket, IP queue pair).
 */
typedef int (*mca_bml_base_module_add_procs_fn_t)(
                                                  size_t nprocs,
                                                  struct ompi_proc_t** procs, 
                                                  struct mca_bml_base_endpoint_t** endpoints,
                                                  struct ompi_bitmap_t* reachable
                                                  );

/**
 * Notification of change to the process list.
 *
 * @param bml (IN)     BML module
 * @param nprocs (IN)  Number of processes
 * @param proc (IN)    Set of processes
 * @param peer (IN)    Set of peer addressing information.
 * @return             Status indicating if cleanup was successful
 *
 * When the process list changes, the PML notifies the BML of the
 * change, to provide the opportunity to cleanup or release any
 * resources associated with the peer.
 */
typedef int (*mca_bml_base_module_del_procs_fn_t)(
                                                  size_t nprocs,
                                                  struct ompi_proc_t** procs
                                                  );

/**
 * Callback function that is called asynchronously on receipt
 * of data by the transport layer.
 */

typedef void (*mca_bml_base_module_recv_cb_fn_t)(
                                                 mca_btl_base_module_t* bml_btl, 
                                                 mca_btl_base_tag_t tag,
                                                 mca_btl_base_descriptor_t* descriptor,
                                                 void* cbdata
                                                 );


/**
 * Register a callback function that is called on receipt
 * of a fragment.
 *
 * @param bml (IN)     BML module
 * @return             Status indicating if cleanup was successful
 *
 * When the process list changes, the PML notifies the BML of the
 * change, to provide the opportunity to cleanup or release any
 * resources associated with the peer.
 */
typedef int (*mca_bml_base_module_register_fn_t)(
                                                 mca_btl_base_tag_t tag,
                                                 mca_bml_base_module_recv_cb_fn_t cbfunc,
                                                 void* cbdata
                                                 );



/**
 * BML module interface functions and attributes.
 */
struct mca_bml_base_module_t {
    /* BML common attributes */
    mca_bml_base_component_t* bml_component; /**< pointer back to the BML component structure */
    size_t      bml_eager_limit;      /**< maximum size of first fragment -- eager send */
    size_t      bml_min_send_size;    /**< threshold below which the BML should not fragment */
    size_t      bml_max_send_size;    /**< maximum send fragment size supported by the BML */
    size_t      bml_min_rdma_size;    /**< threshold below which the BML should not fragment */
    size_t      bml_max_rdma_size;    /**< maximum rdma fragment size supported by the BML */
    
    /* BML function table */
    mca_bml_base_module_add_procs_fn_t   bml_add_procs;
    mca_bml_base_module_del_procs_fn_t   bml_del_procs;
    mca_bml_base_module_register_fn_t    bml_register;
    mca_bml_base_module_finalize_fn_t    bml_finalize;

    mca_bml_base_module_progress_fn_t bml_progress;

};
typedef struct mca_bml_base_module_t mca_bml_base_module_t;

/*
 * Macro for use in modules that are of type bml v1.0.0
 */
#define MCA_BML_BASE_VERSION_1_0_0 \
  /*  v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* bml v1.0 */ \
  "bml", 1, 0, 0

#endif /* OMPI_MCA_BML_H */
