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
#ifndef MCA_PML_OB1_ENDPOINT_H
#define MCA_PML_OB1_ENDPOINT_H

#include "util/output.h"
#include "mca/bmi/bmi.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * A data structure associated with a ompi_proc_t that caches
 * addressing/scheduling attributes for a specific BMI instance
 * that can be used to reach the process.
 */

struct mca_pml_ob1_endpoint_t {
    int    bmi_weight;                            /**< BMI weight for scheduling */
    int    bmi_flags;                             /**< support for put/get? */
    size_t bmi_eager_limit;                       /**< BMI eager limit */
    size_t bmi_min_send_size;                     /**< BMI min send size */
    size_t bmi_max_send_size;                     /**< BMI max send size */
    size_t bmi_min_rdma_size;                     /**< BMI min rdma size */
    size_t bmi_max_rdma_size;                     /**< BMI max rdma size */
    struct mca_bmi_base_module_t *bmi;            /**< BMI module */
    struct mca_bmi_base_endpoint_t* bmi_endpoint; /**< BMI addressing info */
    struct mca_bmi_base_descriptor_t* bmi_cache;

    /* BMI function table */
    mca_bmi_base_module_alloc_fn_t   bmi_alloc;
    mca_bmi_base_module_free_fn_t    bmi_free;
    mca_bmi_base_module_send_fn_t    bmi_send;
    mca_bmi_base_module_prepare_fn_t bmi_prepare_src;
    mca_bmi_base_module_prepare_fn_t bmi_prepare_dst;
    mca_bmi_base_module_put_fn_t     bmi_put;
    mca_bmi_base_module_get_fn_t     bmi_get;
};
typedef struct mca_pml_ob1_endpoint_t mca_pml_ob1_endpoint_t;

/**
 * A dynamically growable array of mca_pml_ob1_endpoint_t instances.
 * Maintains an index into the array that is used for round-robin
 * scheduling across contents.
 */
struct mca_pml_ob1_ep_array_t {
    ompi_object_t super;
    size_t arr_size;    /**< number available */
    size_t arr_reserve; /**< size of allocated bmi_proc array */
    size_t arr_index;   /**< last used index*/
    mca_pml_ob1_endpoint_t* arr_endpoints;   /**< array of bmi endpoints */
};
typedef struct mca_pml_ob1_ep_array_t mca_pml_ob1_ep_array_t;


OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_pml_ob1_ep_array_t);


/**
 * If required, reallocate (grow) the array to the indicate size.
 * 
 * @param array (IN)
 * @param size (IN)
 */
int mca_pml_ob1_ep_array_reserve(mca_pml_ob1_ep_array_t*, size_t);

static inline size_t mca_pml_ob1_ep_array_get_size(mca_pml_ob1_ep_array_t* array)
{
    return array->arr_size;
}

/**
 * Grow the array if required, and set the size.
 * 
 * @param array (IN)
 * @param size (IN)
 */
static inline void mca_pml_ob1_ep_array_set_size(mca_pml_ob1_ep_array_t* array, size_t size)
{
    if(array->arr_size > array->arr_reserve)
        mca_pml_ob1_ep_array_reserve(array, size);
    array->arr_size = size;
}

/**
 * Grow the array size by one and return the item at that index.
 * 
 * @param array (IN)
 */
static inline mca_pml_ob1_endpoint_t* mca_pml_ob1_ep_array_insert(mca_pml_ob1_ep_array_t* array)
{
#if OMPI_ENABLE_DEBUG
    if(array->arr_size >= array->arr_reserve) {
        ompi_output(0, "mca_pml_ob1_ep_array_insert: invalid array index %d >= %d", 
            array->arr_size, array->arr_reserve);
        return 0;
    }
#endif
    return &array->arr_endpoints[array->arr_size++];
}

/**
 * Return an array item at the specified index.
 * 
 * @param array (IN)
 * @param index (IN)
 */
static inline mca_pml_ob1_endpoint_t* mca_pml_ob1_ep_array_get_index(mca_pml_ob1_ep_array_t* array, size_t index)
{
#if OMPI_ENABLE_DEBUG
    if(index >= array->arr_size) {
        ompi_output(0, "mca_pml_ob1_ep_array_get_index: invalid array index %d >= %d",
            index, array->arr_size);
        return 0;
    }
#endif
    return &array->arr_endpoints[index];
}

/**
 * Return the next LRU index in the array.
 * 
 * @param array (IN)
 * @param index (IN)
 */
static inline mca_pml_ob1_endpoint_t* mca_pml_ob1_ep_array_get_next(mca_pml_ob1_ep_array_t* array)
{
    mca_pml_ob1_endpoint_t* endpoint;
#if OMPI_ENABLE_DEBUG
    if(array->arr_size == 0) {
        ompi_output(0, "mca_pml_ob1_ep_array_get_next: invalid array size");
        return 0;
    }
#endif
    endpoint = &array->arr_endpoints[array->arr_index++];
    if(array->arr_index == array->arr_size) {
        array->arr_index = 0;
    }
    return endpoint;
}

/**
 * Locate an element in the array
 * 
 * @param array (IN)
 * @param index (IN)
 */
static inline mca_pml_ob1_endpoint_t* mca_pml_ob1_ep_array_find(
    mca_pml_ob1_ep_array_t* array, struct mca_bmi_base_module_t* bmi)
{
    size_t i=0;   
    for(i=0; i<array->arr_size; i++) {
        if(array->arr_endpoints[i].bmi == bmi) {
            return &array->arr_endpoints[i];
        }
    }
    return NULL;
}

/**
 * Allocate a descriptor for control message
 */

#if OMPI_HAVE_THREAD_SUPPORT
#define MCA_PML_OB1_ENDPOINT_DES_ALLOC(endpoint, descriptor, size)                       \
do {                                                                                     \
    if(NULL != (descriptor = endpoint->bmi_cache)) {                                     \
        /* atomically acquire the cached descriptor */                                   \
        if(ompi_atomic_cmpset_ptr(&endpoint->bmi_cache, descriptor, NULL) == 0) {        \
            endpoint->bmi_cache = NULL;                                                  \
        } else {                                                                         \
            descriptor = endpoint->bmi_alloc(endpoint->bmi, sizeof(mca_pml_ob1_hdr_t) +  \
               MCA_BMI_DES_MAX_SEGMENTS * sizeof(mca_bmi_base_segment_t));               \
        }                                                                                \
    } else {                                                                             \
            descriptor = endpoint->bmi_alloc(endpoint->bmi, sizeof(mca_pml_ob1_hdr_t) +  \
                MCA_BMI_DES_MAX_SEGMENTS * sizeof(mca_bmi_base_segment_t));              \
    }                                                                                    \
    descriptor->des_src->seg_len = size;                                                 \
} while(0)
#else
#define MCA_PML_OB1_ENDPOINT_DES_ALLOC(endpoint, descriptor, size)                       \
do {                                                                                     \
    if(NULL != (descriptor = endpoint->bmi_cache)) {                                     \
        endpoint->bmi_cache = NULL;                                                      \
    } else {                                                                             \
        descriptor = endpoint->bmi_alloc(endpoint->bmi, sizeof(mca_pml_ob1_hdr_t) +      \
            MCA_BMI_DES_MAX_SEGMENTS * sizeof(mca_bmi_base_segment_t));                  \
    }                                                                                    \
    descriptor->des_src->seg_len = size;                                                 \
} while(0)
#endif

/**
 * Return a descriptor
 */

#if OMPI_HAVE_THREAD_SUPPORT
#define MCA_PML_OB1_ENDPOINT_DES_RETURN(endpoint, descriptor)                            \
do {                                                                                     \
    if(NULL == endpoint->bmi_cache) {                                                    \
        if(ompi_atomic_cmpset_ptr(&endpoint->bmi_cache,NULL,descriptor) == 0) {          \
             endpoint->bmi_free(endpoint->bmi,descriptor);                               \
        }                                                                                \
    } else {                                                                             \
        endpoint->bmi_free(endpoint->bmi,descriptor);                                    \
    }
} while(0)
#else
#define MCA_PML_OB1_ENDPOINT_DES_RETURN(endpoint, descriptor)                            \
do {                                                                                     \
    if(NULL == endpoint->bmi_cache) {                                                    \
        endpoint->bmi_cache = descriptor;                                                \
    } else {                                                                             \
        endpoint->bmi_free(endpoint->bmi,descriptor);                                    \
    }                                                                                    \
} while(0)
#endif

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

