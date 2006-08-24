/**
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
  * The public definition of the MCA Allocator framework.
  */
#ifndef MCA_ALLOCATOR_H
#define MCA_ALLOCATOR_H
#include "opal/mca/mca.h"
#include "ompi/mca/mpool/mpool.h" 

struct mca_mpool_base_resources_t;


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
/* Here so that we can use mca_allocator_base_module_t in the function typedefs */
struct mca_allocator_base_module_t;

/**
  * The allocate function typedef for the function to be provided by the component.
  */
typedef void* (*mca_allocator_base_module_alloc_fn_t)(
    struct mca_allocator_base_module_t*, 
    size_t size, 
    size_t align, 
    mca_mpool_base_registration_t** registration);
 
/**
  * The realloc function typedef
  */
typedef void* (*mca_allocator_base_module_realloc_fn_t)(
    struct mca_allocator_base_module_t*, 
    void*, size_t, 
    mca_mpool_base_registration_t** registration);

/**
  * Free function typedef
  */
typedef void(*mca_allocator_base_module_free_fn_t)(
    struct mca_allocator_base_module_t*, void *);


/**
 * compact/return memory to higher level allocator
 */

typedef int (*mca_allocator_base_module_compact_fn_t)(
    struct mca_allocator_base_module_t* allocator 
);
 

/**
 * cleanup (free) any resources held by allocator
 */

typedef int (*mca_allocator_base_module_finalize_fn_t)(
    struct mca_allocator_base_module_t* allocator 
);

/**
 * The data structure for each component.
 */
struct mca_allocator_base_module_t {
    mca_allocator_base_module_alloc_fn_t alc_alloc;
    /**< Allocate memory */
    mca_allocator_base_module_realloc_fn_t alc_realloc; 
    /**< Reallocate memory */
    mca_allocator_base_module_free_fn_t alc_free;       
    /**< Free memory */
    mca_allocator_base_module_compact_fn_t alc_compact;   
    /**< Return memory */
    mca_allocator_base_module_finalize_fn_t alc_finalize; 
    /**< Finalize and free everything */
    /* memory pool and resources */
    struct mca_mpool_base_module_t* alc_mpool;
};
/**
 * Convenience typedef.
 */
typedef struct mca_allocator_base_module_t mca_allocator_base_module_t;


/**
  * A function to get more memory from the system. This function is to be
  * provided by the module to the allocator framework.
  */

typedef void* (*mca_allocator_base_component_segment_alloc_fn_t)(
    struct mca_mpool_base_module_t* module,
    size_t* size, 
    mca_mpool_base_registration_t** registration);

/**
  * A function to free memory from the control of the allocator framework 
  * back to the system. This function is to be provided by the module to the
  * allocator frmaework.
  */
typedef void* (*mca_allocator_base_component_segment_free_fn_t)(
    struct mca_mpool_base_module_t* module,
    void* segment);


/**
  * The function used to initialize the component. 
  */
typedef struct mca_allocator_base_module_t* 
    (*mca_allocator_base_component_init_fn_t)(
    bool enable_mpi_threads,
    mca_allocator_base_component_segment_alloc_fn_t segment_alloc,
    mca_allocator_base_component_segment_free_fn_t segment_free, 
    struct mca_mpool_base_module_t* mpool
);

/**
 * The data structure provided by each component to the framework which
 * describes the component.
 */
struct mca_allocator_base_component_1_0_0_t {
    mca_base_component_t allocator_version; 
    /**< The version of the component */
    mca_base_component_data_1_0_0_t allocator_data; 
    /**< The component metadata */
    mca_allocator_base_component_init_fn_t allocator_init; 
    /**< The component initialization function. */
};

/**
 * Convenience typedef.
 */
typedef struct mca_allocator_base_component_1_0_0_t mca_allocator_base_component_t;

/**
 * Macro for use in components that are of type allocator v1.0.0
 */
#define MCA_ALLOCATOR_BASE_VERSION_1_0_0 \
  /* allocator v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* allocator v1.0 */ \
  "allocator", 1, 0, 0

/**
 * The output integer used for the mca base
 */
OMPI_DECLSPEC extern int mca_allocator_base_output;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_ALLOCATOR_H */

