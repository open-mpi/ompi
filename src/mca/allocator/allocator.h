/**
  * $HEADER$
  */
/**
  * @file 
  * The public definition of the MCA Allocator framework.
  */
#ifndef MCA_ALLOCATOR_H
#define MCA_ALLOCATOR_H
#include "mca/mca.h"

/* Here so that we can use mca_allocator_t in the function typedefs */
struct mca_allocator_t;

/**
  * The allocate function typedef for the functrion to be provided by the component.
  */
typedef void* (*mca_allocator_alloc_fn_t)(struct mca_allocator_t*, size_t size, size_t align);
 
/**
  * The realloc function typedef
  */
typedef void* (*mca_allocator_realloc_fn_t)(struct mca_allocator_t*, void*, size_t);

/**
  * Free function typedef
  */
typedef void(*mca_allocator_free_fn_t)(struct mca_allocator_t*, void *);


/**
 * compact/return memory to higher level allocator
 */

typedef int (*mca_allocator_return_fn_t)(
    struct mca_allocator_t* allocator 
);
 

/**
 * cleanup (free) any resources held by allocator
 */

typedef int (*mca_allocator_finalize_fn_t)(
    struct mca_allocator_t* allocator 
);

/**
 * The data structure for each component.
 */
struct mca_allocator_t {
    mca_allocator_alloc_fn_t alc_alloc;     /**< Allocate memory */
    mca_allocator_realloc_fn_t alc_realloc; /**< Reallocate memory */
    mca_allocator_free_fn_t alc_free;       /**< Free memory */
    mca_allocator_return_fn_t alc_return;   /**< Return memory */
    mca_allocator_finalize_fn_t alc_finalize; /**< Finalize and free everything */
};
/**
 * Convenience typedef.
 */
typedef struct mca_allocator_t mca_allocator_t;


/**
  * A function to get more memory from the system. This function is to be
  * provided by the module to the allocator framework.
  */

typedef void* (*mca_allocator_segment_alloc_fn_t)(size_t* size);

/**
  * A function to free memory from the control of the allocator framework 
  * back to the system. This function is to be provided by the module to the
  * allocator frmaework.
  */
typedef void* (*mca_allocator_segment_free_fn_t)(void* segment);


/**
  * The function used to initialize the module. 
  */
typedef struct mca_allocator_t* (*mca_allocator_base_module_init_fn_t)(
    bool *allow_multi_user_threads,
    mca_allocator_segment_alloc_fn_t segment_alloc,
    mca_allocator_segment_free_fn_t segment_free
);

/**
 * The data structure provided by each component to the framework which
 * describes the component.
 */
struct mca_allocator_base_module_1_0_0_t {
    mca_base_module_t allocator_version; /**< The version of the module */
    mca_base_module_data_1_0_0_t allocator_data; /**< The module metadata */
    mca_allocator_base_module_init_fn_t allocator_init; 
    /**< The module initialization function. */
};
/**
 * Convenience typedef.
 */
typedef struct mca_allocator_base_module_1_0_0_t mca_allocator_base_module_t;

/**
 * Macro for use in modules that are of type allocator v1.0.0
 */
#define MCA_ALLOCATOR_BASE_VERSION_1_0_0 \
  /* mpool v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* ptl v1.0 */ \
  "allocator", 1, 0, 0

/**
 * The output integer used for the mca base
 */
extern int mca_allocator_base_output;

#endif /* MCA_ALLOCATOR_H */

