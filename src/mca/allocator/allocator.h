/**
  * $HEADER$
  */
/**
  * @file
  */
#ifndef MCA_ALLOCATOR_H
#define MCA_ALLOCATOR_H
#include "mca/mca.h"

struct mca_allocator_t;

/**
  * allocate function typedef
  */
typedef void* (*mca_allocator_alloc_fn_t)(size_t);
 
/**
  * allocate align function typedef
  */
typedef void* (*mca_allocator_alloc_align_fn_t)(size_t, size_t);

/**
  * realloc function typedef
  */
typedef void* (*mca_allocator_realloc_fn_t)(void*, size_t);

/**
  * free function typedef
  */
typedef void(*mca_allocator_free_fn_t)(void *);


typedef int (*mca_allocator_finalize_fn_t)(
    struct mca_allocator_t* allocator 
);


struct mca_allocator_t {
    mca_allocator_alloc_fn_t alc_alloc;
    mca_allocator_alloc_align_fn_t alc_alloc_align;
    mca_allocator_realloc_fn_t alc_realloc;
    mca_allocator_free_fn_t alc_free;
    mca_allocator_finalize_fn_t alc_finalize;
};
typedef struct mca_allocator_t mca_allocator_t;


/**
  *
  */

typedef void* (*mca_allocator_segment_alloc_fn_t)(size_t* size);

/**
  *
  */
typedef void* (*mca_allocator_segment_free_fn_t)(void* segment);


/**
  * module initialization function
  */

typedef struct mca_allocator_t* (*mca_allocator_base_module_init_fn_t)(
    bool *allow_multi_user_threads,
    mca_allocator_segment_alloc_fn_t segment_alloc,
    mca_allocator_segment_free_fn_t segment_free
);

struct mca_allocator_base_module_1_0_0_t {
    mca_base_module_t allocator_version;
    mca_base_module_data_1_0_0_t allocator_data;
    mca_allocator_base_module_init_fn_t allocator_init;
};
typedef struct mca_allocator_base_module_1_0_0_t mca_allocator_base_module_t;

#endif /* MCA_ALLOCATOR_H */

