/**
  * $HEADER$
  */
/**
  * @file
  */
#ifndef MCA_MPOOL_H
#define MCA_MPOOL_H
#include "mca/mca.h"

struct mca_mpool_t;


/**
  * allocate function typedef
  */
typedef void* (*mca_mpool_alloc_fn_t)(struct mca_mpool_t*, size_t size, size_t align);

/**
  * realloc function typedef
  */
typedef void* (*mca_mpool_realloc_fn_t)(struct mca_mpool_t*, void* addr, size_t size);

/**
  * free function typedef
  */
typedef void (*mca_mpool_free_fn_t)(struct mca_mpool_t*, void *);

/**
  * register memory
  */
typedef void (*mca_mpool_register_fn_t)(struct mca_mpool_t*, void * addr, size_t size, void* user);

/**
  * deregister memory
  */
typedef void (*mca_mpool_deregister_fn_t)(struct mca_mpool_t*, void * addr);

typedef int (*mca_mpool_base_finalize_fn_t)(
    struct mca_mpool_t* mpool 
);


struct mca_mpool_t {
    /* interface functions */
    mca_mpool_alloc_fn_t mpool_alloc;
    mca_mpool_alloc_fn_t mpool_realloc;
    mca_mpool_free_fn_t mpool_free;
    mca_mpool_register_fn_t mpool_register;
    mca_mpool_deregister_fn_t mpool_deregister;
    mca_mpool_base_finalize_fn_t mpool_finalize;
};
typedef struct mca_mpool_t mca_mpool_t;

/**
  * module initialization function
  */
typedef struct mca_mpool_t* (*mca_mpool_base_module_init_fn_t)(
    bool *allow_multi_user_threads
);

struct mca_mpool_base_module_1_0_0_t {
    mca_base_module_t mpool_version;
    mca_base_module_data_1_0_0_t mpool_data;
    mca_mpool_base_module_init_fn_t mpool_init;
};
typedef struct mca_mpool_base_module_1_0_0_t mca_mpool_base_module_t;

#endif /* MCA_MPOOL_H */

