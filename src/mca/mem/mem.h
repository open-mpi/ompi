/**
  * $HEADER$
  */
/**
  * @file
  */
#ifndef MCA_MEM_H
#define MCA_MEM_H
#include "mca/mca.h"


/**
  * allocate function typedef
  */
typedef void*(*mca_mem_alloc_fn_t)(size_t);
 
/**
  * allocate align function typedef
  */
typedef void*(*mca_mem_alloc_align_fn_t)(size_t, size_t);

/**
  * free function typedef
  */
typedef void(*mca_mem_free_fn_t)(void *);

/**
  * register memory
  */
typedef void(*mca_mem_register_fn_t)(void *, size_t);

/**
  * deregister memory
  */
typedef void(*mca_mem_deregister_fn_t)(void *);

typedef int (*mca_mem_base_finalize_fn_t)(
    struct mca_mem_t* mem 
);

struct mca_mem_t {
/* interface functions */
    mca_mem_alloc_fn_t mem_alloc;
    mca_mem_alloc_align_fn_t mem_alloc_align;
    mca_mem_free_fn_t mem_free;
    mca_mem_register_fn_t mem_register;
    mca_mem_deregister_fn_t mem_deregister;
    mca_mem_base_finalize_fn_t mem_finalize;
};
typedef struct mca_mem_t mca_mem_t;

/**
  * module initialization function
  */
typedef struct mca_mem_t* (*mca_mem_base_module_init_fn_t)(
    bool *allow_multi_user_threads
);

struct mca_mem_base_module_1_0_0_t {
    mca_base_module_t mem_version;
    mca_base_module_data_1_0_0_t mem_data;
    mca_mem_base_module_init_fn_t mem_init;
};
typedef struct mca_mem_base_module_1_0_0_t mca_mem_base_module_t;

#endif /* MCA_MEM_H */

