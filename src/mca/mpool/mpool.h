/**
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
  * Description of the Memory Pool framework
  */
#ifndef MCA_MPOOL_H
#define MCA_MPOOL_H
#include "mca/mca.h"

struct mca_mpool_t;


/**
 * component initialize
 */
typedef struct mca_mpool_base_module_t* (*mca_mpool_base_component_init_fn_t)
  (bool *allow_multi_user_threads);

/**
  * if appropriate - returns base address of memory pool
  */
typedef void* (*mca_mpool_base_module_address_fn_t)(void);

/**
  * allocate function typedef
  */
typedef void* (*mca_mpool_base_module_alloc_fn_t)(size_t size, size_t align);

/**
  * allocate function typedef
  */
typedef void* (*mca_mpool_base_module_alloc_and_register_fn_t)(size_t size, size_t align, void* user);

/**
  * realloc function typedef
  */
typedef void* (*mca_mpool_base_module_realloc_fn_t)(void* addr, size_t size);

/**
  * free function typedef
  */
typedef void (*mca_mpool_base_module_free_fn_t)(void *);

/**
  * register memory
  */
typedef int (*mca_mpool_base_module_register_fn_t)(void * addr, size_t size, void* user);

/**
  * deregister memory
  */
typedef int (*mca_mpool_base_module_deregister_fn_t)(void * addr, size_t size);

/**
  * finalize
  */
typedef void (*mca_mpool_base_module_finalize_fn_t)(struct mca_mpool_base_module_t*);


/**
 * mpool component descriptor. Contains component version information
 * and open/close/init functions.
 */
struct mca_mpool_base_component_1_0_0_t {
  mca_base_component_t mpool_version;        /**< version */
  mca_base_component_data_1_0_0_t mpool_data;/**< metadata */

  mca_mpool_base_component_init_fn_t mpool_init;    /**< init function */
};
/**
 * Convenience typedef.
 */
typedef struct mca_mpool_base_component_1_0_0_t mca_mpool_base_component_1_0_0_t;
/**
  * Convenience typedef
  */
typedef struct mca_mpool_base_component_1_0_0_t mca_mpool_base_component_t;

/**
 *  mpool module descriptor. Contains the interface functions exported
 *  by the component.  This does not expose memory management
 *  details.
 */
struct mca_mpool_base_module_t {
    mca_mpool_base_component_t *mpool_component;  /**< component stuct */
    mca_mpool_base_module_address_fn_t mpool_base;       /**< returns the base address */
    mca_mpool_base_module_alloc_fn_t mpool_alloc;        /**< allocate function */
    mca_mpool_base_module_realloc_fn_t mpool_realloc;    /**< reallocate function */
    mca_mpool_base_module_free_fn_t mpool_free;          /**< free function */
    mca_mpool_base_module_register_fn_t mpool_register;  /**< register memory */
    mca_mpool_base_module_deregister_fn_t mpool_deregister; /**< deregister memory */
    mca_mpool_base_module_finalize_fn_t mpool_finalize;  /**< finalize */
};
/**
 * Convenience typedef
 */
typedef struct mca_mpool_base_module_t mca_mpool_base_module_t;

/**
 * Macro for use in components that are of type mpool v1.0.0
 */
#define MCA_MPOOL_BASE_VERSION_1_0_0 \
  /* mpool v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* ptl v1.0 */ \
  "mpool", 1, 0, 0

#endif /* MCA_MPOOL_H */

