/**
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
  * Description of the Registration Cache framework
  */
#ifndef MCA_RCACHE_H
#define MCA_RCACHE_H
#include "opal/mca/mca.h"
#include "ompi/info/info.h"
#include "opal/class/opal_list.h" 
#include "ompi/mca/mpool/mpool.h"
#include "opal/threads/mutex.h"


/**
 * component initialize
 */
typedef struct mca_rcache_base_module_t* (*mca_rcache_base_component_init_fn_t)(void);
                                                               

typedef int (*mca_rcache_base_module_find_fn_t) (
        struct mca_rcache_base_module_t* rcache, void* addr, size_t size,
        mca_mpool_base_registration_t **reg);

typedef int (*mca_rcache_base_module_find_all_fn_t)(
        struct mca_rcache_base_module_t* rcache, void* addr, size_t size,
        ompi_pointer_array_t *regs);

typedef int (*mca_rcache_base_module_insert_fn_t)(
        struct mca_rcache_base_module_t* rcache,
        mca_mpool_base_registration_t* registration, size_t limit);

typedef int (*mca_rcache_base_module_delete_fn_t)(
        struct mca_rcache_base_module_t* rcache,
        mca_mpool_base_registration_t* registration);

/**
  * finalize
  */
typedef void (*mca_rcache_base_module_finalize_fn_t)(
        struct mca_rcache_base_module_t*);

/** 
 * rcache component descriptor. Contains component version information and 
 * open/close/init functions 
 */ 

struct mca_rcache_base_component_1_0_0_t{ 
    mca_base_component_t rcache_version;      /**< version */ 
    mca_base_component_data_1_0_0_t rcache_data; /**<metadata */ 
    mca_rcache_base_component_init_fn_t rcache_init; /**<init function */ 
}; 

typedef struct mca_rcache_base_component_1_0_0_t mca_rcache_base_component_1_0_0_t; 

typedef struct mca_rcache_base_component_1_0_0_t mca_rcache_base_component_t; 


/**
 * rcache module descriptor
 */ 
struct mca_rcache_base_module_t {
    mca_rcache_base_component_t *rcache_component; /**< component struct */
    mca_rcache_base_module_find_fn_t rcache_find;
    mca_rcache_base_module_find_all_fn_t rcache_find_all;
    mca_rcache_base_module_insert_fn_t rcache_insert;
    mca_rcache_base_module_delete_fn_t rcache_delete;
    mca_rcache_base_module_finalize_fn_t rcache_finalize;
    opal_mutex_t lock;
};
typedef struct mca_rcache_base_module_t mca_rcache_base_module_t;

/**
 * Macro for use in components that are of type rcache v1.0.0
 */
#define MCA_RCACHE_BASE_VERSION_1_0_0 \
  /* rcache v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* rcache v1.0 */ \
  "rcache", 1, 0, 0

#endif /* MCA_RCACHE_H */

