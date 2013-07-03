/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
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
 * Copyright (c) 2006      Voltaire. All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC. All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_MPOOL_OPENIB_H
#define MCA_MPOOL_OPENIB_H

#include "ompi_config.h"
#include "opal/class/opal_list.h"
#include "ompi/class/ompi_free_list.h"
#include "opal/mca/event/event.h"
#include "ompi/mca/mpool/mpool.h"
#include <sys/mman.h>

BEGIN_C_DECLS

#define MCA_MPOOL_GRDMA_NAME_MAX 256

struct mca_mpool_grdma_pool_t {
    opal_list_item_t super;
    char *pool_name;
    opal_list_t lru_list;
    opal_list_t gc_list;
    struct mca_rcache_base_module_t *rcache;
};
typedef struct mca_mpool_grdma_pool_t mca_mpool_grdma_pool_t;

OBJ_CLASS_DECLARATION(mca_mpool_grdma_pool_t);

struct mca_mpool_grdma_component_t {
    mca_mpool_base_component_t super;
    opal_list_t pools;
    char *rcache_name;
    bool print_stats;
    int leave_pinned;
};
typedef struct mca_mpool_grdma_component_t mca_mpool_grdma_component_t;

OMPI_DECLSPEC extern mca_mpool_grdma_component_t mca_mpool_grdma_component;

struct mca_mpool_grdma_module_t;

struct mca_mpool_base_resources_t {
    char  *pool_name;
    void  *reg_data;
    size_t sizeof_reg;
    int (*register_mem)(void *reg_data, void *base, size_t size,
        mca_mpool_base_registration_t *reg);
    int (*deregister_mem)(void *reg_data, mca_mpool_base_registration_t *reg);
};
typedef struct mca_mpool_base_resources_t mca_mpool_base_resources_t;

struct mca_mpool_grdma_module_t {
    mca_mpool_base_module_t super;
    struct mca_mpool_base_resources_t resources;
    mca_mpool_grdma_pool_t *pool;
    ompi_free_list_t reg_list;
    uint32_t stat_cache_hit;
    uint32_t stat_cache_miss;
    uint32_t stat_evicted;
    uint32_t stat_cache_found;
    uint32_t stat_cache_notfound;
};
typedef struct mca_mpool_grdma_module_t mca_mpool_grdma_module_t;

/*
 *  Initializes the mpool module.
 */
void mca_mpool_grdma_module_init(mca_mpool_grdma_module_t *mpool, mca_mpool_grdma_pool_t *pool);

/*
 *  Returns base address of shared memory mapping.
 */
void *mca_mpool_grdma_base(mca_mpool_base_module_t *mpool);

/**
  *  Allocate block of registered memory.
  */
void* mca_mpool_grdma_alloc(mca_mpool_base_module_t *mpool, size_t size,
        size_t align, uint32_t flags,
        mca_mpool_base_registration_t** registration);

/**
  * realloc block of registered memory
  */
void* mca_mpool_grdma_realloc( mca_mpool_base_module_t *mpool, void* addr,
        size_t size, mca_mpool_base_registration_t** registration);

/**
  * register block of memory
  */
int mca_mpool_grdma_register(mca_mpool_base_module_t* mpool, void *addr,
        size_t size, uint32_t flags, mca_mpool_base_registration_t **reg);

/**
 * deregister memory
 */
int mca_mpool_grdma_deregister(mca_mpool_base_module_t *mpool,
        mca_mpool_base_registration_t *reg);

/**
  * free memory allocated by alloc function
  */
void mca_mpool_grdma_free(mca_mpool_base_module_t *mpool, void * addr,
        mca_mpool_base_registration_t *reg);

/**
 * find registration for a given block of memory
 */
int mca_mpool_grdma_find(struct mca_mpool_base_module_t* mpool, void* addr,
        size_t size, mca_mpool_base_registration_t **reg);

/**
 * unregister all registration covering the block of memory
 */
int mca_mpool_grdma_release_memory(mca_mpool_base_module_t* mpool, void *base,
        size_t size);

/**
 * finalize mpool
 */
void mca_mpool_grdma_finalize(struct mca_mpool_base_module_t *mpool);

/**
 * Fault Tolerance Event Notification Function
 * @param state Checkpoint Stae
 * @return OMPI_SUCCESS or failure status
 */
int mca_mpool_grdma_ft_event(int state);

/**
 * evict one unused registration from the mpool's lru.
 * @return true on success, false on failure
 */
bool mca_mpool_grdma_evict (struct mca_mpool_base_module_t *mpool);

END_C_DECLS
#endif
