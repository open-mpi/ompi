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
 * Copyright (c) 2012      NVIDIA Corporation.  All rights reserved.
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
#ifndef MCA_MPOOL_RGPUSM_H
#define MCA_MPOOL_RGPUSM_H

#include "ompi_config.h"
#include "opal/class/opal_list.h"
#include "ompi/class/ompi_free_list.h"
#include "ompi/mca/mpool/mpool.h"

BEGIN_C_DECLS

struct mca_mpool_rgpusm_component_t {
    mca_mpool_base_component_t super;
    char* rcache_name;
    unsigned long long rcache_size_limit;
    bool print_stats;
    int leave_pinned;
    int output;
};
typedef struct mca_mpool_rgpusm_component_t mca_mpool_rgpusm_component_t;

OMPI_DECLSPEC extern mca_mpool_rgpusm_component_t mca_mpool_rgpusm_component;

struct mca_mpool_base_resources_t {
    void *reg_data;
    size_t sizeof_reg;
    int (*register_mem)(void *base, size_t size, mca_mpool_base_registration_t *newreg,
                        mca_mpool_base_registration_t *hdrreg);
    int (*deregister_mem)(void *reg_data, mca_mpool_base_registration_t *reg);
};
typedef struct mca_mpool_base_resources_t mca_mpool_base_resources_t;

struct mca_mpool_rgpusm_module_t {
    mca_mpool_base_module_t super;
    struct mca_mpool_base_resources_t resources;
    ompi_free_list_t reg_list;
    opal_list_t lru_list;
    uint32_t stat_cache_hit;
    uint32_t stat_cache_valid;
    uint32_t stat_cache_invalid;
    uint32_t stat_cache_miss;
    uint32_t stat_evicted;
    uint32_t stat_cache_found;
    uint32_t stat_cache_notfound;
}; typedef struct mca_mpool_rgpusm_module_t mca_mpool_rgpusm_module_t;

/*
 *  Initializes the mpool module.
 */
void mca_mpool_rgpusm_module_init(mca_mpool_rgpusm_module_t *mpool);

/**
  * register block of memory
  */
int mca_mpool_rgpusm_register(mca_mpool_base_module_t* mpool, void *addr,
        size_t size, uint32_t flags, mca_mpool_base_registration_t **reg);

/**
 * deregister memory
 */
int mca_mpool_rgpusm_deregister(mca_mpool_base_module_t *mpool,
        mca_mpool_base_registration_t *reg);

/**
  * free memory allocated by alloc function
  */
void mca_mpool_rgpusm_free(mca_mpool_base_module_t *mpool, void * addr,
        mca_mpool_base_registration_t *reg);

/**
 * find registration for a given block of memory
 */
int mca_mpool_rgpusm_find(struct mca_mpool_base_module_t* mpool, void* addr,
        size_t size, mca_mpool_base_registration_t **reg);

/**
 * unregister all registration covering the block of memory
 */
int mca_mpool_rgpusm_release_memory(mca_mpool_base_module_t* mpool, void *base,
        size_t size);

/**
 * finalize mpool
 */
void mca_mpool_rgpusm_finalize(struct mca_mpool_base_module_t *mpool);

/**
 * Fault Tolerance Event Notification Function
 * @param state Checkpoint Stae
 * @return OMPI_SUCCESS or failure status
 */
int mca_mpool_rgpusm_ft_event(int state);

END_C_DECLS
#endif
