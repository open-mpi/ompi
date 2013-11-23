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
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All rights
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
#ifndef MCA_MPOOL_UDREG_H
#define MCA_MPOOL_UDREG_H

#include "ompi_config.h"
#include "opal/class/opal_list.h"
#include "ompi/class/ompi_free_list.h"
#include "opal/mca/event/event.h"
#include "ompi/mca/mpool/mpool.h"
#if HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

BEGIN_C_DECLS

struct mca_mpool_udreg_component_t {
    mca_mpool_base_component_t super;
    bool print_stats;
    int leave_pinned;
    opal_list_t huge_pages;
    bool use_huge_pages;
};
typedef struct mca_mpool_udreg_component_t mca_mpool_udreg_component_t;

OMPI_DECLSPEC extern mca_mpool_udreg_component_t mca_mpool_udreg_component;

struct mca_mpool_udreg_module_t;

struct mca_mpool_base_resources_t {
    /* the start of this mpool should match grdma */
    char  *pool_name;
    void  *reg_data;
    size_t sizeof_reg;
    int (*register_mem)(void *reg_data, void *base, size_t size,
        mca_mpool_base_registration_t *reg);
    int (*deregister_mem)(void *reg_data, mca_mpool_base_registration_t *reg);

    /* udreg specific resources */
    bool   use_kernel_cache;
    bool   use_evict_w_unreg;
    int    max_entries;
    size_t page_size;
};
typedef struct mca_mpool_base_resources_t mca_mpool_base_resources_t;

struct mca_mpool_udreg_hugepage_t {
    opal_list_item_t super;
    unsigned long    page_size;
    char            *path;
    opal_list_t      allocations;
    int              cnt;
};
typedef struct mca_mpool_udreg_hugepage_t mca_mpool_udreg_hugepage_t;

OBJ_CLASS_DECLARATION(mca_mpool_udreg_hugepage_t);

struct mca_mpool_udreg_hugepage_alloc_t {
    opal_list_item_t            super;
    int                         fd;
    char                       *path;
    void                       *ptr;
    size_t                      size;
    mca_mpool_udreg_hugepage_t *huge_table;
};
typedef struct mca_mpool_udreg_hugepage_alloc_t mca_mpool_udreg_hugepage_alloc_t;

OBJ_CLASS_DECLARATION(mca_mpool_udreg_hugepage_pool_item_t);

struct mca_mpool_udreg_module_t {
    mca_mpool_base_module_t super;
    struct mca_mpool_base_resources_t resources;
    ompi_free_list_t reg_list;
    mca_mpool_udreg_hugepage_t *huge_page;
    void *udreg_handle;
};
typedef struct mca_mpool_udreg_module_t mca_mpool_udreg_module_t;


/*
 *  Initializes the mpool module.
 */
int mca_mpool_udreg_module_init(mca_mpool_udreg_module_t *mpool);

/*
 *  Returns base address of shared memory mapping.
 */
void *mca_mpool_udreg_base(mca_mpool_base_module_t *mpool);

/**
  *  Allocate block of registered memory.
  */
void* mca_mpool_udreg_alloc(mca_mpool_base_module_t *mpool, size_t size,
        size_t align, uint32_t flags,
        mca_mpool_base_registration_t** registration);

/**
  * realloc block of registered memory
  */
void* mca_mpool_udreg_realloc( mca_mpool_base_module_t *mpool, void* addr,
        size_t size, mca_mpool_base_registration_t** registration);

/**
  * register block of memory
  */
int mca_mpool_udreg_register(mca_mpool_base_module_t* mpool, void *addr,
        size_t size, uint32_t flags, mca_mpool_base_registration_t **reg);

/**
 * deregister memory
 */
int mca_mpool_udreg_deregister(mca_mpool_base_module_t *mpool,
        mca_mpool_base_registration_t *reg);

/**
  * free memory allocated by alloc function
  */
void mca_mpool_udreg_free(mca_mpool_base_module_t *mpool, void * addr,
        mca_mpool_base_registration_t *reg);

/**
 * find registration for a given block of memory
 */
int mca_mpool_udreg_find(struct mca_mpool_base_module_t* mpool, void* addr,
        size_t size, mca_mpool_base_registration_t **reg);

/**
 * finalize mpool
 */
void mca_mpool_udreg_finalize(struct mca_mpool_base_module_t *mpool);

/**
 * Fault Tolerance Event Notification Function
 * @param state Checkpoint Stae
 * @return OMPI_SUCCESS or failure status
 */
int mca_mpool_udreg_ft_event(int state);

/**
 * evict one unused registration from the mpool's lru.
 * @return true on success, false on failure
 */
bool mca_mpool_udreg_evict (struct mca_mpool_base_module_t *mpool);

END_C_DECLS
#endif
