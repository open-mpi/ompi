/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 *
 * Copyright (c) 2006      Voltaire. All rights reserved.
 * Copyright (c) 2009      IBM Corporation.  All rights reserved.
 * Copyright (c) 2015-2017 Los Alamos National Security, LLC. All rights
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
  * Description of the Registration Cache framework
  */
#ifndef MCA_RCACHE_VMA_H
#define MCA_RCACHE_VMA_H
#include "opal_config.h"
#include "opal/mca/mca.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_rb_tree.h"
#include "opal/class/opal_lifo.h"
#include "opal/mca/rcache/rcache.h"

BEGIN_C_DECLS

struct mca_rcache_vma_module_t {
    mca_rcache_base_module_t base;
    opal_rb_tree_t rb_tree;
    opal_list_t vma_list;
    opal_lifo_t vma_gc_lifo;
    size_t reg_cur_cache_size;
};
typedef struct mca_rcache_vma_module_t mca_rcache_vma_module_t;


struct mca_rcache_vma_component_t {
    mca_rcache_base_component_t super;
};
typedef struct mca_rcache_vma_component_t mca_rcache_vma_component_t;

OPAL_DECLSPEC extern mca_rcache_vma_component_t mca_rcache_vma_component;



void mca_rcache_vma_module_init(mca_rcache_vma_module_t* rcache);

int mca_rcache_vma_find(mca_rcache_base_module_t* rcache, void* addr,
        size_t size, mca_mpool_base_registration_t **reg);

int mca_rcache_vma_find_all(mca_rcache_base_module_t* rcache, void* addr,
         size_t size,  mca_mpool_base_registration_t **regs, int reg_cnt);

int mca_rcache_vma_insert(struct mca_rcache_base_module_t* rcache,
        mca_mpool_base_registration_t* registration, size_t limit);

int mca_rcache_vma_delete(struct mca_rcache_base_module_t* rcache,
        mca_mpool_base_registration_t* registration);

/**
  * init/finalize
  */

void mca_rcache_vma_module_init(mca_rcache_vma_module_t *rcache);

void mca_rcache_vma_finalize(struct mca_rcache_base_module_t*);

void mca_rcache_vma_dump_range(struct mca_rcache_base_module_t *rcache,
                               unsigned char* addr, size_t size, char *msg);


extern opal_free_list_t mca_rcache_vma_tree_items;
extern bool mca_rcache_vma_tree_items_inited;
extern unsigned int mca_rcache_vma_tree_items_min;
extern int mca_rcache_vma_tree_items_max;
extern unsigned int mca_rcache_vma_tree_items_inc;

/**
 * Iterate over registrations in the specified range.
 *
 * @param[in] vma_module  vma tree
 * @param[in] base        base address of region
 * @param[in] size        size of region
 * @param[in] callback_fn function to call for each matching registration handle
 * @param[in] ctx         callback context
 *
 * The callback will be made with the vma lock held. This is a recursive lock so
 * it is still safe to call any vma functions on this vma_module. Keep in mind it
 * is only safe to call mca_rcache_base_vma_delete() on the supplied registration
 * from the callback. The iteration will terminate if the callback returns anything
 * other than OPAL_SUCCESS.
 */
int mca_rcache_vma_iterate (mca_rcache_base_module_t *rcache, unsigned char *base, size_t size,
                            int (*callback_fn) (mca_mpool_base_registration_t *, void *),
                            void *ctx);

END_C_DECLS

#endif /* MCA_RCACHE_VMA_H */


