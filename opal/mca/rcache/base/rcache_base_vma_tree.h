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
 *
 * Copyright (c) 2013      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
  * @file
  * Registation cache VMA tree implementation
  */
#ifndef MCA_RCACHE_BASE_VMA_TREE_H
#define MCA_RCACHE_BASE_VMA_TREE_H

#include "opal_config.h"

#include "opal/mca/rcache/rcache.h"
#include "rcache_base_vma.h"

/*
 * Data structures for the tree of allocated memory
 */

struct mca_rcache_base_vma_reg_list_item_t
{
    opal_list_item_t super;
    mca_rcache_base_registration_t *reg;
};
typedef struct mca_rcache_base_vma_reg_list_item_t mca_rcache_base_vma_reg_list_item_t;
OBJ_CLASS_DECLARATION(mca_rcache_base_vma_reg_list_item_t);

/**
 * The item in the vma_tree itself
 */
struct mca_rcache_base_vma_item_t
{
    opal_free_list_item_t super;     /**< the parent class */
    uintptr_t start;                 /**< the base of the memory range */
    uintptr_t end;                   /**< the bound of the memory range */
    opal_list_t reg_list;            /**< list of regs on this vma */
    bool in_use;                     /**< vma is in use in iterate */
    mca_rcache_base_vma_module_t *vma_module; /**< pointer to rcache vma belongs to */
};
typedef struct mca_rcache_base_vma_item_t mca_rcache_base_vma_item_t;

OBJ_CLASS_DECLARATION(mca_rcache_base_vma_item_t);


/*
 * initialize the vma tree
 */
int mca_rcache_base_vma_tree_init (mca_rcache_base_vma_module_t *vma_module);

/*
 * clean up the vma tree
 */
void mca_rcache_base_vma_tree_finalize(mca_rcache_base_vma_module_t *vma_module);

/**
 *  Returns the item in the vma tree
 */
mca_rcache_base_registration_t *mca_rcache_base_vma_tree_find (mca_rcache_base_vma_module_t *vma_module,
                                                               unsigned char *base,
                                                               unsigned char *bound);
/**
 * Returns all registration that overlaps given memory region
 */
int mca_rcache_base_vma_tree_find_all (
        mca_rcache_base_vma_module_t *vma_module, unsigned char *base,
        unsigned char *bound, mca_rcache_base_registration_t **regs,
        int reg_cnt);

/*
 * insert an item in the vma tree
 */
int mca_rcache_base_vma_tree_insert (mca_rcache_base_vma_module_t *vma_module,
                                     mca_rcache_base_registration_t* reg, size_t limit);

/*
 * remove an item from the vma tree
 */
int mca_rcache_base_vma_tree_delete (mca_rcache_base_vma_module_t *vma_module,
                                     mca_rcache_base_registration_t *reg);

/*
 * Dump out the contents of the rcache for debugging.
 */
void mca_rcache_base_vma_tree_dump_range (mca_rcache_base_vma_module_t *vma_module,
                                          unsigned char *base, size_t size, char *msg);


/*
 * Iterate over matching registration handles in the tree.
 */
int mca_rcache_base_vma_tree_iterate (mca_rcache_base_vma_module_t *vma_module,
                                      unsigned char *base, size_t size,
                                      int (*callback_fn) (struct mca_rcache_base_registration_t *, void *),
                                      void *ctx);

#endif /* MCA_RCACHE_BASE_VMA_TREE_H */
