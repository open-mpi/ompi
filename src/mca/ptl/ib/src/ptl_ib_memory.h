/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 *
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_IB_MEMORY_H
#define MCA_PTL_IB_MEMORY_H

/* Standard system includes */
#include <sys/types.h>

/* Open MPI includes */
#include "include/types.h"
#include "include/constants.h"
#include "class/ompi_object.h"
#include "class/ompi_list.h"
#include "class/ompi_rb_tree.h"
#include "class/ompi_free_list.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
/* vapi.h is not a C++ safe header file */
#include <vapi.h>
#include <vapi_common.h>

typedef struct mca_ptl_ib_mem_registry_info_t mca_ptl_ib_mem_registry_info_t;

struct mca_ptl_ib_mem_registry_info_t {
    ompi_list_item_t super;
    mca_ptl_ib_mem_registry_info_t *next;
    int ref_cnt;
    VAPI_mr_hndl_t hndl;
    VAPI_mr_t request;
    VAPI_mr_t reply;
};

OBJ_CLASS_DECLARATION(mca_ptl_ib_mem_registry_info_t);

typedef struct mca_ptl_ib_mem_registry_t mca_ptl_ib_mem_registry_t;

struct mca_ptl_ib_mem_registry_t {
    ompi_rb_tree_t rb_tree;
    ompi_free_list_t info_free_list;
    ompi_ptr_t *hints;
    mca_ptl_ib_mem_registry_info_t *evictable;
    struct mca_ptl_ib_state_t *ib_state;
    int hints_log_size;
    int hints_size;
};

OBJ_CLASS_DECLARATION(mca_ptl_ib_mem_registry_t);

/* find information on a registered memory region for a given address,
 * region size, and access permissions
 *
 */
static inline mca_ptl_ib_mem_registry_info_t *mca_ptl_ib_mem_registry_find(
    mca_ptl_ib_mem_registry_t *registry, VAPI_mr_t *key)
{
    mca_ptl_ib_mem_registry_info_t *info = (mca_ptl_ib_mem_registry_info_t *)NULL;
    uint64_t hints_hash = 0, addrll;

    if (registry->hints_size) {
        addrll = (uint64_t)(key->start);
    
        /* calculate hash index for hints array - hash is (hints_log_size - 1) bits of key 
         * from first non-zero least significant bit
         */
        hints_hash = addrll & (-addrll);
        hints_hash = (((hints_hash << registry->hints_log_size) - hints_hash) & addrll) / 
            hints_hash;

        if ((info = registry->hints[hints_hash].pval) != (void *)NULL) {
            if ((info->reply.start <= key->start) && 
                ((info->reply.start + info->reply.size) >= (key->start + key->size)) &&
                ((info->reply.acl & key->acl) == key->acl)) {
                return info;
            }
        }
    }
    
    /* search the red/black tree */
    info = ompi_rb_tree_find(&(registry->rb_tree), key);

    /* store a pointer to this info in the hints array for later lookups */
    if ((info != NULL) && registry->hints_size) {
        registry->hints[hints_hash].pval = info;
    }

    return info;
}

mca_ptl_ib_mem_registry_info_t *mca_ptl_ib_mem_registry_register(
    mca_ptl_ib_mem_registry_t *registry,
    VAPI_mr_t *mr);

int mca_ptl_ib_mem_registry_deregister(
    mca_ptl_ib_mem_registry_t *registry,
    VAPI_mr_t *mr);

void mca_ptl_ib_mem_registry_init(
    mca_ptl_ib_mem_registry_t *registry,
    struct mca_ptl_ib_state_t *ib_state);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
