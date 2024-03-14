/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2020 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2010-2017 IBM Corporation. All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "base.h"
#include "mpool_base_tree.h"
#include "opal/align.h"
#include "opal/mca/mpool/mpool.h"
#include "opal/mca/threads/mutex.h"
#include "opal/util/info.h"
#include <stdint.h>
#include <string.h>

static void unregister_tree_item(mca_mpool_base_tree_item_t *mpool_tree_item)
{
    mca_mpool_base_module_t *mpool;

    mpool = mpool_tree_item->mpool;
    mpool->mpool_free(mpool, mpool_tree_item->key);
}

/**
 * Function to allocate special memory according to what the user requests in
 * the info object.
 *
 * If the info parameter is MPI_INFO_NULL, then this function will try to allocate
 * the memory with the optionally named mpool or malloc and try to register the
 * pointer with as many registration caches as possible. Registration caches that
 * fail to register the region will be ignored. The mpool name can optionally be
 * specified in the info object.
 *
 * @param size the size of the memory area to allocate
 * @param info an info object which tells us what kind of memory to allocate
 *
 * @retval pointer to the allocated memory
 * @retval NULL on failure
 */
void *mca_mpool_base_alloc(size_t size, opal_info_t *info, const char *hints)
{
    mca_mpool_base_tree_item_t *mpool_tree_item = NULL;
    mca_mpool_base_module_t *mpool;
    void *mem = NULL;
    opal_cstring_t *align_info_str;
    long long memory_alignment = OPAL_ALIGN_MIN;

    mpool_tree_item = mca_mpool_base_tree_item_get();
    if (!mpool_tree_item) {
        return NULL;
    }

    if (NULL != info) {
        int flag;
        opal_info_get(info, "mpi_minimum_memory_alignment",
                      &align_info_str, &flag);

        if (flag) {
            long long tmp_align = atoll(align_info_str->string);
            OBJ_RELEASE(align_info_str);
            if (tmp_align > memory_alignment) {
                memory_alignment = tmp_align;
            }
        }
    }

    mpool_tree_item->num_bytes = size;
    mpool_tree_item->count = 0;

    mpool = mca_mpool_base_module_lookup(hints);
    if (NULL != mpool) {
        mem = mpool->mpool_alloc (mpool, size, memory_alignment, 0);
    }

    if (NULL == mem) {
        /* fall back to default mpool */
        mem = mca_mpool_base_default_module->mpool_alloc(mca_mpool_base_default_module,
                                                         size, memory_alignment, 0);

        mca_mpool_base_tree_item_put(mpool_tree_item);
    } else {
        mpool_tree_item->mpool = mpool;
        mpool_tree_item->key = mem;
        mca_mpool_base_tree_insert(mpool_tree_item);
    }

    return mem;
}

/**
 * Function to free memory previously allocated by mca_mpool_base_alloc
 *
 * @param base pointer to the memory to free
 *
 * @retval OPAL_SUCCESS
 * @retval OPAL_ERR_BAD_PARAM if the passed base pointer was invalid
 */
int mca_mpool_base_free(void *base)
{
    mca_mpool_base_tree_item_t *mpool_tree_item = NULL;
    int rc;

    if (!base) {
        return OPAL_ERROR;
    }

    mpool_tree_item = mca_mpool_base_tree_find(base);

    if (!mpool_tree_item) {
        /* nothing in the tree this was just plain old malloc'd memory */
        mca_mpool_base_default_module->mpool_free(mca_mpool_base_default_module, base);
        return OPAL_SUCCESS;
    }

    rc = mca_mpool_base_tree_delete(mpool_tree_item);
    if (OPAL_SUCCESS == rc) {
        unregister_tree_item(mpool_tree_item);
        mca_mpool_base_tree_item_put(mpool_tree_item);
    }

    return rc;
}
