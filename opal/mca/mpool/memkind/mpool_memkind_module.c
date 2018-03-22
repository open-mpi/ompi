/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2011 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009-2018 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011-2014 NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include <string.h>
#include "mpool_memkind.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include "opal/mca/mpool/base/base.h"

/*
 *  Initializes the mpool module.
 */

void mca_mpool_memkind_module_init(mca_mpool_memkind_module_t *mpool)
{
    mpool->super.mpool_component = &mca_mpool_memkind_component.super;
    mpool->super.mpool_alloc = mca_mpool_memkind_alloc;
    mpool->super.mpool_realloc = mca_mpool_memkind_realloc;
    mpool->super.mpool_free = mca_mpool_memkind_free;
    mpool->super.flags = MCA_MPOOL_FLAGS_MPI_ALLOC_MEM;
}

void* mca_mpool_memkind_alloc(
    mca_mpool_base_module_t* mpool,
    size_t size,
    size_t align,
    uint32_t flags)
{
    mca_mpool_memkind_module_t *memkind_module = (mca_mpool_memkind_module_t *) mpool;
    void *addr;

    if (0 == align) {
        align = memkind_module->page_size;
    }

    if ((errno = memkind_posix_memalign(memkind_module->kind, &addr, align, size))!= 0){
        return NULL;
    }

    return addr;
}

void* mca_mpool_memkind_realloc(mca_mpool_base_module_t *mpool, void *addr,
                                size_t size)
{
    mca_mpool_memkind_module_t *memkind_module = (mca_mpool_memkind_module_t *) mpool;
    return memkind_realloc (memkind_module->kind, addr, size);
}

void mca_mpool_memkind_free(mca_mpool_base_module_t *mpool, void *addr)
{
    mca_mpool_memkind_module_t *memkind_module = (mca_mpool_memkind_module_t *) mpool;
    memkind_free(memkind_module->kind, addr);
}
