/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_MPOOL_MEMKIND_H
#define MCA_MPOOL_MEMKIND_H

#include "opal_config.h"

#include "opal/mca/event/event.h"
#include "opal/mca/mpool/mpool.h"

#include "opal/mca/allocator/allocator.h"
#include <memkind.h>

BEGIN_C_DECLS

static const int mca_mpool_memkind_default_pagesize = 4096;

struct mca_mpool_memkind_module_t {
    mca_mpool_base_module_t super;
    memkind_t kind;
    int page_size;
};
typedef struct mca_mpool_memkind_module_t mca_mpool_memkind_module_t;

struct mca_mpool_memkind_component_t {
    mca_mpool_base_component_t super;
    int  hbw;
    int  pagesize;
    int  bind;
    int  default_partition;
    int  priority;
    char *memkind_file;
    int  output;
    mca_mpool_memkind_module_t modules[MEMKIND_NUM_BASE_KIND];
};
typedef struct mca_mpool_memkind_component_t mca_mpool_memkind_component_t;
OPAL_MODULE_DECLSPEC extern mca_mpool_memkind_component_t mca_mpool_memkind_component;

/*
 *  Initializes the mpool module.
*/

void mca_mpool_memkind_module_init(mca_mpool_memkind_module_t *mpool, int partition);

/**
  *  Allocate block of high bandwidth memory.
  */
void* mca_mpool_memkind_alloc(
    mca_mpool_base_module_t* mpool,
    size_t size,
    size_t align,
    uint32_t flags);

/**
  * realloc function typedef
  */
void* mca_mpool_memkind_realloc(
    mca_mpool_base_module_t* mpool,
    void* addr,
    size_t size);

/**
  * free function typedef
  */
void mca_mpool_memkind_free(
    mca_mpool_base_module_t* mpool,
    void * addr);

END_C_DECLS

#endif
