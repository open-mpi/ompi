/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Voltaire. All rights reserved.
 * Copyright (c) 2011-2016 Los Alamos National Security, LLC. All rights
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
#ifndef MCA_RCACHE_UDREG_H
#define MCA_RCACHE_UDREG_H

#include "opal_config.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_free_list.h"
#include "opal/mca/event/event.h"
#include "opal/mca/rcache/rcache.h"
#include "opal/util/proc.h"
#if HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

BEGIN_C_DECLS

struct mca_rcache_udreg_component_t {
    mca_rcache_base_component_t super;
    bool print_stats;
    int leave_pinned;
};
typedef struct mca_rcache_udreg_component_t mca_rcache_udreg_component_t;

OPAL_DECLSPEC extern mca_rcache_udreg_component_t mca_rcache_udreg_component;

struct mca_rcache_udreg_resources_t {
    mca_rcache_base_resources_t base;

    /* udreg specific resources */
    bool   use_kernel_cache;
    bool   use_evict_w_unreg;
    int    max_entries;
    size_t page_size;
};
typedef struct mca_rcache_udreg_resources_t mca_rcache_udreg_resources_t;

struct mca_rcache_udreg_module_t;

struct mca_rcache_udreg_module_t {
    mca_rcache_base_module_t super;
    mca_rcache_udreg_resources_t resources;
    opal_free_list_t reg_list;
    opal_mutex_t lock;
    void *udreg_handle;
    /** used to communicate the access flags to the underlying registration
     * function */
    int requested_access_flags;
    int requested_flags;
};
typedef struct mca_rcache_udreg_module_t mca_rcache_udreg_module_t;


/*
 *  Initializes the rcache module.
 */
int mca_rcache_udreg_module_init(mca_rcache_udreg_module_t *rcache);

END_C_DECLS
#endif
