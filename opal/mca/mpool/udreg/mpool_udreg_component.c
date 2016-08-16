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
 * Copyright (c) 2007-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#define OPAL_DISABLE_ENABLE_MEM_DEBUG 1
#include "opal_config.h"
#include "opal/mca/base/base.h"
#include "opal/runtime/opal_params.h"
#include "mpool_udreg.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#ifdef HAVE_SYS_VFS_H
#include <sys/vfs.h>
#endif
#ifdef HAVE_SYS_STATVFS_H
#include <sys/statvfs.h>
#endif
#ifdef HAVE_SYS_MOUNT_H
#include <sys/mount.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#include <fcntl.h>
#include <mntent.h>

/*
 * Local functions
 */
static int udreg_open(void);
static int udreg_close(void);
static int udreg_register(void);
static mca_mpool_base_module_t* udreg_init(
        struct mca_mpool_base_resources_t* resources);

mca_mpool_udreg_component_t mca_mpool_udreg_component = {
    {
        /* First, the mca_base_component_t struct containing meta
           information about the component itself */

        .mpool_version ={
            MCA_MPOOL_BASE_VERSION_2_0_0,

            .mca_component_name = "udreg",
            MCA_BASE_MAKE_VERSION(component, OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                                  OPAL_RELEASE_VERSION),
            .mca_open_component = udreg_open,
            .mca_close_component = udreg_close,
            .mca_register_component_params = udreg_register,
        },
        .mpool_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        .mpool_init = udreg_init
    }
};

/**
  * component open/close/init function
  */
static int udreg_open(void)
{
    OBJ_CONSTRUCT(&mca_mpool_udreg_component.huge_pages, opal_list_t);

    return OPAL_SUCCESS;
}


static int udreg_register(void)
{
    mca_mpool_udreg_component.print_stats = false;
    (void) mca_base_component_var_register(&mca_mpool_udreg_component.super.mpool_version,
                                           "print_stats", "print pool usage statistics at the end of the run",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_mpool_udreg_component.print_stats);

    return OPAL_SUCCESS;
}


static int udreg_close(void)
{
    opal_list_item_t *item;

    while (NULL != (item = opal_list_remove_first (&mca_mpool_udreg_component.huge_pages))) {
        OBJ_RELEASE(item);
    }

    OBJ_DESTRUCT(&mca_mpool_udreg_component.huge_pages);

    return OPAL_SUCCESS;
}

static int page_compare (opal_list_item_t **a,
                         opal_list_item_t **b) {
    mca_mpool_udreg_hugepage_t *pagea = (mca_mpool_udreg_hugepage_t *) *a;
    mca_mpool_udreg_hugepage_t *pageb = (mca_mpool_udreg_hugepage_t *) *b;
    if (pagea->page_size > pageb->page_size) {
        return 1;
    } else if (pagea->page_size < pageb->page_size) {
        return -1;
    }

    return 0;
}

static void udreg_find_hugepages (void) {
    FILE *fh;
    struct mntent *mntent;
    char *ctx, *tok;
    unsigned long page_size = 0;
    mca_mpool_udreg_hugepage_t *pool;

    fh = setmntent ("/proc/mounts", "r");
    if (NULL == fh) {
        return;
    }

    while (NULL != (mntent = getmntent(fh))) {

        if (0 != strcmp(mntent->mnt_type, "hugetlbfs")) {
            continue;
        }

        pool = OBJ_NEW(mca_mpool_udreg_hugepage_t);
        if (NULL == pool) {
            break;
        }

        pool->path = strdup(mntent->mnt_opts);
        if (NULL == pool->path) {
            break;
        }

        tok = strtok_r (pool->path, ",", &ctx);

        do {
            if (0 == strncmp (tok, "pagesize", 8)) {
                break;
            }
            tok = strtok_r (NULL, ",", &ctx);
        } while (tok);

        if (!tok) {
#if defined(USE_STATFS)
            struct statfs info;

            statfs (mntent->mnt_dir, &info);
#elif defined(HAVE_STATVFS)
            struct statvfs info;
            statvfs (mntent->mnt_dir, &info);
#endif
            page_size = info.f_bsize;
        } else {
            (void) sscanf (tok, "pagesize=%lu", &page_size);
        }

        if (0 == page_size) {
            /* could not get page size */
            continue;
        }

        opal_list_append (&mca_mpool_udreg_component.huge_pages, &pool->super);

    }

    opal_list_sort (&mca_mpool_udreg_component.huge_pages, page_compare);

    mca_mpool_udreg_component.use_huge_pages =
        !!(opal_list_get_size (&mca_mpool_udreg_component.huge_pages));

    endmntent (fh);
}



static mca_mpool_base_module_t *
udreg_init(struct mca_mpool_base_resources_t *resources)
{
    mca_mpool_udreg_module_t* mpool_module;
    static int inited = false;
    int rc;

    /* Set this here (vs in component.c) because
       opal_leave_pinned* may have been set after MCA params were
       read (e.g., by the openib btl) */
    mca_mpool_udreg_component.leave_pinned = (int)
        (1 == opal_leave_pinned || opal_leave_pinned_pipeline);

    if (!inited) {
        inited = true;
        udreg_find_hugepages ();
    }

    mpool_module =
        (mca_mpool_udreg_module_t *) malloc (sizeof (mca_mpool_udreg_module_t));

    memmove (&mpool_module->resources, resources, sizeof (*resources));

    rc = mca_mpool_udreg_module_init(mpool_module);
    if (OPAL_SUCCESS != rc) {
        free (mpool_module);
        return NULL;
    }

    return &mpool_module->super;
}
