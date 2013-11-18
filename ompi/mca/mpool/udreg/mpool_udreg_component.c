/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Voltaire. All rights reserved.
 * Copyright (c) 2007-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC. All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#define OPAL_DISABLE_ENABLE_MEM_DEBUG 1
#include "ompi_config.h"
#include "opal/mca/base/base.h"
#include "orte/util/proc_info.h"
#include "ompi/runtime/params.h"
#include "mpool_udreg.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include <fcntl.h>

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

      {
          MCA_MPOOL_BASE_VERSION_2_0_0,

          "udreg", /* MCA component name */
          OMPI_MAJOR_VERSION,  /* MCA component major version */
          OMPI_MINOR_VERSION,  /* MCA component minor version */
          OMPI_RELEASE_VERSION,  /* MCA component release version */
          udreg_open,  /* component open  */
          udreg_close,
          NULL,
          udreg_register
      },
      {
          /* The component is checkpoint ready */
          MCA_BASE_METADATA_PARAM_CHECKPOINT
      },

      udreg_init
    }
};

/**
  * component open/close/init function
  */
static int udreg_open(void)
{
    OBJ_CONSTRUCT(&mca_mpool_udreg_component.huge_pages, opal_list_t);

    return OMPI_SUCCESS;
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

    return OMPI_SUCCESS;
}


static int udreg_close(void)
{
    opal_list_item_t *item;

    while (NULL != (item = opal_list_remove_first (&mca_mpool_udreg_component.huge_pages))) {
        OBJ_RELEASE(item);
    }

    OBJ_DESTRUCT(&mca_mpool_udreg_component.huge_pages);

    return OMPI_SUCCESS;
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
    char *path;
    char buffer[1024];
    char *ctx, *tok;

    fh = fopen ("/proc/mounts", "r");
    if (NULL == fh) {
        return;
    }

    while (fgets (buffer, 1024, fh)) {
        mca_mpool_udreg_hugepage_t *pool;

        (void) strtok_r (buffer, " ", &ctx);
        path = strtok_r (NULL, " ", &ctx);
        tok = strtok_r (NULL, " ", &ctx);

        if (0 != strcmp (tok, "hugetlbfs")) {
            continue;
        }

        pool = OBJ_NEW(mca_mpool_udreg_hugepage_t);
        if (NULL == pool) {
            break;
        }

        pool->path = strdup (path);

        tok = strtok_r (NULL, " ", &ctx);
        tok = strtok_r (tok, ",", &ctx);

        do {
            if (0 == strncmp (tok, "pagesize", 8)) {
                break;
            }
            tok = strtok_r (NULL, ",", &ctx);
        } while (tok);
        sscanf (tok, "pagesize=%lu", &pool->page_size);

        opal_list_append (&mca_mpool_udreg_component.huge_pages, &pool->super);
    }

    fclose (fh);

    opal_list_sort (&mca_mpool_udreg_component.huge_pages, page_compare);

    mca_mpool_udreg_component.use_huge_pages =
        !!(opal_list_get_size (&mca_mpool_udreg_component.huge_pages));
}



static mca_mpool_base_module_t *
udreg_init(struct mca_mpool_base_resources_t *resources)
{
    mca_mpool_udreg_module_t* mpool_module;
    static int inited = false;
    int rc;

    /* Set this here (vs in component.c) because
       ompi_mpi_leave_pinned* may have been set after MCA params were
       read (e.g., by the openib btl) */
    mca_mpool_udreg_component.leave_pinned = (int)
        (1 == ompi_mpi_leave_pinned || ompi_mpi_leave_pinned_pipeline);

    if (!inited) {
        inited = true;
        udreg_find_hugepages ();
    }

    mpool_module =
        (mca_mpool_udreg_module_t *) malloc (sizeof (mca_mpool_udreg_module_t));

    memmove (&mpool_module->resources, resources, sizeof (*resources));

    rc = mca_mpool_udreg_module_init(mpool_module);
    if (OMPI_SUCCESS != rc) {
        free (mpool_module);
        return NULL;
    }

    return &mpool_module->super;
}
