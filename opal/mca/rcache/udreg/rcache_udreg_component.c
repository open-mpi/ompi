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
#include "rcache_udreg.h"
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
static mca_rcache_base_module_t* udreg_init(
        struct mca_rcache_base_resources_t* resources);

mca_rcache_udreg_component_t mca_rcache_udreg_component = {
    {
        /* First, the mca_base_component_t struct containing meta
           information about the component itself */

        .rcache_version ={
            MCA_RCACHE_BASE_VERSION_3_0_0,

            .mca_component_name = "udreg",
            MCA_BASE_MAKE_VERSION(component, OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                                  OPAL_RELEASE_VERSION),
            .mca_open_component = udreg_open,
            .mca_close_component = udreg_close,
            .mca_register_component_params = udreg_register,
        },
        .rcache_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        .rcache_init = udreg_init
    }
};

/**
  * component open/close/init function
  */
static int udreg_open(void)
{
    return OPAL_SUCCESS;
}


static int udreg_register(void)
{
    mca_rcache_udreg_component.print_stats = false;
    (void) mca_base_component_var_register(&mca_rcache_udreg_component.super.rcache_version,
                                           "print_stats", "print pool usage statistics at the end of the run",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_rcache_udreg_component.print_stats);

    return OPAL_SUCCESS;
}


static int udreg_close(void)
{
    return OPAL_SUCCESS;
}

static mca_rcache_base_module_t *
udreg_init(struct mca_rcache_base_resources_t *resources)
{
    mca_rcache_udreg_resources_t *udreg_resources = (mca_rcache_udreg_resources_t *) resources;
    mca_rcache_udreg_module_t* rcache_module;
    static int inited = false;
    int rc;

    /* Set this here (vs in component.c) because
       opal_leave_pinned* may have been set after MCA params were
       read (e.g., by the openib btl) */
    mca_rcache_udreg_component.leave_pinned = (int)
        (1 == opal_leave_pinned || opal_leave_pinned_pipeline);

    if (!inited) {
        inited = true;
    }

    rcache_module =
        (mca_rcache_udreg_module_t *) malloc (sizeof (mca_rcache_udreg_module_t));

    memmove (&rcache_module->resources, udreg_resources, sizeof (*udreg_resources));

    rc = mca_rcache_udreg_module_init(rcache_module);
    if (OPAL_SUCCESS != rc) {
        free (rcache_module);
        return NULL;
    }

    return &rcache_module->super;
}
