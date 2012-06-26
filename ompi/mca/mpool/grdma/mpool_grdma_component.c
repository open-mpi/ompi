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
 * Copyright (c) 2012      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#define OPAL_DISABLE_ENABLE_MEM_DEBUG 1
#include "ompi_config.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "orca/include/rte_orca.h"
#include "ompi/runtime/params.h"
#include "mpool_grdma.h"
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
static int grdma_open(void);
static int grdma_close(void);
static int grdma_register(void);
static mca_mpool_base_module_t* grdma_init(
        struct mca_mpool_base_resources_t* resources);

mca_mpool_grdma_component_t mca_mpool_grdma_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
          MCA_MPOOL_BASE_VERSION_2_0_0,

          "grdma", /* MCA component name */
          OMPI_MAJOR_VERSION,  /* MCA component major version */
          OMPI_MINOR_VERSION,  /* MCA component minor version */
          OMPI_RELEASE_VERSION,  /* MCA component release version */
          grdma_open,  /* component open  */
          grdma_close,
          NULL,
          grdma_register
      },
      {
          /* The component is checkpoint ready */
          MCA_BASE_METADATA_PARAM_CHECKPOINT
      },

      grdma_init
    }
};

/**
  * component open/close/init function
  */
static int grdma_open(void)
{
    OBJ_CONSTRUCT(&mca_mpool_grdma_component.pools, opal_list_t);

    return OMPI_SUCCESS;
}


static int grdma_register(void)
{
    int val;

    mca_base_param_reg_string(&mca_mpool_grdma_component.super.mpool_version,
            "rcache_name",
            "The name of the registration cache the mpool should use",
            false, false, "vma", &mca_mpool_grdma_component.rcache_name);

    mca_base_param_reg_int(&mca_mpool_grdma_component.super.mpool_version,
            "rcache_size_limit",
            "the maximum size of registration cache in bytes. "
            "0 is unlimited (default 0)", false, false, 0, &val);

    mca_mpool_grdma_component.rcache_size_limit = (size_t)val;

    mca_base_param_reg_int(&mca_mpool_grdma_component.super.mpool_version,
            "print_stats",
            "print pool usage statistics at the end of the run",
            false, false, 0, &val);

    mca_mpool_grdma_component.print_stats = val?true:false;

    return OMPI_SUCCESS;
}


static int grdma_close(void)
{
    OBJ_DESTRUCT(&mca_mpool_grdma_component.pools);

    if (NULL != mca_mpool_grdma_component.rcache_name) {
        free(mca_mpool_grdma_component.rcache_name);
    }

    return OMPI_SUCCESS;
}


static mca_mpool_base_module_t *
grdma_init(struct mca_mpool_base_resources_t *resources)
{
    mca_mpool_grdma_module_t* mpool_module;
    mca_mpool_grdma_pool_t *pool = NULL;
    opal_list_item_t *item;

    /* Set this here (vs in component.c) because
       ompi_mpi_leave_pinned* may have been set after MCA params were
       read (e.g., by the openib btl) */
    mca_mpool_grdma_component.leave_pinned = (int) 
        (1 == ompi_mpi_leave_pinned || ompi_mpi_leave_pinned_pipeline);

    /* find the specified pool */
    for (item = opal_list_get_first (&mca_mpool_grdma_component.pools) ;
         item != opal_list_get_end (&mca_mpool_grdma_component.pools) ;
         item = opal_list_get_next (item)) {
        pool = (mca_mpool_grdma_pool_t *) item;

        if (0 == strcmp (pool->pool_name, resources->pool_name)) {
            break;
        }

        pool = NULL;
    }

    if (NULL == pool) {
        /* create new pool */
        pool = OBJ_NEW(mca_mpool_grdma_pool_t);
        if (NULL == pool) {
            return NULL;
        }

        pool->pool_name = strdup (resources->pool_name);

        opal_list_append (&mca_mpool_grdma_component.pools, &pool->super);
    }

    mpool_module =
        (mca_mpool_grdma_module_t *) malloc (sizeof (mca_mpool_grdma_module_t));

    mpool_module->resources = *resources;

    mca_mpool_grdma_module_init(mpool_module, pool);

    return &mpool_module->super;
}
