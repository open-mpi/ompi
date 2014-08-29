/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2009 Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2008-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2013 Los Alamos National Security, LLC.  
 *                         All rights reserved. 
 * Copyright (c) 2014      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H*/
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif  /* HAVE_STDLIB_H */
#include <errno.h>
#include "opal/mca/base/base.h"

#include "ompi/mca/allocator/base/base.h"
#include "mpool_sm.h"
#include "ompi/mca/common/sm/common_sm.h"
#include "ompi/proc/proc.h"

#if OPAL_ENABLE_FT_CR    == 1
#include "opal/runtime/opal_cr.h"
#endif

/*
 * Local functions
 */
static int
mca_mpool_sm_register(void);

static int
mca_mpool_sm_open(void);

static int
mca_mpool_sm_close(void);

static mca_mpool_base_module_t *
mca_mpool_sm_init(struct mca_mpool_base_resources_t* resources);

mca_mpool_sm_component_t mca_mpool_sm_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        MCA_MPOOL_BASE_VERSION_2_0_0,

        "sm", /* MCA component name */
        OMPI_MAJOR_VERSION,  /* MCA component major version */
        OMPI_MINOR_VERSION,  /* MCA component minor version */
        OMPI_RELEASE_VERSION,  /* MCA component release version */
        mca_mpool_sm_open,  /* component open  */
        mca_mpool_sm_close,
        NULL,
        mca_mpool_sm_register
      },
      {
          /* The component is checkpoint ready */
          MCA_BASE_METADATA_PARAM_CHECKPOINT
      },

      mca_mpool_sm_init
    }
};

static long default_min = 134217728;
static unsigned long long ompi_mpool_sm_min_size;
static int ompi_mpool_sm_verbose;

static int mca_mpool_sm_register(void)
{
    /* register SM component parameters */
    (void) mca_base_var_group_component_register(&mca_mpool_sm_component.super.mpool_version,
                                                 "Shared memory pool");

    mca_mpool_sm_component.sm_allocator_name = "bucket";
    (void) mca_base_component_var_register(&mca_mpool_sm_component.super.mpool_version,
                                           "allocator", "Name of allocator component "
                                           "to use with sm mpool", MCA_BASE_VAR_TYPE_STRING,
                                           NULL, 0, 0, OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_mpool_sm_component.sm_allocator_name);

    /* register as an unsigned long long to get up to 64 bits for the size */
    ompi_mpool_sm_min_size = default_min;
    (void) mca_base_component_var_register(&mca_mpool_sm_component.super.mpool_version,
                                           "min_size", "Minimum size of the sm mpool shared memory file",
                                           MCA_BASE_VAR_TYPE_UNSIGNED_LONG_LONG, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_mpool_sm_min_size);

    ompi_mpool_sm_verbose = 0;
    (void) mca_base_component_var_register(&mca_mpool_sm_component.super.mpool_version,
                                           "verbose", "Enable verbose output for mpool sm component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_mpool_sm_verbose);

    return OMPI_SUCCESS;
}

/**
  * component open/close/init function
  */
static int mca_mpool_sm_open(void)
{
    if (ompi_mpool_sm_verbose != 0) {
        mca_mpool_sm_component.verbose = opal_output_open(NULL);
    } else {
        mca_mpool_sm_component.verbose = -1;
    }

    return OMPI_SUCCESS;
}

static int mca_mpool_sm_close( void )
{
    return OMPI_SUCCESS;
}

static mca_mpool_base_module_t *
mca_mpool_sm_init(struct mca_mpool_base_resources_t *resources)
{
    mca_mpool_sm_module_t *mpool_module;
    mca_allocator_base_component_t* allocator_component;
    ompi_proc_t **procs;
    size_t num_all_procs, i, num_local_procs = 0;

    /* README: this needs to change if procs in different jobs (even
     * spawned ones) are to talk using shared memory */
    if (NULL == (procs = ompi_proc_world(&num_all_procs))) {
        /* out of resources, so just bail */
        return NULL;
    }
    for (i = 0 ; i < num_all_procs ; ++i) {
        if (OPAL_PROC_ON_LOCAL_NODE(procs[i]->proc_flags)) {
            num_local_procs++;
        }
    }

    /* Make a new mpool module */
    mpool_module = 
        (mca_mpool_sm_module_t *)malloc(sizeof(mca_mpool_sm_module_t));
    mca_mpool_sm_module_init(mpool_module);

    /* set sm_size */
    mpool_module->sm_size = resources->size;

    /* clip at the min size */
    if (mpool_module->sm_size < (long) ompi_mpool_sm_min_size) {
      mpool_module->sm_size = (long) ompi_mpool_sm_min_size;
    }

    allocator_component = mca_allocator_component_lookup(
        mca_mpool_sm_component.sm_allocator_name);

    /* if specified allocator cannot be loaded - look for an alternative */
    if (NULL == allocator_component) {
        if (opal_list_get_size(&ompi_allocator_base_framework.framework_components) == 0) {
            mca_base_component_list_item_t *item =
                (mca_base_component_list_item_t *)
                opal_list_get_first(&ompi_allocator_base_framework.framework_components);
            allocator_component =
                (mca_allocator_base_component_t *)item->cli_component;
            opal_output(
                0, "mca_mpool_sm_init: "
                "unable to locate allocator: %s - using %s\n",
                mca_mpool_sm_component.sm_allocator_name,
                allocator_component->allocator_version.mca_component_name);
        } else {
            opal_output(0, "mca_mpool_sm_init: "
                        "unable to locate allocator: %s\n",
                        mca_mpool_sm_component.sm_allocator_name);
            free(procs);
            return NULL;
        }
    }

    mpool_module->mem_node = resources->mem_node;

    opal_output(mca_mpool_sm_component.verbose,
                "mca_mpool_sm_init: shared memory size used: (%ld)",
                mpool_module->sm_size);

    if (NULL == (mpool_module->sm_common_module =
        mca_common_sm_module_attach(&resources->bs_meta_buf,
                                    sizeof(mca_common_sm_module_t), 8))) {
        opal_output(mca_mpool_sm_component.verbose, "mca_mpool_sm_init: "
                    "unable to create shared memory mapping (%s)",
                    resources->bs_meta_buf.seg_name);
        free(mpool_module);
        free(procs);
        return NULL;
    }
    free(procs);

    /* setup allocator */
    mpool_module->sm_allocator = 
      allocator_component->allocator_init(true,
                                          mca_common_sm_seg_alloc, 
                                          NULL, &(mpool_module->super));
    if (NULL == mpool_module->sm_allocator) {
        opal_output(0, "mca_mpool_sm_init: unable to initialize allocator");
        free(mpool_module);
        return NULL;
    }
   
    return &mpool_module->super;
}

