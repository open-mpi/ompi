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
 * Copyright (c) 2012      NVIDIA Corporation.  All rights reserved.
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
#include "mpool_rgpusm.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

/*
 * Local functions
 */
static int rgpusm_open(void);
static int rgpusm_close(void);
static int rgpusm_register(void);
static mca_mpool_base_module_t* rgpusm_init(struct mca_mpool_base_resources_t* resources);

static int ompi_mpool_rgpusm_verbose = 0;

mca_mpool_rgpusm_component_t mca_mpool_rgpusm_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
          MCA_MPOOL_BASE_VERSION_2_0_0,

          "rgpusm", /* MCA component name */
          OMPI_MAJOR_VERSION,  /* MCA component major version */
          OMPI_MINOR_VERSION,  /* MCA component minor version */
          OMPI_RELEASE_VERSION,  /* MCA component release version */
          rgpusm_open,  /* component open  */
          rgpusm_close,
          NULL,
          rgpusm_register
      },
      {
          /* The component is checkpoint ready */
          MCA_BASE_METADATA_PARAM_CHECKPOINT
      },

      rgpusm_init
    }
};

/**
  * component open/close/init function
  */
static int rgpusm_open(void)
{
    mca_mpool_rgpusm_component.output = opal_output_open(NULL);
    opal_output_set_verbosity(mca_mpool_rgpusm_component.output, ompi_mpool_rgpusm_verbose);

    return OMPI_SUCCESS;
}


static int rgpusm_register(void)
{
    mca_mpool_rgpusm_component.rcache_name = "vma";
    (void) mca_base_component_var_register(&mca_mpool_rgpusm_component.super.mpool_version,
                                           "rcache_name",
                                           "The name of the registration cache the mpool should use",
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_mpool_rgpusm_component.rcache_name);
    mca_mpool_rgpusm_component.rcache_size_limit = 0;
    (void) mca_base_component_var_register(&mca_mpool_rgpusm_component.super.mpool_version,
                                           "rcache_size_limit",
                                           "the maximum size of registration cache in bytes. "
                                           "0 is unlimited (default 0)",
                                           MCA_BASE_VAR_TYPE_UNSIGNED_LONG_LONG, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_mpool_rgpusm_component.rcache_size_limit);

    mca_mpool_rgpusm_component.leave_pinned = 1;
    (void) mca_base_component_var_register(&mca_mpool_rgpusm_component.super.mpool_version,
                                           "leave_pinned",
                                           "Whether to keep memory handles around or release them when done. ",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_mpool_rgpusm_component.leave_pinned);

    mca_mpool_rgpusm_component.print_stats = false;
    (void) mca_base_component_var_register(&mca_mpool_rgpusm_component.super.mpool_version,
                                           "print_stats",
                                           "print pool usage statistics at the end of the run",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_mpool_rgpusm_component.print_stats);

    /* Set different levels of verbosity in the rgpusm related code. */
    ompi_mpool_rgpusm_verbose = 0;
    (void) mca_base_component_var_register(&mca_mpool_rgpusm_component.super.mpool_version,
                                           "verbose", "Set level of mpool rgpusm verbosity",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_mpool_rgpusm_verbose);

    return OMPI_SUCCESS;
}


static int rgpusm_close(void)
{
    return OMPI_SUCCESS;
}


static mca_mpool_base_module_t* rgpusm_init(
     struct mca_mpool_base_resources_t *resources)
{
    mca_mpool_rgpusm_module_t* mpool_module;

    mpool_module =
        (mca_mpool_rgpusm_module_t*)malloc(sizeof(mca_mpool_rgpusm_module_t));

    mpool_module->resources = *resources;

    mca_mpool_rgpusm_module_init(mpool_module);

    return &mpool_module->super;
}
