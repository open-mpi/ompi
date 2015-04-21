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
 * Copyright (c) 2012      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
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
#include "mpool_gpusm.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

/*
 * Local functions
 */
static int gpusm_open(void);
static int gpusm_close(void);
static int gpusm_register(void);
static mca_mpool_base_module_t* gpusm_init(struct mca_mpool_base_resources_t* resources);

mca_mpool_gpusm_component_t mca_mpool_gpusm_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

        .mpool_version = {
            MCA_MPOOL_BASE_VERSION_2_0_0,

            .mca_component_name = "gpusm",
            MCA_BASE_MAKE_VERSION(component, OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                                  OPAL_RELEASE_VERSION),
            .mca_open_component = gpusm_open,
            .mca_close_component = gpusm_close,
            .mca_register_component_params = gpusm_register,
        },
        .mpool_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        .mpool_init = gpusm_init,
    }
};

/**
  * Component open/close/init/register functions.  Most do not do anything,
  * but keep around for placeholders.
  */
static int gpusm_open(void)
{
    return OPAL_SUCCESS;
}


static int gpusm_register(void)
{
	return OPAL_SUCCESS;
}


static int gpusm_close(void)
{
    return OPAL_SUCCESS;
}


static mca_mpool_base_module_t* gpusm_init(struct mca_mpool_base_resources_t *resources)
{
    mca_mpool_gpusm_module_t* mpool_module;

    mpool_module =
        (mca_mpool_gpusm_module_t*)malloc(sizeof(mca_mpool_gpusm_module_t));

    mpool_module->resources = *resources;

    mca_mpool_gpusm_module_init(mpool_module);

    return &mpool_module->super;
}
