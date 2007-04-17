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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "mpool_rdma.h"
#include "orte/util/proc_info.h"
#include "orte/util/sys_info.h"
#include "ompi/runtime/params.h"
#include <unistd.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

/*
 * Local functions
 */
static int mca_mpool_rdma_open(void);
static mca_mpool_base_module_t* mca_mpool_rdma_init(
        struct mca_mpool_base_resources_t* resources);

mca_mpool_rdma_component_t mca_mpool_rdma_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a mpool v1.0.0 component (which also
           implies a specific MCA version) */

          MCA_MPOOL_BASE_VERSION_1_0_0,

          "rdma", /* MCA component name */
          OMPI_MAJOR_VERSION,  /* MCA component major version */
          OMPI_MINOR_VERSION,  /* MCA component minor version */
          OMPI_RELEASE_VERSION,  /* MCA component release version */
          mca_mpool_rdma_open,  /* component open  */
          NULL
      },

      /* Next the MCA v1.0.0 component meta data */

      {
        /* Whether the component is checkpointable or not */
        false
      },

      mca_mpool_rdma_init
    }
};

/**
  * component open/close/init function
  */
static int mca_mpool_rdma_open(void)
{
    int val;

    mca_base_param_reg_string(&mca_mpool_rdma_component.super.mpool_version,
            "rcache_name",
            "The name of the registration cache the mpool should use",
            false, false, "vma", &mca_mpool_rdma_component.rcache_name);

    mca_base_param_reg_int(&mca_mpool_rdma_component.super.mpool_version,
            "rcache_size_limit",
            "the maximum size of registration cache in bytes. "
            "0 is unlimited (default 0)", false, false, 0, &val);

    mca_mpool_rdma_component.rcache_size_limit = (size_t)val;

    mca_base_param_reg_int(&mca_mpool_rdma_component.super.mpool_version,
            "print_stats",
            "print pool usage statistics at the end of the run",
            false, false, 0, &val);

    mca_mpool_rdma_component.print_stats = val?true:false;

    mca_mpool_rdma_component.leave_pinned = (int) 
        (ompi_mpi_leave_pinned || ompi_mpi_leave_pinned_pipeline);

    return OMPI_SUCCESS;
}

static mca_mpool_base_module_t* mca_mpool_rdma_init(
     struct mca_mpool_base_resources_t *resources)
{
    mca_mpool_rdma_module_t* mpool_module;

    mpool_module =
        (mca_mpool_rdma_module_t*)malloc(sizeof(mca_mpool_rdma_module_t));

    mpool_module->resources = *resources;

    mca_mpool_rdma_module_init(mpool_module);

    return &mpool_module->super;
}
