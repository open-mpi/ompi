/*
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights 
 *                         reserved. 
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

#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/proc_info.h"

#include "ompi/proc/proc.h"
#include "ompi/mca/allocator/base/base.h"

#include "mpool_pcie.h"

/*
 * Local functions
 */
static int mca_mpool_pcie_open(void);
static int mca_mpool_pcie_close( void );
static mca_mpool_base_module_t* mca_mpool_pcie_init(
    struct mca_mpool_base_resources_t* resources);

mca_mpool_pcie_component_t mca_mpool_pcie_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a mpool v2.0.0 component (which also
           implies a specific MCA version) */

        MCA_MPOOL_BASE_VERSION_2_0_0,

        "pcie", /* MCA component name */
        OMPI_MAJOR_VERSION,  /* MCA component major version */
        OMPI_MINOR_VERSION,  /* MCA component minor version */
        OMPI_RELEASE_VERSION,  /* MCA component release version */
        mca_mpool_pcie_open,  /* component open  */
        mca_mpool_pcie_close
      },

      /* Next the MCA v2.0.0 component meta data */
      
      {
          /* The component is not checkpoint ready */
          false
      },

      mca_mpool_pcie_init
    }
};


static int
mca_mpool_pcie_open(void)
{
    return OMPI_SUCCESS;
}


static int
mca_mpool_pcie_close(void)
{
    return OMPI_SUCCESS;
}


static mca_mpool_base_module_t*
mca_mpool_pcie_init(struct mca_mpool_base_resources_t* resources)
{
    mca_mpool_pcie_module_t* mpool_module;

    mpool_module = (mca_mpool_pcie_module_t*)malloc(sizeof(mca_mpool_pcie_module_t)); 
    if(NULL == mpool_module) return NULL;

    mpool_module->super.mpool_component = &mca_mpool_pcie_component.super;
    mpool_module->super.mpool_base = NULL; /* no base .. */
    mpool_module->super.mpool_alloc = mca_mpool_pcie_alloc;
    mpool_module->super.mpool_realloc = mca_mpool_pcie_realloc;
    mpool_module->super.mpool_free = mca_mpool_pcie_free;
    mpool_module->super.mpool_register = NULL;
    mpool_module->super.mpool_find = NULL;
    mpool_module->super.mpool_deregister = NULL;
    mpool_module->super.mpool_release_memory = NULL;
    mpool_module->super.mpool_finalize = NULL;
    mpool_module->super.rcache = NULL;
    mpool_module->super.flags = MCA_MPOOL_FLAGS_MPI_ALLOC_MEM;

    mpool_module->base = resources->base;
    mpool_module->len = resources->len;
    mpool_module->offset = 0;

    return (mca_mpool_base_module_t*) mpool_module;
}

