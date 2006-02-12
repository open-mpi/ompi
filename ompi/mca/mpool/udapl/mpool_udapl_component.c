/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
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
#include "ompi/mca/allocator/base/base.h"
#include "mpool_udapl.h"
#include "orte/util/proc_info.h"
#include "orte/util/sys_info.h"
#include <unistd.h> 

/*
 * Local functions
 */
static int mca_mpool_udapl_open(void);
static mca_mpool_base_module_t* mca_mpool_udapl_init(
    struct mca_mpool_base_resources_t* resources);

mca_mpool_udapl_component_t mca_mpool_udapl_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a mpool v1.0.0 component (which also
           implies a specific MCA version) */

        MCA_MPOOL_BASE_VERSION_1_0_0,

        "udapl", /* MCA component name */
        OMPI_MAJOR_VERSION,  /* MCA component major version */
        OMPI_MINOR_VERSION,  /* MCA component minor version */
        OMPI_RELEASE_VERSION,  /* MCA component release version */
        mca_mpool_udapl_open,  /* component open  */
        NULL /* component close */
      },

      /* Next the MCA v1.0.0 component meta data */
      
      {
        /* Whether the component is checkpointable or not */
        false
      },

      mca_mpool_udapl_init
    }
};


static void mca_mpool_udapl_registration_constructor(mca_mpool_udapl_registration_t *registration)
{
    registration->base_reg.base = NULL; 
    registration->base_reg.bound = NULL; 
    registration->base_reg.flags = 0;
}

static void mca_mpool_udapl_registration_destructor(mca_mpool_udapl_registration_t *registration)
{
    registration->base_reg.base = NULL; 
    registration->base_reg.bound = NULL; 
    registration->base_reg.flags = 0;
}

OBJ_CLASS_INSTANCE(mca_mpool_udapl_registration_t,
                   mca_mpool_base_registration_t,
                   mca_mpool_udapl_registration_constructor,
                   mca_mpool_udapl_registration_destructor);




/**
  * component open/close/init function
  */
static int mca_mpool_udapl_open(void)
{
    opal_output(0, "mpool_udapl_open\n");
    return OMPI_SUCCESS;
}


/* Allocates a segment of memory and registers with uDAPL, user_out returns the memory handle. */ 
static mca_mpool_base_module_t* mca_mpool_udapl_init(
     struct mca_mpool_base_resources_t* resources)
{
    mca_mpool_udapl_module_t* udapl_mpool; 

    opal_output(0, "mpool_udapl_init\n");

    mca_base_param_reg_string(&mca_mpool_udapl_component.super.mpool_version, 
                              "rcache_name", 
                              "The name of the registration cache the mpool should use", 
                              false, 
                              false, 
                              "rb", 
                              &(mca_mpool_udapl_component.rcache_name)); 

    udapl_mpool = (mca_mpool_udapl_module_t*)malloc(sizeof(mca_mpool_udapl_module_t)); 
    mca_mpool_udapl_module_init(udapl_mpool); 
    udapl_mpool->udapl_res = *resources;
    return &udapl_mpool->super;
}


