/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "orte_config.h"
#include "include/orte_constants.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "util/proc_info.h"
#include "opal/util/output.h"
#include "mca/ras/host/ras_host.h"

/*
 * Local functions
 */

static int orte_ras_host_open(void);
static int orte_ras_host_close(void);
static orte_ras_base_module_t* orte_ras_host_init(int*);


orte_ras_host_component_t mca_ras_host_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a iof v1.0.0 component (which also
           implies a specific MCA version) */

        ORTE_RAS_BASE_VERSION_1_0_0,

        "host", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        orte_ras_host_open,  /* component open  */
        orte_ras_host_close  /* component close */
      },

      /* Next the MCA v1.0.0 component meta data */
      {
        /* Whether the component is checkpointable or not */
        false
      },

      orte_ras_host_init
    }
};


/**
  * component open/close/init function
  */
static int orte_ras_host_open(void)
{
    size_t id;

    mca_base_param_reg_int(&mca_ras_host_component.super.ras_version, "debug",
                           "Toggle debug output for Host RAS component",
                           false, false, 1,
                           &mca_ras_host_component.debug);

    mca_base_param_reg_int(&mca_ras_host_component.super.ras_version, "debug",
                           "Selection priority for the Host RAS component",
                           false, false, 1,
                           &mca_ras_host_component.priority);

    /* JMS To be changed post-beta to LAM's C/N command line notation */
    id = mca_base_param_find("ras_base", NULL, "schedule_policy");
    if (0 > id) {
        id = mca_base_param_reg_string_name("ras_base", "schedule_policy",
                                            "Scheduling Policy for RAS. [slot | node]",
                                            false, false, "slot",
                                            &mca_ras_host_component.schedule_policy);
    }
    else {
        mca_base_param_lookup_string(id, &mca_ras_host_component.schedule_policy);
    }

    return ORTE_SUCCESS;
}


static orte_ras_base_module_t *orte_ras_host_init(int* priority)
{
    *priority = mca_ras_host_component.priority;
    return &orte_ras_host_module;
}

/**
 *  Close all subsystems.
 */

static int orte_ras_host_close(void)
{
    if (NULL != mca_ras_host_component.schedule_policy) {
        free(mca_ras_host_component.schedule_policy);
    }

    return ORTE_SUCCESS;
}


