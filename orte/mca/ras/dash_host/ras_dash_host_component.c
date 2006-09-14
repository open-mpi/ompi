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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/orte_constants.h"
#include "orte/util/proc_info.h"
#include "orte/mca/ras/dash_host/ras_dash_host.h"


/*
 * Local functions
 */
static int orte_ras_dash_host_open(void);


orte_ras_dash_host_component_t mca_ras_dash_host_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a ras v1.3.0 component (which also
           implies a specific MCA version) */

        ORTE_RAS_BASE_VERSION_1_3_0,

        "dash_host", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        orte_ras_dash_host_open,  /* component open  */
        NULL
      },

      /* Next the MCA v1.0.0 component meta data */
      {
        /* Whether the component is checkpointable or not */
        false
      },

      orte_ras_dash_host_init
    }
};


/**
  * component open function
  */
static int orte_ras_dash_host_open(void)
{
    mca_base_param_reg_int(&mca_ras_dash_host_component.super.ras_version,
                           "priority",
                           "Selection priority for the dash_host RAS component",
                           false, false, 5,
                           &mca_ras_dash_host_component.priority);

    return ORTE_SUCCESS;
}
