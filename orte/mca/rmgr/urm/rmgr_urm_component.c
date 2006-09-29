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
#include "orte/orte_constants.h"

#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/dss/dss_types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/proc_info.h"
#include "orte/mca/rds/rds.h"
#include "orte/mca/ras/ras.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/pls/pls.h"
#include "orte/mca/rml/rml.h"

#include "orte/mca/rmgr/rmgr.h"
#include "rmgr_urm.h"

/*
 * Local functions
 */

static int orte_rmgr_urm_open(void);
static int orte_rmgr_urm_close(void);
static orte_rmgr_base_module_t* orte_rmgr_urm_init(int *priority);


orte_rmgr_urm_component_t mca_rmgr_urm_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a rmgr v2.0.0 component (which also
           implies a specific MCA version) */

        ORTE_RMGR_BASE_VERSION_2_0_0,

        "urm", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        orte_rmgr_urm_open,  /* component open  */
        orte_rmgr_urm_close  /* component close */
      },

      /* Next the MCA v1.0.0 component meta data */
      {
        /* Whether the component is checkpointable or not */
        false
      },

      orte_rmgr_urm_init
    }
};


/**
  * component open/close/init function
  */
static int orte_rmgr_urm_open(void)
{
    return ORTE_SUCCESS;
}

static orte_rmgr_base_module_t *orte_rmgr_urm_init(int* priority)
{
    int param, value;
    
    /* if we are NOT an HNP, then we do NOT want to be selected */
    if(!orte_process_info.seed) {
        return NULL;
    }

    param = mca_base_param_reg_int_name("orte", "timing",
                                        "Request that critical timing loops be measured",
                                        false, false, 0, &value);
    if (value != 0) {
        mca_rmgr_urm_component.timing = true;
    } else {
        mca_rmgr_urm_component.timing = false;
    }
    
    /* volunteer to be selected */
    *priority = 100;
    return &orte_rmgr_urm_module;
}


/**
 *  Close all subsystems.
 */
static int orte_rmgr_urm_close(void)
{
    return ORTE_SUCCESS;
}
