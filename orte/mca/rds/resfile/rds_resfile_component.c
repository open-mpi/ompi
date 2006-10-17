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

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"

#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns_types.h"

#include "orte/mca/rds/base/rds_private.h"
#include "orte/mca/rds/resfile/rds_resfile.h"

/*
 * Local functions
 */

static int orte_rds_resfile_open(void);
static int orte_rds_resfile_close(void);
static orte_rds_base_module_t* orte_rds_resfile_init(void);

orte_rds_resfile_component_t mca_rds_resfile_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a iof v1.0.0 component (which also
           implies a specific MCA version) */

        ORTE_RDS_BASE_VERSION_1_3_0,

        "resfile", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        orte_rds_resfile_open,  /* component open  */
        orte_rds_resfile_close  /* component close */
      },

      /* Next the MCA v1.0.0 component meta data */
      {
        /* Whether the component is checkpointable or not */
        false
      },

      orte_rds_resfile_init,
      orte_rds_resfile_finalize
    }
};


orte_rds_base_module_t orte_rds_resfile_module = {
    orte_rds_resfile_query,
    orte_rds_base_store_resource,
};

/*
 * Instantiate component variables
 */
bool orte_rds_resfile_queried;

/**
  * component open/close/init function
  */
static int orte_rds_resfile_open(void)
{
    OBJ_CONSTRUCT(&mca_rds_resfile_component.lock, opal_mutex_t);

    mca_base_param_reg_int(&mca_rds_resfile_component.super.rds_version, "debug",
                           "Toggle debug output for resfile RDS component",
                           false, false, (int)false, 
                           &mca_rds_resfile_component.debug);
    mca_base_param_reg_string(&mca_rds_resfile_component.super.rds_version, "name",
                              "ORTE Resource filename",
                              false, false, NULL, 
                              &mca_rds_resfile_component.filename);
    
    orte_rds_resfile_queried = false;
    
    return ORTE_SUCCESS;
}


static orte_rds_base_module_t *orte_rds_resfile_init(void)
{
    /* if we are not an HNP, then don't select us */
    if (!orte_process_info.seed) {
        return NULL;
    }

    /* if we are an HNP, then volunteer */
    OBJ_DESTRUCT(&mca_rds_resfile_component.lock);

    return &orte_rds_resfile_module;
}

int orte_rds_resfile_finalize(void)
{
    return ORTE_SUCCESS;
}

/**
 *  Close all subsystems.
 */

static int orte_rds_resfile_close(void)
{
    return ORTE_SUCCESS;
}


