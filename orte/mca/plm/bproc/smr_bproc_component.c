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
#include "orte/util/proc_info.h"
#include "opal/util/output.h"

#include "orte/mca/smr/base/smr_private.h"
#include "orte/mca/smr/bproc/smr_bproc.h"

/*
 * Local functions
 */

static int orte_smr_bproc_open(void);
static int orte_smr_bproc_close(void);
static orte_smr_base_module_t* orte_smr_bproc_init(int *priority);

orte_smr_bproc_component_t mca_smr_bproc_component = {
    {
      /* First, the mca_base_module_t struct containing meta
         information about the module itself */
      {
        /* Indicate that we are a bproc smr v1.3.0 module (which also
           implies a specific MCA version) */

        ORTE_SMR_BASE_VERSION_1_3_0,

        "bproc", /* MCA module name */
        ORTE_MAJOR_VERSION,  /* MCA module major version */
        ORTE_MINOR_VERSION,  /* MCA module minor version */
        ORTE_RELEASE_VERSION,  /* MCA module release version */
        orte_smr_bproc_open,  /* component open */
        orte_smr_bproc_close  /* component close */
      },

      /* Next the MCA v1.0.0 module meta data */

      {
        /* Whether the module is checkpointable or not */

        false
      },

      orte_smr_bproc_init
    }
};

orte_smr_base_module_t orte_smr_bproc_module = {
    orte_smr_base_get_proc_state,
    orte_smr_base_set_proc_state,
    orte_smr_base_get_node_state,
    orte_smr_base_set_node_state,
    orte_smr_base_get_job_state,
    orte_smr_base_set_job_state,
    orte_smr_bproc_begin_monitoring,
    orte_smr_base_init_job_stage_gates,
    orte_smr_base_init_orted_stage_gates,
    orte_smr_base_define_alert_monitor,
    orte_smr_base_job_stage_gate_subscribe,    
    orte_smr_bproc_finalize
};

/**
 * Utility function to register parameters
 */
static int orte_smr_bproc_param_register_int(
    const char* param_name,
    int default_value)
{
    int id = mca_base_param_register_int("smr","bproc",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}

/**
 *
 */

static int orte_smr_bproc_open(void)
{
    mca_smr_bproc_component.debug =
        orte_smr_bproc_param_register_int("debug", 0);
    mca_smr_bproc_component.priority =
        orte_smr_bproc_param_register_int("priority", 1);
    mca_smr_bproc_component.monitoring = false;
    
    return ORTE_SUCCESS;
}

/**
 *
 */

static orte_smr_base_module_t* orte_smr_bproc_init(int *priority)
{
    if (!orte_process_info.seed) {
        return NULL;
    }

    *priority = mca_smr_bproc_component.priority;
    return &orte_smr_bproc_module;
}


/**
 *
 */

static int orte_smr_bproc_close(void)
{
    return ORTE_SUCCESS;
}


