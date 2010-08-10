/*
 * Copyright (c) 2009-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "opal/util/show_help.h"
#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/util/basename.h"
#include "opal/util/argv.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

#include "orte/util/error_strings.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"
#include "opal/dss/dss.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/filem/filem.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/runtime/orte_wait.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"
#include "orte/mca/sstore/sstore.h"
#include "orte/mca/sstore/base/base.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/errmgr/base/errmgr_private.h"

#include "errmgr_example.h"

#include MCA_timer_IMPLEMENTATION_HEADER


/******************
 * Automatic Recovery module
 ******************/
static orte_errmgr_base_module_t global_module = {
    /** Initialization Function */
    orte_errmgr_example_global_module_init,
    /** Finalization Function */
    orte_errmgr_example_global_module_finalize,
    /** Update State */
    orte_errmgr_example_global_update_state,
    orte_errmgr_example_global_predicted_fault,
    /*orte_errmgr_example_global_process_fault,*/
    orte_errmgr_example_global_suggest_map_targets,
    orte_errmgr_example_global_ft_event
};

/************************************
 * Locally Global vars & functions
 ************************************/

/************************
 * Function Definitions
 ************************/
/*
 * MCA Functions
 */
int orte_errmgr_example_component_query(mca_base_module_t **module, int *priority)
{
    if( !(orte_enable_recovery) ) {
        opal_output_verbose(10, mca_errmgr_example_component.super.output_handle,
                            "errmgr:example:component_query() - Disabled: Recovery is not enabled");

        *priority = -1;
        *module = NULL;
        return ORTE_SUCCESS;
    }

    opal_output_verbose(10, mca_errmgr_example_component.super.output_handle,
                        "errmgr:example:component_query()");

    *priority = mca_errmgr_example_component.super.priority;
    if( ORTE_PROC_IS_HNP ) {
        *module = (mca_base_module_t *)&global_module;
    }
    else {
        *module = NULL;
    }

    return ORTE_SUCCESS;
}

/************************
 * Function Definitions
 ************************/
int orte_errmgr_example_global_module_init(void)
{
    opal_output_verbose(10, mca_errmgr_example_component.super.output_handle,
                        "errmgr:example:init()");

    return ORTE_SUCCESS;
}

int orte_errmgr_example_global_module_finalize(void)
{
    opal_output_verbose(10, mca_errmgr_example_component.super.output_handle,
                        "errmgr:example:finalize()");

    return ORTE_SUCCESS;
}

int orte_errmgr_example_global_predicted_fault(opal_list_t *proc_list,
                                               opal_list_t *node_list,
                                               opal_list_t *suggested_map,
                                               orte_errmgr_stack_state_t *stack_state)
{
    opal_output_verbose(10, mca_errmgr_example_component.super.output_handle,
                        "errmgr:example:predicted_fault()");

    return ORTE_SUCCESS;
}

int orte_errmgr_example_global_update_state(orte_jobid_t job,
                                            orte_job_state_t jobstate,
                                            orte_process_name_t *proc_name,
                                            orte_proc_state_t state,
                                            orte_exit_code_t exit_code,
                                            orte_errmgr_stack_state_t *stack_state)
{
    opal_output_verbose(10, mca_errmgr_example_component.super.output_handle,
                        "errmgr:example:update_state(%s)",
                        ORTE_NAME_PRINT(proc_name));

    return ORTE_SUCCESS;
}

int orte_errmgr_example_global_process_fault(orte_job_t *jdata,
                                             orte_process_name_t *proc_name,
                                             orte_proc_state_t state,
                                             orte_errmgr_stack_state_t *stack_state)
{
    opal_output_verbose(10, mca_errmgr_example_component.super.output_handle,
                        "errmgr:example:process_fault(%s)",
                        ORTE_NAME_PRINT(proc_name));

    return ORTE_SUCCESS;
}

int orte_errmgr_example_global_suggest_map_targets(orte_proc_t *proc,
                                                   orte_node_t *oldnode,
                                                   opal_list_t *node_list,
                                                   orte_errmgr_stack_state_t *stack_state)
{
    opal_output_verbose(10, mca_errmgr_example_component.super.output_handle,
                        "errmgr:example:suggest_map_targets()");

    return ORTE_SUCCESS;
}

int orte_errmgr_example_global_ft_event(int state)
{
    return ORTE_SUCCESS;
}

/*****************
 * Local Functions
 *****************/
