/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
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
#include "opal/util/output.h"
#include "orte/constants.h"



#include "orte/mca/filem/filem.h"
#include "orte/mca/filem/base/base.h"
#include "filem_rsh.h"

/*
 * Public string for version number
 */
const char *orte_filem_rsh_component_version_string = 
"ORTE FILEM rsh MCA component version " ORTE_VERSION;

/*
 * Local functionality
 */
static int filem_rsh_register(void);
static int filem_rsh_open(void);
static int filem_rsh_close(void);

int orte_filem_rsh_max_incomming = 10;
int orte_filem_rsh_max_outgoing = 10;
int orte_filem_rsh_progress_meter = 0;

/*
 * Instantiate the public struct with all of our public information
 * and pointer to our public functions in it
 */
orte_filem_rsh_component_t mca_filem_rsh_component = {
    /* First do the base component stuff */
    {
        /* Handle the general mca_component_t struct containing 
         *  meta information about the component itrsh
         */
        {
            ORTE_FILEM_BASE_VERSION_2_0_0,
            /* Component name and version */
            "rsh",
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,
            
            /* Component open and close functions */
            filem_rsh_open,
            filem_rsh_close,
            orte_filem_rsh_component_query,
            filem_rsh_register
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
    },

    /* cp_command */
    NULL,

    /* cp_local_command */
    NULL,

    /* remote_sh_command */
    NULL
};

static int filem_rsh_register(void)
{
    mca_base_component_t *component = &mca_filem_rsh_component.super.base_version;
    mca_filem_rsh_component.cp_command = "scp";
    (void) mca_base_component_var_register(component, "rcp",
                                           "The rsh cp command for the FILEM rsh component",
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_filem_rsh_component.cp_command);

    mca_filem_rsh_component.cp_local_command = "cp";
    (void) mca_base_component_var_register(component, "cp",
                                           "The Unix cp command for the FILEM rsh component",
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_filem_rsh_component.cp_local_command);

    mca_filem_rsh_component.remote_sh_command = "ssh";
    (void) mca_base_component_var_register(component, "rsh",
                                           "The remote shell command for the FILEM rsh component",
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_filem_rsh_component.remote_sh_command);

    orte_filem_rsh_max_incomming = 10;
    (void) mca_base_component_var_register(component, "max_incomming",
                                           "Maximum number of incomming connections (0 = any)",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &orte_filem_rsh_max_incomming);

    orte_filem_rsh_max_outgoing = 10;
    (void) mca_base_component_var_register(component, "max_outgoing",
                                           "Maximum number of out going connections (0 = any)",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &orte_filem_rsh_max_outgoing);

    orte_filem_rsh_progress_meter = 0;
    (void) mca_base_component_var_register(component, "progress_meter",
                                           "Display Progress every X percentage done. [Default = 0/off]",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &orte_filem_rsh_progress_meter);

    return ORTE_SUCCESS;
}

static int filem_rsh_open(void) 
{

    if( orte_filem_rsh_max_incomming < 0 ) {
        orte_filem_rsh_max_incomming = 1;
    }

    if( orte_filem_rsh_max_outgoing < 0 ) {
        orte_filem_rsh_max_outgoing = 1;
    }

    orte_filem_rsh_progress_meter = (orte_filem_rsh_progress_meter % 101);

    /*
     * Debug Output
     */
    opal_output_verbose(10, orte_filem_base_output,
                        "filem:rsh: open()");
    opal_output_verbose(20, orte_filem_base_output,
                        "filem:rsh: open: cp command  = %s", 
                        mca_filem_rsh_component.cp_command);
    opal_output_verbose(20, orte_filem_base_output,
                        "filem:rsh: open: cp local command  = %s", 
                        mca_filem_rsh_component.cp_local_command);
    opal_output_verbose(20, orte_filem_base_output,
                        "filem:rsh: open: rsh command  = %s", 
                        mca_filem_rsh_component.remote_sh_command);

    return ORTE_SUCCESS;
}

static int filem_rsh_close(void)
{
    opal_output_verbose(10, orte_filem_base_output,
                        "filem:rsh: close()");

    return ORTE_SUCCESS;
}
