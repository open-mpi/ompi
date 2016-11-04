/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/base/mca_base_var.h"

#include "orte/runtime/orte_globals.h"

#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/plm/base/plm_private.h"
#include "plm_alps.h"


/*
 * Public string showing the plm ompi_alps component version number
 */
const char *mca_plm_alps_component_version_string =
  "Open MPI alps plm MCA component version " ORTE_VERSION;
bool mca_plm_alps_using_aprun = {true};


/*
 * Local functions
 */
static int plm_alps_register(void);
static int plm_alps_open(void);
static int plm_alps_close(void);
static int orte_plm_alps_component_query(mca_base_module_t **module, int *priority);


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

orte_plm_alps_component_t mca_plm_alps_component = {

    {
        /* First, the mca_component_t struct containing meta
           information about the component itself */

        .base_version = {
            ORTE_PLM_BASE_VERSION_2_0_0,

            /* Component name and version */
            .mca_component_name = "alps",
            MCA_BASE_MAKE_VERSION(component, ORTE_MAJOR_VERSION, ORTE_MINOR_VERSION,
                                  ORTE_RELEASE_VERSION),

            /* Component open and close functions */
            .mca_open_component = plm_alps_open,
            .mca_close_component = plm_alps_close,
            .mca_query_component = orte_plm_alps_component_query,
            .mca_register_component_params = plm_alps_register,
        },
        .base_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
    }

    /* Other orte_plm_alps_component_t items -- left uninitialized
       here; will be initialized in plm_alps_open() */
};


static int plm_alps_register(void)
{
    mca_base_component_t *comp = &mca_plm_alps_component.super.base_version;

    mca_plm_alps_component.debug = false;
    (void) mca_base_component_var_register (comp, "debug", "Enable debugging of alps plm",
                                            MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &mca_plm_alps_component.debug);

    if (mca_plm_alps_component.debug == 0) {
        mca_plm_alps_component.debug = orte_debug_flag;
    }

    mca_plm_alps_component.priority = 100;
    (void) mca_base_component_var_register (comp, "priority", "Default selection priority",
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &mca_plm_alps_component.priority);

    mca_plm_alps_component.aprun_cmd = "aprun";
    (void) mca_base_component_var_register (comp, "aprun", "Command to run instead of aprun",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &mca_plm_alps_component.aprun_cmd);

    mca_plm_alps_component.custom_args = NULL;
    (void) mca_base_component_var_register (comp, "args", "Custom arguments to aprun",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &mca_plm_alps_component.custom_args);
    return ORTE_SUCCESS;
}

static int plm_alps_open(void)
{
    return ORTE_SUCCESS;
}


static int orte_plm_alps_component_query(mca_base_module_t **module, int *priority)
{
#if CRAY_WLM_DETECT
    char slurm[]="SLURM";
    char *wlm_detected = NULL;

    wlm_detected = wlm_detect_get_active();

    /*
     * The content of wlm_detected.h indicates wlm_detect_get_active
     * may return NULL upon failure.  Resort to the suggested plan
     * B in that event.
     */

    if (NULL == wlm_detected) {
        wlm_detected = (char *)wlm_detect_get_default();
        OPAL_OUTPUT_VERBOSE((10, orte_plm_base_framework.framework_output,
                             "%s plm:alps: wlm_detect_get_active returned NULL, using %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), wlm_detected));

    }

    if((NULL != wlm_detected) && !strcmp(slurm, wlm_detected)) {
        mca_plm_alps_using_aprun = false;
    }
#endif

    *priority = mca_plm_alps_component.priority;
    *module = (mca_base_module_t *) &orte_plm_alps_module;
    OPAL_OUTPUT_VERBOSE((1, orte_plm_base_framework.framework_output,
                        "%s plm:alps: available for selection",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    return ORTE_SUCCESS;
}


static int plm_alps_close(void)
{
    return ORTE_SUCCESS;
}
