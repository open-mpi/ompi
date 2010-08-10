/*
 * Copyright (c)      2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include "orte/constants.h"
#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"

#include "opal/mca/base/mca_base_param.h"
#include "orte/util/proc_info.h"

#include "orte/mca/sstore/sstore.h"
#include "orte/mca/sstore/base/base.h"

#include "orte/mca/sstore/base/static-components.h"

#if ORTE_DISABLE_FULL_SUPPORT
/* have to include a bogus function here so that
 * the build system sees at least one function
 * in the library
 */
int orte_sstore_base_open(void)
{
    return ORTE_SUCCESS;
}

#else

/*
 * Globals
 */
ORTE_DECLSPEC int  orte_sstore_base_output  = -1;
orte_sstore_base_module_t orte_sstore = {
    NULL, /* sstore_init     */
    NULL, /* ssotore_finalize */

    NULL, /* request_checkpoint_handle    */
    NULL, /* request_restart_handle       */
    NULL, /* request_global_snapshot_data */
    NULL, /* register_handle              */
    NULL, /* get_attr */
    NULL, /* set_attr */
    NULL, /* sync     */
    NULL, /* remove   */
    NULL, /* pack     */
    NULL, /* unpack   */
    NULL, /* fetch_app_deps */
    NULL  /* wait_all_deps  */
};
opal_list_t orte_sstore_base_components_available;
orte_sstore_base_component_t orte_sstore_base_selected_component;
int orte_sstore_context;

bool   orte_sstore_base_is_checkpoint_available = false;
char * orte_sstore_base_local_metadata_filename;
char * orte_sstore_base_global_metadata_filename;
char * orte_sstore_base_local_snapshot_fmt;
char * orte_sstore_base_global_snapshot_dir = NULL;
char * orte_sstore_base_global_snapshot_ref = NULL;
char * orte_sstore_base_prelaunch_location  = NULL;

orte_sstore_base_handle_t orte_sstore_handle_current;
orte_sstore_base_handle_t orte_sstore_handle_last_stable;

/* Determine the context of this module */
int orte_sstore_base_determine_context(void);

/**
 * Function for finding and opening either all MCA components,
 * or the one that was specifically requested via a MCA parameter.
 */
int orte_sstore_base_open(void)
{
    char *str_value = NULL;
    int mca_index;

    orte_sstore_handle_current     = ORTE_SSTORE_HANDLE_INVALID;
    orte_sstore_handle_last_stable = ORTE_SSTORE_HANDLE_INVALID;

    orte_sstore_base_local_metadata_filename  = strdup("snapshot_meta.data");
    orte_sstore_base_global_metadata_filename = strdup("global_snapshot_meta.data");
    orte_sstore_base_local_snapshot_fmt       = strdup("opal_snapshot_%d.ckpt");

    orte_sstore_base_output = opal_output_open(NULL);

    /*
     * Base Global Snapshot directory
     */
    mca_index = mca_base_param_reg_string_name("sstore",
                                               "base_global_snapshot_dir",
                                               "The base directory to use when storing global snapshots",
                                               false, false,
                                               opal_home_directory(),
                                               &orte_sstore_base_global_snapshot_dir);
    mca_base_param_reg_syn_name(mca_index, "snapc", "base_global_snapshot_dir", true);

    /*
     * User defined snapshot reference to use for this job
     */
    mca_index = mca_base_param_reg_string_name("sstore",
                                               "base_global_snapshot_ref",
                                               "The global snapshot reference to be used for this job. "
                                               " [Default = ompi_global_snapshot_MPIRUNPID.ckpt]",
                                               false, false,
                                               NULL,
                                               &orte_sstore_base_global_snapshot_ref);
    mca_base_param_reg_syn_name(mca_index, "snapc", "base_global_snapshot_ref", true);

    /*
     * Old, dead parameters
     */
#if 0
    /* (Should just choose to use the 'central' component
     * Store the checkpoint files in their final location.
     * This assumes that the storage place is on a shared file 
     * system that all nodes can access uniformly.
     * Default = enabled
     */
    mca_base_param_reg_int_name("sstore",
                                "base_store_in_place",
                                "If global_snapshot_dir is on a shared file system all nodes can access, "
                                "then the checkpoint files can be stored in place instead of incurring a "
                                "remote copy. [Default = enabled]",
                                false, false,
                                1,
                                &value);
#endif

#if 0
    OPAL_OUTPUT_VERBOSE((20, orte_sstore_base_output,
                         "sstore:base: open: base_global_snapshot_ref    = %s",
                         orte_sstore_base_global_snapshot_ref));

    /*
     * Pre-establish the global snapshot directory upon job registration
     */
    mca_base_param_reg_int_name("sstore_base",
                                "establish_global_snapshot_dir",
                                "Establish the global snapshot directory on job startup. [Default = disabled]",
                                false, false,
                                0,
                                &value);
    orte_sstore_base_establish_global_snapshot_dir = OPAL_INT_TO_BOOL(value);

    OPAL_OUTPUT_VERBOSE((20, orte_sstore_base_output,
                         "sstore:base: open: base_establish_global_snapshot_dir    = %d",
                         orte_sstore_base_establish_global_snapshot_dir));
#endif

    /*
     * Setup the prelaunch variable to point to the first possible snapshot
     * location
     */
    asprintf(&orte_sstore_base_prelaunch_location,
             "%s/%s/%d",
             orte_sstore_base_global_snapshot_dir,
             orte_sstore_base_global_snapshot_ref,
             0);

    opal_output_verbose(10, orte_sstore_base_output,
                        "sstore:base: open()");
    opal_output_verbose(10, orte_sstore_base_output,
                        "sstore:base: open: Global snapshot directory = %s",
                        orte_sstore_base_global_snapshot_dir);
    opal_output_verbose(10, orte_sstore_base_output,
                        "sstore:base: open: Global snapshot reference = %s",
                        (NULL == orte_sstore_base_global_snapshot_ref ? "Default" : orte_sstore_base_global_snapshot_ref));
    opal_output_verbose(10, orte_sstore_base_output,
                        "sstore:base: open: Prelaunch location        = %s",
                        orte_sstore_base_prelaunch_location);

    /* 
     * Which Sstore component to open
     *  - NULL or "" = auto-select
     *  - ow. select that specific component
     */
    mca_base_param_reg_string_name("sstore", NULL,
                                   "Which Sstore component to use (empty = auto-select)",
                                   false, false,
                                   NULL, &str_value);
    if( NULL != str_value ) {
        free(str_value);
        str_value = NULL;
    }

    /* Open up all available components */
    if (OPAL_SUCCESS !=
        mca_base_components_open("sstore", 
                                 orte_sstore_base_output, 
                                 mca_sstore_base_static_components,
                                 &orte_sstore_base_components_available,
                                 true)) {
        return ORTE_ERROR;
    }

    orte_sstore_context = ORTE_SSTORE_UNASSIGN_TYPE;
    orte_sstore_base_determine_context();

    return ORTE_SUCCESS;
}

int orte_sstore_base_determine_context(void)
{
    if( ORTE_PROC_IS_HNP) {
        orte_sstore_context |= ORTE_SSTORE_GLOBAL_TYPE;
        if( ORTE_PROC_IS_DAEMON ) {
            orte_sstore_context |= ORTE_SSTORE_LOCAL_TYPE;
        }
    }
    else if( ORTE_PROC_IS_DAEMON ) {
        orte_sstore_context |= ORTE_SSTORE_LOCAL_TYPE;
    }
    else if( ORTE_PROC_IS_TOOL ) {
        orte_sstore_context |= ORTE_SSTORE_TOOL_TYPE;
    }
    else if( !ORTE_PROC_IS_DAEMON ) {
        orte_sstore_context |= ORTE_SSTORE_APP_TYPE;
    }

    return ORTE_SUCCESS;
}

#endif
