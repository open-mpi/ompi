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
#include "opal/util/output.h"
#include "orte/constants.h"

#include "orte/mca/sstore/sstore.h"
#include "orte/mca/sstore/base/base.h"
#include "sstore_stage.h"

/*
 * Public string for version number
 */
const char *orte_sstore_stage_component_version_string = 
    "ORTE SSTORE stage MCA component version " ORTE_VERSION;

/*
 * Local functionality
 */
static int sstore_stage_register (void);
static int sstore_stage_open(void);
static int sstore_stage_close(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointer to our public functions in it
 */
orte_sstore_stage_component_t mca_sstore_stage_component = {
    /* First do the base component stuff */
    {
        /* Handle the general mca_component_t struct containing 
         *  meta information about the component itstage
         */
        {
            ORTE_SSTORE_BASE_VERSION_2_0_0,
            /* Component name and version */
            "stage",
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,
            
            /* Component open and close functions */
            sstore_stage_open,
            sstore_stage_close,
            orte_sstore_stage_component_query,
            sstore_stage_register
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        /* Verbosity level */
        0,
        /* opal_output handler */
        -1
    },
};

char * orte_sstore_stage_local_snapshot_dir = NULL;
bool   orte_sstore_stage_global_is_shared = false;
bool   orte_sstore_stage_skip_filem = false;
bool   orte_sstore_stage_enabled_caching = false;
bool   orte_sstore_stage_enabled_compression = false;
int    orte_sstore_stage_compress_delay = 0;
int    orte_sstore_stage_progress_meter = 0;

static int sstore_stage_select (void)
{
    mca_base_component_t *component = &mca_sstore_stage_component.super.base_version;
    int mca_index;

    /*
     * The local directory to use when staging checkpoints back to central storage
     */
    orte_sstore_stage_local_snapshot_dir = strdup (opal_tmp_directory());
    mca_index = mca_base_component_var_register (component, "local_snapshot_dir",
                                                 "The temporary base directory to use when storing local snapshots before they are moved.",
                                                 MCA_BASE_VAR_TYPE_STRING, NULL, 0, MCA_BASE_VAR_FLAG_INTERNAL,
                                                 OPAL_INFO_LVL_9,
                                                 MCA_BASE_VAR_SCOPE_READONLY,
                                                 &orte_sstore_stage_local_snapshot_dir);
    (void) mca_base_var_register_synonym (mca_index, "orte", "crs", "base", "snapshot_dir", MCA_BASE_VAR_SYN_FLAG_DEPRECATED);

    /*
     * If the global storage is just on a different file system, then we pass
     * this hint on to FileM.
     */
    orte_sstore_stage_global_is_shared = false;
    mca_index = mca_base_component_var_register (component, "global_is_shared",
                                                 "If the global_snapshot_dir is on a shared file system all nodes can access, "
                                                 "then the checkpoint files can be copied more efficiently when FileM is used."
                                                 " [Default = disabled]", MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                                 OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                                 &orte_sstore_stage_global_is_shared);
    (void) mca_base_var_register_synonym (mca_index, "orte", "snapc", "base", "global_shared", MCA_BASE_VAR_SYN_FLAG_DEPRECATED);

    /*
     * Debugging option to skip the filem step
     * Warning: Will not produce a usable global snapshot
     */
    orte_sstore_stage_skip_filem = false;
    mca_index = mca_base_component_var_register (component, "skip_filem",
                                                 "Not for general use! For debugging only! Pretend to move files. [Default = disabled]",
                                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                                 OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                                 &orte_sstore_stage_skip_filem);
    (void) mca_base_var_register_synonym (mca_index, "orte", "snapc", "base", "skip_filem", MCA_BASE_VAR_SYN_FLAG_DEPRECATED);

    /*
     * Maintain a local cache of checkpoints taken, so that automatic recovery
     * does not require a transfer from central storage.
     */
    orte_sstore_stage_enabled_caching = false;
    mca_index = mca_base_component_var_register (component, "caching",
                                                 "Maintain a node local cache of last checkpoint. [Default = disabled]",
                                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                                 OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                                 &orte_sstore_stage_enabled_caching);

    /*
     * Compress checkpoints before/after transfer
     */
    orte_sstore_stage_enabled_compression = false;
    (void) mca_base_component_var_register (component, "compress",
                                            "Compress local snapshots. [Default = disabled]",
                                            MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                            OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                            &orte_sstore_stage_enabled_compression);

    /*
     * Number of seconds to delay the start of compression when sync'ing
     */
    orte_sstore_stage_compress_delay = 0;
    (void) mca_base_component_var_register (component, "compress_delay",
                                            "Seconds to delay the start of compression on sync() "
                                            " [Default = 0]",
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                            OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                            &orte_sstore_stage_compress_delay);

    /*
     * A progress meter
     */
    orte_sstore_stage_progress_meter = 0;
    (void) mca_base_component_var_register (component, "progress_meter",
                                            "Display Progress every X percentage done. [Default = 0/off]",
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                            OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                            &orte_sstore_stage_progress_meter);
    orte_sstore_stage_progress_meter = (orte_sstore_stage_progress_meter % 101);

    /*
     * Priority
     */
    mca_sstore_stage_component.super.priority = 10;
    (void) mca_base_component_var_register (component, "priority", "Priority of the SSTORE stage component",
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                            OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                            &mca_sstore_stage_component.super.priority);

    /*
     * Verbose Level
     */
    (void) mca_base_component_var_register (component, "verbose",
                                            "Verbose level for the SSTORE stage component",
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                            OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                            &mca_sstore_stage_component.super.verbose);

    return ORTE_SUCCESS;
}

static int sstore_stage_open(void) 
{
    int mca_index, value;

    /* If there is a custom verbose level for this component than use it
     * otherwise take our parents level and output channel
     */
    if ( 0 != mca_sstore_stage_component.super.verbose) {
        mca_sstore_stage_component.super.output_handle = opal_output_open(NULL);
        opal_output_set_verbosity(mca_sstore_stage_component.super.output_handle,
                                  mca_sstore_stage_component.super.verbose);
    } else {
        mca_sstore_stage_component.super.output_handle = orte_sstore_base_framework.framework_output;
    }
    
    /*
     * Debug Output
     */
    opal_output_verbose(10, mca_sstore_stage_component.super.output_handle,
                        "sstore:stage: open()");
    opal_output_verbose(20, mca_sstore_stage_component.super.output_handle,
                        "sstore:stage: open: priority   = %d", 
                        mca_sstore_stage_component.super.priority);
    opal_output_verbose(20, mca_sstore_stage_component.super.output_handle,
                        "sstore:stage: open: verbosity  = %d", 
                        mca_sstore_stage_component.super.verbose);
    opal_output_verbose(20, mca_sstore_stage_component.super.output_handle,
                        "sstore:stage: open: Local snapshot directory = %s", 
                        orte_sstore_stage_local_snapshot_dir);
    opal_output_verbose(20, mca_sstore_stage_component.super.output_handle,
                        "sstore:stage: open: Is Global dir. shared    = %s", 
                        (orte_sstore_stage_global_is_shared ? "True" : "False"));
    opal_output_verbose(20, mca_sstore_stage_component.super.output_handle,
                        "sstore:stage: open: Node Local Caching       = %s", 
                        (orte_sstore_stage_enabled_caching ? "Enabled" : "Disabled"));
    opal_output_verbose(20, mca_sstore_stage_component.super.output_handle,
                        "sstore:stage: open: Compression              = %s", 
                        (orte_sstore_stage_enabled_compression ? "Enabled" : "Disabled"));
    opal_output_verbose(20, mca_sstore_stage_component.super.output_handle,
                        "sstore:stage: open: Compression Delay        = %d", 
                        orte_sstore_stage_compress_delay);
    opal_output_verbose(20, mca_sstore_stage_component.super.output_handle,
                        "sstore:stage: open: Skip FileM (Debug Only)  = %s", 
                        (orte_sstore_stage_skip_filem ? "True" : "False"));

    return ORTE_SUCCESS;
}

static int sstore_stage_close(void)
{
    opal_output_verbose(10, mca_sstore_stage_component.super.output_handle,
                        "sstore:stage: close()");

    return ORTE_SUCCESS;
}
