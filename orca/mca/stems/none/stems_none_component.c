/*
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orca_config.h"

#include "orca/constants.h"

#include "opal/mca/mca.h"

#include "orca/mca/stems/stems.h"
#include "orca/mca/stems/base/base.h"
#include "stems_none.h"

/*
 * Public string for version number
 */
const char *orca_stems_none_component_version_string = 
    "ORCA STEMS none MCA component version " ORCA_VERSION;

/*
 * Local functionality
 */
static int stems_none_open(void);
static int stems_none_close(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointer to our public functions in it
 */
orca_stems_none_component_t mca_stems_none_component = {
    /* First do the base component stuff */
    {
        /* Handle the general mca_component_t struct containing 
         *  meta information about the component itnone
         */
        {
            ORCA_STEMS_BASE_VERSION_1_0_0,

            /* Component name and version */
            "none",
            ORCA_MAJOR_VERSION,
            ORCA_MINOR_VERSION,
            ORCA_RELEASE_VERSION,
            
            /* Component open and close functions */
            stems_none_open,
            stems_none_close,
            orca_stems_none_component_query
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        /* Verbosity level */
        0,
        /* opal_output handler */
        -1,
        /* Default priority */
        1
    }
};

/*
 * None module
 */
static orca_stems_base_module_t loc_module = {
    /** Initialization Function */
    orca_stems_none_module_init,
    /** Finalization Function */
    orca_stems_none_module_finalize,

    /** Process Name Functions */
    orca_stems_none_name_print,
    orca_stems_none_name_to_string,
    orca_stems_none_name_from_string,
    orca_stems_none_name_hash,
    orca_stems_none_name_compare,

    /** Process Information */
    orca_stems_none_info_proc_is_bound,
    orca_stems_none_info_proc_get_applied_binding,

    /** General Information Functions */
    orca_stems_none_info_create_session_dirs,
    orca_stems_none_info_in_parallel_debugger,
    orca_stems_none_info_standalone_operation,
    orca_stems_none_info_cr_continue_like_restart,
    orca_stems_none_info_job_identifier,

    /** Out-Of-Band Communication */
    orca_stems_none_oob_send_buffer,
    orca_stems_none_oob_send_buffer_nb,
    orca_stems_none_oob_recv_buffer,
    orca_stems_none_oob_recv_buffer_nb,
    orca_stems_none_oob_recv_cancel,
    orca_stems_none_oob_parse_uris,

    /** Remote Process Information */
    orca_stems_none_proc_get_locality,
    orca_stems_none_proc_get_hostname,
    orca_stems_none_proc_get_node_rank,

    /** Error Manager */
    orca_stems_none_error_log,
    orca_stems_none_error_set_callback,
    orca_stems_none_error_abort,
    orca_stems_none_error_abort_peers,

    /** Collectives */
    orca_stems_none_coll_modex,
    orca_stems_none_coll_barrier,
    orca_stems_none_coll_set_attribute,
    orca_stems_none_coll_get_attribute,

    /** Notifier / Show Help */
    orca_stems_none_notifier_show_help,
    orca_stems_none_notifier_show_help_avail,
    orca_stems_none_notifier_show_help_want_aggregate
};

static int stems_none_open(void) 
{
    return OPAL_SUCCESS;
}

static int stems_none_close(void)
{
    return OPAL_SUCCESS;
}

int orca_stems_none_component_query(mca_base_module_t **module, int *priority)
{
    *module   = (mca_base_module_t *)&loc_module;
    *priority = mca_stems_none_component.super.priority;

    return OPAL_SUCCESS;
}

