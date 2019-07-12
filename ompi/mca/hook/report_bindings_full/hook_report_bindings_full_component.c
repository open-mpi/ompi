/*
 * Copyright (c) 2019      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "hook_report_bindings_full.h"

static int ompi_hook_report_bindings_full_component_open(void);
static int ompi_hook_report_bindings_full_component_close(void);
static int ompi_hook_report_bindings_full_component_register(void);

/*
 * Public string showing the component version number
 */
const char *mca_hook_report_bindings_full_component_version_string =
    "Open MPI 'report_bindings_full' hook MCA component version " OMPI_VERSION;

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
const ompi_hook_base_component_1_0_0_t mca_hook_report_bindings_full_component = {

    /* First, the mca_component_t struct containing meta information
     * about the component itself */
    .hookm_version = {
        OMPI_HOOK_BASE_VERSION_1_0_0,

        /* Component name and version */
        .mca_component_name = "report_bindings_full",
        MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                              OMPI_RELEASE_VERSION),

        /* Component open and close functions */
        .mca_open_component = ompi_hook_report_bindings_full_component_open,
        .mca_close_component = ompi_hook_report_bindings_full_component_close,
        .mca_register_component_params = ompi_hook_report_bindings_full_component_register,

        // Force this component to always be considered - component must be static
        //.mca_component_flags = MCA_BASE_COMPONENT_FLAG_ALWAYS_CONSIDER,
    },
    .hookm_data = {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },

    /* Component functions */
    .hookm_mpi_initialized_top = NULL,
    .hookm_mpi_initialized_bottom = NULL,

    .hookm_mpi_finalized_top = NULL,
    .hookm_mpi_finalized_bottom = NULL,

    .hookm_mpi_init_top = NULL,
    .hookm_mpi_init_top_post_opal = NULL,
    .hookm_mpi_init_bottom = ompi_hook_report_bindings_full_mpi_init_bottom,
    .hookm_mpi_init_error = NULL,

    .hookm_mpi_finalize_top = NULL,
    .hookm_mpi_finalize_bottom = NULL,
};

int mca_hook_report_bindings_full_verbose = 0;
int mca_hook_report_bindings_full_output  = -1;
bool hook_report_bindings_full_enable_mpi_init = false;
bool hook_report_bindings_full = false;

static int ompi_hook_report_bindings_full_component_open(void)
{
    // Nothing to do
    return OMPI_SUCCESS;
}

static int ompi_hook_report_bindings_full_component_close(void)
{
    // Nothing to do
    return OMPI_SUCCESS;
}

static int ompi_hook_report_bindings_full_component_register(void)
{

    /*
     * Component verbosity level
     */
    // Inherit the verbosity of the base framework, but also allow this to be overridden
    if( ompi_hook_base_framework.framework_verbose > MCA_BASE_VERBOSE_NONE ) {
        mca_hook_report_bindings_full_verbose = ompi_hook_base_framework.framework_verbose;
    }
    else {
        mca_hook_report_bindings_full_verbose = MCA_BASE_VERBOSE_NONE;
    }
    (void) mca_base_component_var_register(&mca_hook_report_bindings_full_component.hookm_version, "verbose",
                                           NULL,
                                           MCA_BASE_VAR_TYPE_INT, NULL,
                                           0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_hook_report_bindings_full_verbose);

    mca_hook_report_bindings_full_output = opal_output_open(NULL);
    opal_output_set_verbosity(mca_hook_report_bindings_full_output, mca_hook_report_bindings_full_verbose);

    /*
     * If the component is active for mpi_init
     */
    hook_report_bindings_full_enable_mpi_init = false;
    (void) mca_base_component_var_register(&mca_hook_report_bindings_full_component.hookm_version, "enable_mpi_init",
                                           "Enable report_bindings_full behavior on mpi_init",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL,
                                           0, 0,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &hook_report_bindings_full_enable_mpi_init);

    // User can set OMPI_MCA_report_bindings_full too
    int hook_report_bindings_full = false;
    (void) mca_base_var_register("ompi", NULL, NULL, "report_bindings_full",
                                 "Enable report_bindings_full behavior at mpi_init",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL,
                                 0, 0,
                                 OPAL_INFO_LVL_3,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &hook_report_bindings_full);

    if (hook_report_bindings_full) {
        hook_report_bindings_full_enable_mpi_init = true;
    }

    return OMPI_SUCCESS;
}

