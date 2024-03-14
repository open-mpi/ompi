/*
 * Copyright (c) 2016-2022 IBM Corporation. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "hook_comm_method.h"

static int ompi_hook_comm_method_component_open(void);
static int ompi_hook_comm_method_component_close(void);
static int ompi_hook_comm_method_component_register(void);

/*
 * Public string showing the component version number
 */
const char *mca_hook_comm_method_component_version_string =
    "Open MPI 'comm_method' hook MCA component version " OMPI_VERSION;

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
ompi_hook_base_component_1_0_0_t mca_hook_comm_method_component = {

    /* First, the mca_component_t struct containing meta information
     * about the component itself */
    .hookm_version = {
        OMPI_HOOK_BASE_VERSION_1_0_0,

        /* Component name and version */
        .mca_component_name = "comm_method",
        MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                              OMPI_RELEASE_VERSION),

        /* Component open and close functions */
        .mca_open_component = ompi_hook_comm_method_component_open,
        .mca_close_component = ompi_hook_comm_method_component_close,
        .mca_register_component_params = ompi_hook_comm_method_component_register,

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
    .hookm_mpi_init_bottom = ompi_hook_comm_method_mpi_init_bottom,
    .hookm_mpi_init_error = NULL,

    .hookm_mpi_finalize_top = ompi_hook_comm_method_mpi_finalize_top,
    .hookm_mpi_finalize_bottom = NULL,
};

enum mca_hook_comm_method_mode_flags_t {
    /* Display on MPI_INIT */
    OMPI_HOOK_COMM_METHOD_INIT = 0x01,
    /* Display on MPI_FINALIZE */
    OMPI_HOOK_COMM_METHOD_FINALIZE = 0x02,
};

int mca_hook_comm_method_verbose = 0;
int mca_hook_comm_method_output  = -1;
bool mca_hook_comm_method_enable_mpi_init = false;
bool mca_hook_comm_method_enable_mpi_finalize = false;
uint32_t mca_hook_comm_method_enabled_flags = 0x00;
int mca_hook_comm_method_max = 12;
bool mca_hook_comm_method_brief = false;
char *mca_hook_comm_method_fakefile = NULL;

static mca_base_var_enum_value_flag_t mca_hook_comm_method_modes[] = {
    {.flag = OMPI_HOOK_COMM_METHOD_INIT, .string = "mpi_init"},
    {.flag = OMPI_HOOK_COMM_METHOD_FINALIZE, .string = "mpi_finalize"},
    {0, NULL, 0}
};


static int ompi_hook_comm_method_component_open(void)
{
    // Nothing to do
    return OMPI_SUCCESS;
}

static int ompi_hook_comm_method_component_close(void)
{
    // Nothing to do
    return OMPI_SUCCESS;
}

static int ompi_hook_comm_method_component_register(void)
{
    int ret;
    mca_base_var_enum_flag_t *mca_hook_comm_method_flags = NULL;

    /*
     * Component verbosity level
     */
    // Inherit the verbosity of the base framework, but also allow this to be overridden
    if( ompi_hook_base_framework.framework_verbose > MCA_BASE_VERBOSE_NONE ) {
        mca_hook_comm_method_verbose = ompi_hook_base_framework.framework_verbose;
    }
    else {
        mca_hook_comm_method_verbose = MCA_BASE_VERBOSE_NONE;
    }
    (void) mca_base_component_var_register(&mca_hook_comm_method_component.hookm_version, "verbose",
                                           NULL,
                                           MCA_BASE_VAR_TYPE_INT, NULL,
                                           0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_hook_comm_method_verbose);

    mca_hook_comm_method_output = opal_output_open(NULL);
    opal_output_set_verbosity(mca_hook_comm_method_output, mca_hook_comm_method_verbose);

    /*
     * If the component is active for mpi_init / mpi_finalize via the MCA
     * option: ompi_display_comm
     * We created both a component level version of this parameter: hook_comm_method_display
     * along with a OMPI project level version (ompi_display_comm) for ease of
     * use to enable this feature. The user can fine tune the behavior of this
     * feature using the additional component level MCA options.
     */
    mca_hook_comm_method_enable_mpi_init = false;
    mca_hook_comm_method_enable_mpi_finalize = false;
    mca_base_var_enum_create_flag("ompi_comm_method", mca_hook_comm_method_modes, &mca_hook_comm_method_flags);
    
    ret = mca_base_component_var_register(&mca_hook_comm_method_component.hookm_version, "display",
                                "Enable the communication protocol report: when MPI_INIT is invoked (using the 'mpi_init' value) and/or when MPI_FINALIZE is invoked (using the 'mpi_finalize' value).",
                                MCA_BASE_VAR_TYPE_UNSIGNED_INT,
                                &mca_hook_comm_method_flags->super,
                                0, 0,
                                OPAL_INFO_LVL_3,
                                MCA_BASE_VAR_SCOPE_READONLY,
                                &mca_hook_comm_method_enabled_flags);

    (void) mca_base_var_register_synonym(ret, "ompi", "ompi", NULL, "display_comm", MCA_BASE_VAR_SYN_FLAG_INTERNAL);

    OBJ_RELEASE(mca_hook_comm_method_flags);
    if(OPAL_ERR_VALUE_OUT_OF_BOUNDS == ret) {
        opal_output(0, "hook:comm_method: Warning invalid comm_method specified.");
    }
    else {
        if( mca_hook_comm_method_enabled_flags & OMPI_HOOK_COMM_METHOD_INIT ) {
            mca_hook_comm_method_enable_mpi_init = true;
        }
        if( mca_hook_comm_method_enabled_flags & OMPI_HOOK_COMM_METHOD_FINALIZE ) {
            mca_hook_comm_method_enable_mpi_finalize = true;
        }
    }

    // hook_comm_method_max
    (void) mca_base_component_var_register(&mca_hook_comm_method_component.hookm_version, "max",
                                 "Number of hosts for which to print unabbreviated 2d table of comm methods.",
                                 MCA_BASE_VAR_TYPE_INT, NULL,
                                 0, 0,
                                 OPAL_INFO_LVL_3,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_hook_comm_method_max);
    // hook_comm_method_brief
    (void) mca_base_component_var_register(&mca_hook_comm_method_component.hookm_version, "brief",
                                 "Only print the comm method summary, skip the 2d table.",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL,
                                 0, 0,
                                 OPAL_INFO_LVL_3,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_hook_comm_method_brief);

    // hook_comm_method_fakefile is just for debugging, allows complete override of all the
    // comm method in the table
    (void) mca_base_component_var_register(&mca_hook_comm_method_component.hookm_version, "fakefile",
                                 "For debugging only: read comm methods from a file",
                                 MCA_BASE_VAR_TYPE_STRING, NULL,
                                 0, 0,
                                 OPAL_INFO_LVL_3,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_hook_comm_method_fakefile);

    return OMPI_SUCCESS;
}
