/*
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "hook_demo.h"

static int ompi_hook_demo_component_open(void);
static int ompi_hook_demo_component_close(void);
static int ompi_hook_demo_component_register(void);

/*
 * Public string showing the component version number
 */
const char *mca_hook_demo_component_version_string =
    "Open MPI 'demo' hook MCA component version " OMPI_VERSION;

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
const ompi_hook_base_component_1_0_0_t mca_hook_demo_component = {

    /* First, the mca_component_t struct containing meta information
     * about the component itself */
    .hookm_version = {
        OMPI_HOOK_BASE_VERSION_1_0_0,

        /* Component name and version */
        .mca_component_name = "demo",
        MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                              OMPI_RELEASE_VERSION),

        /* Component open and close functions */
        .mca_open_component = ompi_hook_demo_component_open,
        .mca_close_component = ompi_hook_demo_component_close,
        .mca_register_component_params = ompi_hook_demo_component_register,

        // Force this component to always be considered - component must be static
        .mca_component_flags = MCA_BASE_COMPONENT_FLAG_REQUIRED,
    },
    .hookm_data = {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },

    /* Component functions */
    .hookm_mpi_initialized_top = ompi_hook_demo_mpi_initialized_top,
    .hookm_mpi_initialized_bottom = ompi_hook_demo_mpi_initialized_bottom,

    .hookm_mpi_finalized_top = ompi_hook_demo_mpi_finalized_top,
    .hookm_mpi_finalized_bottom = ompi_hook_demo_mpi_finalized_bottom,

    .hookm_mpi_init_top = ompi_hook_demo_mpi_init_top,
    .hookm_mpi_init_top_post_opal = ompi_hook_demo_mpi_init_top_post_opal,
    .hookm_mpi_init_bottom = ompi_hook_demo_mpi_init_bottom,
    .hookm_mpi_init_error = ompi_hook_demo_mpi_init_error,

    .hookm_mpi_finalize_top = ompi_hook_demo_mpi_finalize_top,
    .hookm_mpi_finalize_bottom = ompi_hook_demo_mpi_finalize_bottom,
};

/*
 * Example 'extra' component with an additional callback that is dynamically
 * registered at runtime in addition to the callbacks that are a part of the
 * component structure.
 */
ompi_hook_base_component_1_0_0_t hook_demo_extra_component = {
    /* Component functions */
    .hookm_mpi_init_bottom = ompi_hook_demo_extra_mpi_init_bottom,
};

static int ompi_hook_demo_component_open(void)
{
    opal_output(0, "hook/demo: component_open()");

    // Register the 'extra' callback(s) to be called the next time those
    // functions are encountered.
    ompi_hook_base_register_callbacks( &hook_demo_extra_component );

    return OMPI_SUCCESS;
}

static int ompi_hook_demo_component_close(void)
{
    opal_output(0, "hook/demo: component_close()");

    // Deregister the 'extra' callback(s) so that they are no longer called.
    // Must pass in the same 'component' structure that was passed to the
    // ompi_hook_base_register_callbacks() earlier.
    ompi_hook_base_deregister_callbacks( &hook_demo_extra_component );

    return OMPI_SUCCESS;
}

static int ompi_hook_demo_component_register(void)
{
    opal_output(0, "hook/demo: component_register()");
    return OMPI_SUCCESS;
}

