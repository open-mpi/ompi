/*
 * Copyright (c) 2026 Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "hook_fault_injector.h"
#include "opal/runtime/opal_progress.h"

static int fault_injector_open(void){ return OMPI_SUCCESS; }
static int fault_injector_close(void);
static int fault_injector_register(void);

ompi_hook_base_component_1_0_0_t mca_hook_fault_injector_component = {
    /* First, the mca_component_t struct containing meta information
     * about the component itself */
    .hookm_version = {
        OMPI_HOOK_BASE_VERSION_1_0_0,
        .mca_component_name = "fault_injector",
        MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                              OMPI_RELEASE_VERSION),

        .mca_open_component = fault_injector_open,
        .mca_close_component = fault_injector_close,
        .mca_register_component_params = fault_injector_register,
    },
    .hookm_data = { MCA_BASE_METADATA_PARAM_CHECKPOINT },

    /* Component functions */
    .hookm_mpi_init_top_post_opal = mca_hook_fault_injector_mpi_init_top_post_opal,
    .hookm_mpi_init_bottom = mca_hook_fault_injector_mpi_init_bottom,
};
MCA_BASE_COMPONENT_INIT(ompi, hook, fault_injector)



mca_hook_fault_injector_module_t mca_hook_fault_injector_module = { 0 };

static int fault_injector_register(void)
{
    ompi_hook_base_component_1_0_0_t *component = &mca_hook_fault_injector_component;
    mca_hook_fault_injector_module_t *module = &mca_hook_fault_injector_module;

    // Inherit verbosity of the base framework but allow to be overridden
    module->verbose = ompi_hook_base_framework.framework_verbose;
    if( MCA_BASE_VERBOSE_NONE > module->verbose ) module->verbose = MCA_BASE_VERBOSE_NONE;
    (void) mca_base_component_var_register(
        &component->hookm_version, "verbose", NULL, MCA_BASE_VAR_TYPE_INT, NULL,
        0, 0, OPAL_INFO_LVL_3, MCA_BASE_VAR_SCOPE_READONLY, &module->verbose
    );

    module->output = opal_output_open(NULL);
    opal_output_set_verbosity(module->output, module->verbose);


    (void) mca_base_component_var_register(
        &component->hookm_version, "inject_in_init",
        "Enable fault injections during MPI_Init", MCA_BASE_VAR_TYPE_BOOL, NULL,
        0, 0, OPAL_INFO_LVL_4, MCA_BASE_VAR_SCOPE_READONLY,
        &module->inject_in_init
    );

    (void) mca_base_component_var_register(
        &component->hookm_version, "global_seed",
        "Random number seed to use for generating each rank's fault time. "
        "A value of zero indicates that a random seed should be generated.",
        MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_READONLY, &module->global_seed
    );

    (void) mca_base_component_var_register(
        &component->hookm_version, "injection_delay_ms",
        "Milliseconds of delay to add to the generated fault times "
        "(to avoid failures during app setup)", MCA_BASE_VAR_TYPE_UNSIGNED_INT,
        NULL, 0, 0, OPAL_INFO_LVL_4, MCA_BASE_VAR_SCOPE_READONLY,
        &module->injection_delay_ms
    );

    (void) mca_base_component_var_register(
        &component->hookm_version, "rank_mttf",
        "Mean time to failure for an individual rank, in seconds. A value of "
        "zero disables injection.", MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0, 0,
        OPAL_INFO_LVL_3, MCA_BASE_VAR_SCOPE_READONLY, &module->rank_mttf_s
    );

    return OMPI_SUCCESS;
}

static int fault_injector_close(void)
{
    if (0 != mca_hook_fault_injector_module.rank_mttf_s)
        opal_progress_unregister(mca_hook_fault_injector_progress);
    return OMPI_SUCCESS;
}
