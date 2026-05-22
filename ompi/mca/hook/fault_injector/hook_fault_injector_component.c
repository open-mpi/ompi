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

static int ompi_hook_fault_injector_open(void){ return OMPI_SUCCESS; }
static int ompi_hook_fault_injector_close(void);
static int ompi_hook_fault_injector_register(void);

ompi_hook_base_component_1_0_0_t mca_hook_fault_injector_component = {
    /* First, the mca_component_t struct containing meta information
     * about the component itself */
    .hookm_version = {
        OMPI_HOOK_BASE_VERSION_1_0_0,
        .mca_component_name = "fault_injector",
        MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                              OMPI_RELEASE_VERSION),

        .mca_open_component = ompi_hook_fault_injector_open,
        .mca_close_component = ompi_hook_fault_injector_close,
        .mca_register_component_params = ompi_hook_fault_injector_register,
    },
    .hookm_data = { MCA_BASE_METADATA_PARAM_CHECKPOINT },

    /* Component functions */
    .hookm_mpi_init_top_post_opal = ompi_hook_fault_injector_mpi_init_top_post_opal,
    .hookm_mpi_init_bottom = ompi_hook_fault_injector_mpi_init_bottom,
};
MCA_BASE_COMPONENT_INIT(ompi, hook, fault_injector)


ompi_hook_fault_injector_module_t ompi_hook_fault_injector_module = { 0 };

static int ompi_hook_fault_injector_register(void)
{
    ompi_hook_base_component_1_0_0_t *component = &mca_hook_fault_injector_component;
    ompi_hook_fault_injector_module_t *module = &ompi_hook_fault_injector_module;

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
        &component->hookm_version, "pause",
        "Pause fault injection while true. Faults that should have been "
        "injected while paused will be injected once unpaused.",
        MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
        OPAL_INFO_LVL_3, MCA_BASE_VAR_SCOPE_LOCAL, &module->pause_injection
    );

    module->injection_signal = SIGKILL;
    (void) mca_base_component_var_register(
        &component->hookm_version, "signal",
        "Signal to be raised for simulating a failure. Default is SIGKILL.",
        MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
        OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL, &module->injection_signal
    );

    (void) mca_base_component_var_register(
        &component->hookm_version, "high_priority",
        "More frequent fault injection checks. Will impact message latency.",
        MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_READONLY, &module->high_priority
    );

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
        MCA_BASE_VAR_TYPE_UINT32_T, NULL, 0, 0, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_READONLY, &module->global_seed
    );

    (void) mca_base_component_var_register(
        &component->hookm_version, "delay_ms",
        "Milliseconds of delay to add to the generated fault times "
        "(to avoid failures during app setup)", MCA_BASE_VAR_TYPE_UINT32_T,
        NULL, 0, 0, OPAL_INFO_LVL_4, MCA_BASE_VAR_SCOPE_READONLY,
        &module->injection_delay_ms
    );

    (void) mca_base_component_var_register(
        &component->hookm_version, "rank_mttf",
        "Mean time to failure for an individual rank, in seconds. A value of "
        "zero disables injection.", MCA_BASE_VAR_TYPE_UINT32_T, NULL, 0, 0,
        OPAL_INFO_LVL_3, MCA_BASE_VAR_SCOPE_READONLY, &module->rank_mttf_s
    );

    return OMPI_SUCCESS;
}

static int ompi_hook_fault_injector_close(void)
{
    ompi_hook_fault_injector_module_t *module = &ompi_hook_fault_injector_module;
    opal_output_close(module->output);
    if (0 != module->rank_mttf_s)
        opal_progress_unregister(ompi_hook_fault_injector_progress);
    *module = (ompi_hook_fault_injector_module_t) {0};
    return OMPI_SUCCESS;
}
