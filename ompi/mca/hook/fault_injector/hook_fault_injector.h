/*
 * Copyright (c) 2026 Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#ifndef MCA_HOOK_FAULT_INJECTOR_H
#define MCA_HOOK_FAULT_INJECTOR_H

#include "ompi_config.h"

#include "ompi/constants.h"

#include "ompi/mca/hook/hook.h"
#include "ompi/mca/hook/base/base.h"

#include "opal/mca/timer/base/base.h"

BEGIN_C_DECLS

void ompi_hook_fault_injector_mpi_init_top_post_opal(int argc, char **argv, int requested, int *provided);

void ompi_hook_fault_injector_mpi_init_bottom(int argc, char **argv, int requested, int *provided);

int ompi_hook_fault_injector_progress(void);

struct ompi_hook_fault_injector_module_t {
    int injection_signal;
    uint32_t global_seed;
    uint32_t rank_mttf_s;
    uint32_t injection_delay_ms;
    bool high_priority;
    bool inject_in_init;
    bool pause_injection;

    // Time offset from start to fail
    opal_timer_t target_injection_offset_us;
    // Absolute time to fail
    opal_timer_t target_injection_time_us;

    int verbose;
    int output;
};
typedef struct ompi_hook_fault_injector_module_t ompi_hook_fault_injector_module_t;


OMPI_DECLSPEC extern ompi_hook_base_component_1_0_0_t mca_hook_fault_injector_component;

OMPI_DECLSPEC extern ompi_hook_fault_injector_module_t ompi_hook_fault_injector_module;


END_C_DECLS

#endif /* MCA_HOOK_FAULT_INJECTOR_H */
