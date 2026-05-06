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
#include "opal/mca/timer/base/base.h"
#include "ompi/runtime/ompi_rte.h"
#include "opal/util/output.h"

#include <stdlib.h>
#include <math.h>   // log()

static void init_injection_checks(void);

void mca_hook_fault_injector_mpi_init_top_post_opal(
    int argc, char **argv, int requested, int *provided
) {
    mca_hook_fault_injector_module_t *module = &mca_hook_fault_injector_module;
    if( 0 == module->rank_mttf_s ) return;

    if( !module->global_seed ) module->global_seed = OMPI_PROC_MY_NAME->jobid;
    if( 0 == OMPI_PROC_MY_NAME->vpid ) opal_output_verbose(
        1, module->output, "hook:fault_injector using global seed %d, rank mttf"
        " %u seconds, injection delay %u ms, init injections %s",
        module->global_seed, module->rank_mttf_s, module->injection_delay_ms,
        module->inject_in_init ? "on" : "off"
    );

    srand(module->global_seed);
    int local_seed = rand() ^ OMPI_PROC_MY_NAME->vpid;
    srand(local_seed);
    double prob = rand() / (RAND_MAX+1.0);
    
    double mttf_us = ((double)module->rank_mttf_s)*1000000;
    double target_offset =
        -log(1-prob)*mttf_us + module->injection_delay_ms*1000;
    module->target_injection_offset_us = target_offset;

    if( module->inject_in_init ) init_injection_checks();
}

void mca_hook_fault_injector_mpi_init_bottom(
    int argc, char **argv, int requested, int *provided
) {
    mca_hook_fault_injector_module_t *module = &mca_hook_fault_injector_module;
    if( 0 != module->rank_mttf_s && !module->inject_in_init )
        init_injection_checks();
}

void init_injection_checks(void)
{
    mca_hook_fault_injector_module_t *module = &mca_hook_fault_injector_module;

    opal_output_verbose(
        4, module->output, "%s hook:fault_injector fault after %f seconds\n",
        OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),
        ((double)module->target_injection_offset_us)/1000000
    );
    
    module->target_injection_time_us =
        opal_timer_base_get_usec() + module->target_injection_offset_us;
    opal_progress_register(mca_hook_fault_injector_progress);
}

int mca_hook_fault_injector_progress(void)
{
    mca_hook_fault_injector_module_t *module = &mca_hook_fault_injector_module;

    opal_timer_t cur_time = opal_timer_base_get_usec();
    if( cur_time < module->target_injection_time_us ) return 0;

    if( opal_output_check_verbosity(3, module->output) ){
        opal_output_verbose(
            3, module->output, "%s hook:fault_injector injecting failure %f "
            "seconds after targetted %f seconds\n",
            OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),
            ((double)cur_time - module->target_injection_time_us)/1000000,
            ((double)module->target_injection_offset_us)/1000000
        );
    } else {
        opal_output_verbose(
            2, module->output, "%s hook:fault_injector injecting failure",
            OMPI_NAME_PRINT(OMPI_PROC_MY_NAME)
        );
    }

    exit(1);
    return 0;
}
