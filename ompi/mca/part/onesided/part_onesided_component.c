/*
 * Copyright (c) 2026 Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/mca/part/onesided/part_onesided.h"
#include "ompi/mca/part/onesided/part_onesided_component.h"

static int mca_part_onesided_component_register(void) {
    ompi_part_onesided.verbose = 0;
    (void) mca_base_component_var_register(&mca_part_onesided_component.partm_version, "verbose",
                                           "Enable verbose output for onesided part",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READWRITE,
                                           &ompi_part_onesided.verbose);

    ompi_part_onesided.require_rdma = true;
    (void) mca_base_component_var_register(&mca_part_onesided_component.partm_version, "require_rdma",
                                           "Require RDMA capable BTL for onesided part",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READWRITE,
                                           &ompi_part_onesided.require_rdma);

    ompi_part_onesided.max_outstanding_puts = 1024;
    (void) mca_base_component_var_register(&mca_part_onesided_component.partm_version, "max_outstanding_puts",
                                           "Maximum outstanding put operations",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READWRITE,
                                           &ompi_part_onesided.max_outstanding_puts);

    return OPAL_SUCCESS;
}

static int mca_part_onesided_component_open(void) {
    return OMPI_SUCCESS;
}

static int mca_part_onesided_component_close(void) {
    return OMPI_SUCCESS;
}

static mca_part_base_module_t* mca_part_onesided_component_init(int* priority,
                            bool enable_progress_threads, bool enable_mpi_threads) {
    *priority = 10; /* Default lower than direct (which is 1) */
    return &ompi_part_onesided.super;
}

static int mca_part_onesided_component_fini(void) {
    return OMPI_SUCCESS;
}

mca_part_base_component_4_0_0_t mca_part_onesided_component = {
    .partm_version = {
        MCA_PART_BASE_VERSION_2_0_0,
        .mca_component_name = "onesided",
        MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION, OMPI_RELEASE_VERSION),
        .mca_open_component = mca_part_onesided_component_open,
        .mca_close_component = mca_part_onesided_component_close,
        .mca_register_component_params = mca_part_onesided_component_register,
    },
    .partm_data = {
        MCA_BASE_METADATA_PARAM_NONE
    },
    .partm_init = mca_part_onesided_component_init,
    .partm_finalize = mca_part_onesided_component_fini,
};
