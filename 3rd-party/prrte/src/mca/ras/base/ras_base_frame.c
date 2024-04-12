/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2017-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#include "src/event/event-internal.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"
#include "src/runtime/prte_globals.h"

#include "src/mca/ras/base/base.h"
#include "src/mca/ras/base/ras_private.h"

/* NOTE: the RAS does not require a proxy as only the
 * HNP can open the framework in prte_init - non-HNP
 * procs are not allowed to allocate resources
 */

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public pmix_mca_base_component_t struct.
 */

#include "src/mca/ras/base/static-components.h"

/*
 * Global variables
 */
prte_ras_base_t prte_ras_base = {
    .allocation_read = false,
    .active_module = NULL,
    .total_slots_alloc = 0,
    .multiplier = 0,
    .launch_orted_on_hn = false,
    .simulated = false
};

static int ras_register(pmix_mca_base_register_flag_t flags)
{
    PRTE_HIDE_UNUSED_PARAMS(flags);

    prte_ras_base.multiplier = 1;
    pmix_mca_base_var_register("prte", "ras", "base", "multiplier",
                               "Simulate a larger cluster by launching N daemons/node",
                               PMIX_MCA_BASE_VAR_TYPE_INT,
                               &prte_ras_base.multiplier);
#if SLURM_CRAY_ENV
    /*
     * If we are in a Cray-SLURM environment, then we cannot
     * launch procs local to the HNP. The problem
     * is the MPI processes launched on the head node (where the
     * PRTE_PROC_IS_MASTER evalues to true) get launched by a daemon
     * (mpirun) which is not a child of a slurmd daemon.  This
     * means that any RDMA credentials obtained via the odls/alps
     * local launcher are incorrect. Test for this condition. If
     * found, then take steps to ensure we launch a daemon on
     * the same node as mpirun and that it gets used to fork
     * local procs instead of mpirun so they get the proper
     * credential */

    prte_ras_base.launch_orted_on_hn = true;
#else
    prte_ras_base.launch_orted_on_hn = false;
#endif

    pmix_mca_base_var_register("prte", "ras", "base", "launch_orted_on_hn",
                               "Launch an prte daemon on the head node",
                               PMIX_MCA_BASE_VAR_TYPE_BOOL,
                               &prte_ras_base.launch_orted_on_hn);
    return PRTE_SUCCESS;
}

static int prte_ras_base_close(void)
{
    /* Close selected component */
    if (NULL != prte_ras_base.active_module) {
        prte_ras_base.active_module->finalize();
    }

    return pmix_mca_base_framework_components_close(&prte_ras_base_framework, NULL);
}

/**
 *  * Function for finding and opening either all MCA components, or the one
 *   * that was specifically requested via a MCA parameter.
 *    */
static int prte_ras_base_open(pmix_mca_base_open_flag_t flags)
{
    /* Open up all available components */
    return pmix_mca_base_framework_components_open(&prte_ras_base_framework, flags);
}

PMIX_MCA_BASE_FRAMEWORK_DECLARE(prte, ras, "PRTE Resource Allocation Subsystem", ras_register,
                                prte_ras_base_open, prte_ras_base_close,
                                prte_ras_base_static_components,
                                PMIX_MCA_BASE_FRAMEWORK_FLAG_DEFAULT);
