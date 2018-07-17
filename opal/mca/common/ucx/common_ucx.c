/*
 * Copyright (C) Mellanox Technologies Ltd. 2018. ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "common_ucx.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/base/mca_base_framework.h"
#include "opal/mca/pmix/pmix.h"
#include "opal/memoryhooks/memory.h"

#include <ucm/api/ucm.h>

/***********************************************************************/

extern mca_base_framework_t opal_memory_base_framework;

opal_common_ucx_module_t opal_common_ucx = {
    .verbose             = 0,
    .progress_iterations = 100,
    .registered          = 0,
    .opal_mem_hooks      = 0
};

static void opal_common_ucx_mem_release_cb(void *buf, size_t length,
                                           void *cbdata, bool from_alloc)
{
    ucm_vm_munmap(buf, length);
}

OPAL_DECLSPEC void opal_common_ucx_mca_register(void)
{
    opal_common_ucx.registered++;
    if (opal_common_ucx.registered > 1) {
        /* process once */
        return;
    }

    mca_base_var_register("opal", "opal_common", "ucx", "verbose",
                          "Verbose level of the UCX components",
                          MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                          OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL,
                          &opal_common_ucx.verbose);
    mca_base_var_register("opal", "opal_common", "ucx", "progress_iterations",
                          "Set number of calls of internal UCX progress calls per opal_progress call",
                          MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                          OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL,
                          &opal_common_ucx.progress_iterations);
    mca_base_var_register("opal", "opal_common", "ucx", "opal_mem_hooks",
                          "Use OPAL memory hooks, instead of UCX internal memory hooks",
                          MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                          OPAL_INFO_LVL_3,
                          MCA_BASE_VAR_SCOPE_LOCAL,
                          &opal_common_ucx.opal_mem_hooks);

    opal_common_ucx.output = opal_output_open(NULL);
    opal_output_set_verbosity(opal_common_ucx.output, opal_common_ucx.verbose);

    mca_base_framework_open(&opal_memory_base_framework, 0);

    /* Set memory hooks */
    if (opal_common_ucx.opal_mem_hooks &&
        (OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) ==
        ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) &
         opal_mem_hooks_support_level()))
    {
        MCA_COMMON_UCX_VERBOSE(1, "%s", "using OPAL memory hooks as external events");
        ucm_set_external_event(UCM_EVENT_VM_UNMAPPED);
        opal_mem_hooks_register_release(opal_common_ucx_mem_release_cb, NULL);
    }
}

OPAL_DECLSPEC void opal_common_ucx_mca_deregister(void)
{
    /* unregister only on last deregister */
    opal_common_ucx.registered--;
    assert(opal_common_ucx.registered >= 0);
    if (opal_common_ucx.registered) {
        return;
    }
    opal_mem_hooks_unregister_release(opal_common_ucx_mem_release_cb);
    opal_output_close(opal_common_ucx.output);
}

void opal_common_ucx_empty_complete_cb(void *request, ucs_status_t status)
{
}

static void opal_common_ucx_mca_fence_complete_cb(int status, void *fenced)
{
    *(int*)fenced = 1;
}

OPAL_DECLSPEC void opal_common_ucx_mca_pmix_fence(ucp_worker_h worker)
{
    volatile int fenced = 0;

    opal_pmix.fence_nb(NULL, 0, opal_common_ucx_mca_fence_complete_cb, (void*)&fenced);
    while (!fenced) {
        ucp_worker_progress(worker);
    }
}

