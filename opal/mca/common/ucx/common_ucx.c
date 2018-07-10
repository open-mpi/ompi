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
#include "opal/mca/pmix/pmix.h"

/***********************************************************************/

opal_common_ucx_module_t opal_common_ucx = {
    .verbose             = 0,
    .progress_iterations = 100,
    .registered          = 0
};

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

    opal_common_ucx.output = opal_output_open(NULL);
    opal_output_set_verbosity(opal_common_ucx.output, opal_common_ucx.verbose);
}

OPAL_DECLSPEC void opal_common_ucx_mca_deregister(void)
{
    /* unregister only on last deregister */
    opal_common_ucx.registered--;
    assert(opal_common_ucx.registered >= 0);
    if (opal_common_ucx.registered) {
        return;
    }
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


static void opal_common_ucx_wait_all_requests(void **reqs, int count, ucp_worker_h worker)
{
    int i;

    MCA_COMMON_UCX_VERBOSE(2, "waiting for %d disconnect requests", count);
    for (i = 0; i < count; ++i) {
        opal_common_ucx_wait_request(reqs[i], worker, "ucp_disconnect_nb");
        reqs[i] = NULL;
    }
}

OPAL_DECLSPEC int opal_common_ucx_del_procs(opal_common_ucx_del_proc_t *procs, size_t count,
                                            size_t my_rank, size_t max_disconnect, ucp_worker_h worker)
{
    size_t num_reqs;
    size_t max_reqs;
    void *dreq, **dreqs;
    size_t i;
    size_t n;

    MCA_COMMON_UCX_ASSERT(procs || !count);
    MCA_COMMON_UCX_ASSERT(max_disconnect > 0);

    max_reqs = (max_disconnect > count) ? count : max_disconnect;

    dreqs = malloc(sizeof(*dreqs) * max_reqs);
    if (dreqs == NULL) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    num_reqs = 0;

    for (i = 0; i < count; ++i) {
        n = (i + my_rank) % count;
        if (procs[n].ep == NULL) {
            continue;
        }

        MCA_COMMON_UCX_VERBOSE(2, "disconnecting from rank %zu", procs[n].vpid);
        dreq = ucp_disconnect_nb(procs[n].ep);
        if (dreq != NULL) {
            if (UCS_PTR_IS_ERR(dreq)) {
                MCA_COMMON_UCX_ERROR("ucp_disconnect_nb(%zu) failed: %s", procs[n].vpid,
                                     ucs_status_string(UCS_PTR_STATUS(dreq)));
                continue;
            } else {
                dreqs[num_reqs++] = dreq;
                if (num_reqs >= max_disconnect) {
                    opal_common_ucx_wait_all_requests(dreqs, num_reqs, worker);
                    num_reqs = 0;
                }
            }
        }
    }
    /* num_reqs == 0 is processed by opal_common_ucx_wait_all_requests routine,
     * so suppress coverity warning */
    /* coverity[uninit_use_in_call] */
    opal_common_ucx_wait_all_requests(dreqs, num_reqs, worker);
    free(dreqs);

    opal_common_ucx_mca_pmix_fence(worker);

    return OPAL_SUCCESS;
}

