/*
 * Copyright (C) Mellanox Technologies Ltd. 2018. ALL RIGHTS RESERVED.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2020      Huawei Technologies Co., Ltd.  All rights
 *                         reserved.
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
#include "opal/mca/pmix/pmix-internal.h"
#include "opal/memoryhooks/memory.h"

#include <ucm/api/ucm.h>

/***********************************************************************/

extern mca_base_framework_t opal_memory_base_framework;

opal_common_ucx_module_t opal_common_ucx = {
    .verbose             = 0,
    .progress_iterations = 100,
    .registered          = 0,
    .opal_mem_hooks      = 0,
    .ref_count           = 0,
    .first_version       = NULL
};

static void opal_common_ucx_mem_release_cb(void *buf, size_t length,
                                           void *cbdata, bool from_alloc)
{
    ucm_vm_munmap(buf, length);
}

OPAL_DECLSPEC void opal_common_ucx_mca_var_register(const mca_base_component_t *component)
{
    static int registered = 0;
    static int hook_index;
    static int verbose_index;
    static int progress_index;
    if (!registered) {
        verbose_index = mca_base_var_register("opal", "opal_common", "ucx", "verbose",
                                              "Verbose level of the UCX components",
                                              MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                              MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
                                              MCA_BASE_VAR_SCOPE_LOCAL,
                                              &opal_common_ucx.verbose);
        progress_index = mca_base_var_register("opal", "opal_common", "ucx", "progress_iterations",
                                               "Set number of calls of internal UCX progress "
                                               "calls per opal_progress call",
                                               MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                               MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
                                               MCA_BASE_VAR_SCOPE_LOCAL,
                                               &opal_common_ucx.progress_iterations);
        hook_index = mca_base_var_register("opal", "opal_common", "ucx", "opal_mem_hooks",
                                           "Use OPAL memory hooks, instead of UCX internal "
                                           "memory hooks", MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &opal_common_ucx.opal_mem_hooks);
        registered = 1;
    }
    if (component) {
        mca_base_var_register_synonym(verbose_index, component->mca_project_name,
                                      component->mca_type_name,
                                      component->mca_component_name,
                                      "verbose", 0);
        mca_base_var_register_synonym(progress_index, component->mca_project_name,
                                      component->mca_type_name,
                                      component->mca_component_name,
                                      "progress_iterations", 0);
        mca_base_var_register_synonym(hook_index, component->mca_project_name,
                                      component->mca_type_name,
                                      component->mca_component_name,
                                      "opal_mem_hooks", 0);
    }
}

OPAL_DECLSPEC void opal_common_ucx_mca_register(void)
{
    int ret;

    opal_common_ucx.registered++;
    if (opal_common_ucx.registered > 1) {
        /* process once */
        return;
    }

    opal_common_ucx.output = opal_output_open(NULL);
    opal_output_set_verbosity(opal_common_ucx.output, opal_common_ucx.verbose);

    /* Set memory hooks */
    if (opal_common_ucx.opal_mem_hooks) {
        ret = mca_base_framework_open(&opal_memory_base_framework, 0);
        if (OPAL_SUCCESS != ret) {
            /* failed to initialize memory framework - just exit */
            MCA_COMMON_UCX_VERBOSE(1, "failed to initialize memory base framework: %d, "
                                      "memory hooks will not be used", ret);
            return;
        }

        if ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) ==
            ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) &
             opal_mem_hooks_support_level())) {
            MCA_COMMON_UCX_VERBOSE(1, "%s", "using OPAL memory hooks as external events");
            ucm_set_external_event(UCM_EVENT_VM_UNMAPPED);
            opal_mem_hooks_register_release(opal_common_ucx_mem_release_cb, NULL);
        }
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

#if HAVE_DECL_UCM_TEST_EVENTS
static ucs_status_t opal_common_ucx_mca_test_external_events(int events)
{
#if HAVE_DECL_UCM_TEST_EXTERNAL_EVENTS
    return ucm_test_external_events(UCM_EVENT_VM_UNMAPPED);
#else
    return ucm_test_events(UCM_EVENT_VM_UNMAPPED);
#endif
}

static void opal_common_ucx_mca_test_events(void)
{
    static int warned = 0;
    const char *suggestion;
    ucs_status_t status;

    if (!warned) {
        if (opal_common_ucx.opal_mem_hooks) {
            suggestion = "Please check OPAL memory events infrastructure.";
            status     = opal_common_ucx_mca_test_external_events(UCM_EVENT_VM_UNMAPPED);
        } else {
            suggestion = "Pls try adding --mca opal_common_ucx_opal_mem_hooks 1 "
                         "to mpirun/oshrun command line to resolve this issue.";
            status     = ucm_test_events(UCM_EVENT_VM_UNMAPPED);
        }

        if (status != UCS_OK) {
            MCA_COMMON_UCX_WARN("UCX is unable to handle VM_UNMAP event. "
                                "This may cause performance degradation or data "
                                "corruption. %s", suggestion);
            warned = 1;
        }
    }
}
#endif

void opal_common_ucx_mca_proc_added(void)
{
#if HAVE_DECL_UCM_TEST_EVENTS
    opal_common_ucx_mca_test_events();
#endif
}

OPAL_DECLSPEC int opal_common_ucx_mca_pmix_fence_nb(int *fenced)
{
    return PMIx_Fence_nb(NULL, 0, NULL, 0, opal_common_ucx_mca_fence_complete_cb, (void *)fenced);
}

OPAL_DECLSPEC int opal_common_ucx_mca_pmix_fence(ucp_worker_h worker)
{
    volatile int fenced = 0;
    int ret = OPAL_SUCCESS;

    if (OPAL_SUCCESS != (ret = PMIx_Fence_nb(NULL, 0, NULL, 0,
                    opal_common_ucx_mca_fence_complete_cb, (void*)&fenced))){
        return ret;
    }

    while (!fenced) {
        ucp_worker_progress(worker);
    }

    return ret;
}


static void opal_common_ucx_wait_all_requests(void **reqs, int count,
        ucp_worker_h worker, enum opal_common_ucx_req_type type)
{
    int i;

    MCA_COMMON_UCX_VERBOSE(2, "waiting for %d disconnect requests", count);
    for (i = 0; i < count; ++i) {
        opal_common_ucx_wait_request(reqs[i], worker, type, "ucp_disconnect_nb");
        reqs[i] = NULL;
    }
}

OPAL_DECLSPEC int opal_common_ucx_del_procs_nofence(opal_common_ucx_del_proc_t *procs,
                                                    size_t count, size_t my_rank,
                                                    size_t max_disconnect,
                                                    ucp_worker_h worker)
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
                    opal_common_ucx_wait_all_requests(dreqs, num_reqs, worker,
                            OPAL_COMMON_UCX_REQUEST_TYPE_UCP);
                    num_reqs = 0;
                }
            }
        }
    }
    /* num_reqs == 0 is processed by opal_common_ucx_wait_all_requests routine,
     * so suppress coverity warning */
    /* coverity[uninit_use_in_call] */
    opal_common_ucx_wait_all_requests(dreqs, num_reqs, worker,
            OPAL_COMMON_UCX_REQUEST_TYPE_UCP);
    free(dreqs);

    return OPAL_SUCCESS;
}

OPAL_DECLSPEC int opal_common_ucx_del_procs(opal_common_ucx_del_proc_t *procs, size_t count,
                                            size_t my_rank, size_t max_disconnect, ucp_worker_h worker)
{
    opal_common_ucx_del_procs_nofence(procs, count, my_rank, max_disconnect, worker);

    return opal_common_ucx_mca_pmix_fence(worker);
}


#if HAVE_UCP_WORKER_ADDRESS_FLAGS
static int opal_common_ucx_send_worker_address_type(const mca_base_component_t *version,
                                                    int addr_flags, int modex_scope)
{
    ucs_status_t status;
    ucp_worker_attr_t attrs;
    int rc;

    attrs.field_mask    = UCP_WORKER_ATTR_FIELD_ADDRESS |
                          UCP_WORKER_ATTR_FIELD_ADDRESS_FLAGS;
    attrs.address_flags = addr_flags;

    status = ucp_worker_query(opal_common_ucx.ucp_worker, &attrs);
    if (UCS_OK != status) {
        MCA_COMMON_UCX_ERROR("Failed to query UCP worker address");
        return OPAL_ERROR;
    }

    OPAL_MODEX_SEND(rc, modex_scope, version, (void*)attrs.address, attrs.address_length);

    ucp_worker_release_address(opal_common_ucx.ucp_worker, attrs.address);

    if (OPAL_SUCCESS != rc) {
        return OPAL_ERROR;
    }

    MCA_COMMON_UCX_VERBOSE(2, "Pack %s worker address, size %ld",
                    (modex_scope == PMIX_LOCAL) ? "local" : "remote",
                    attrs.address_length);

    return OPAL_SUCCESS;
}
#endif

static int opal_common_ucx_send_worker_address(const mca_base_component_t *version)
{
    ucs_status_t status;

#if !HAVE_UCP_WORKER_ADDRESS_FLAGS
    ucp_address_t *address;
    size_t addrlen;
    int rc;

    status = ucp_worker_get_address(opal_common_ucx.ucp_worker, &address, &addrlen);
    if (UCS_OK != status) {
        MCA_COMMON_UCX_ERROR("Failed to get worker address");
        return OPAL_ERROR;
    }

    MCA_COMMON_UCX_VERBOSE(2, "Pack worker address, size %ld", addrlen);

    OPAL_MODEX_SEND(rc, PMIX_GLOBAL, version, (void*)address, addrlen);

    ucp_worker_release_address(opal_common_ucx.ucp_worker, address);

    if (OPAL_SUCCESS != rc) {
        goto err;
    }
#else
    /* Pack just network device addresses for remote node peers */
    status = opal_common_ucx_send_worker_address_type(version,
                                                      UCP_WORKER_ADDRESS_FLAG_NET_ONLY,
                                                      PMIX_REMOTE);
    if (UCS_OK != status) {
        goto err;
    }

    status = opal_common_ucx_send_worker_address_type(version, 0, PMIX_LOCAL);
    if (UCS_OK != status) {
        goto err;
    }
#endif

    return OPAL_SUCCESS;

err:
    MCA_COMMON_UCX_ERROR("Open MPI couldn't distribute EP connection details");
    return OPAL_ERROR;
}

int opal_common_ucx_recv_worker_address(opal_process_name_t *proc_name,
                                        ucp_address_t **address_p,
                                        size_t *addrlen_p)
{
    int ret;

    const mca_base_component_t *version = opal_common_ucx.first_version;

    *address_p = NULL;
    OPAL_MODEX_RECV(ret, version, proc_name, (void**)address_p, addrlen_p);
    if (ret < 0) {
        MCA_COMMON_UCX_ERROR("Failed to receive UCX worker address: %s (%d)",
                      opal_strerror(ret), ret);
    }

    return ret;
}

int opal_common_ucx_open(const char *prefix,
                         const ucp_params_t *ucp_params,
                         size_t *request_size)
{
    ucp_context_attr_t attr;
    ucs_status_t status;

    MCA_COMMON_UCX_VERBOSE(1, "opal_common_ucx_open");

    if (opal_common_ucx.first_version != NULL) {
        goto query;
    }

    ucp_config_t *config;
    status = ucp_config_read("MPI", NULL, &config);
    if (UCS_OK != status) {
        return OPAL_ERROR;
    }

    status = ucp_init(&ucp_params, config, &opal_common_ucx.ucp_context);
    ucp_config_release(config);

    if (UCS_OK != status) {
        return OPAL_ERROR;
    }

    /* Initialize CUDA, if supported */
    opal_common_ucx.cuda_initialized = false;
#if HAVE_UCP_ATTR_MEMORY_TYPES && OPAL_CUDA_SUPPORT
    if (attr.memory_types & UCS_BIT(UCS_MEMORY_TYPE_CUDA)) {
        mca_common_cuda_stage_one_init();
        opal_common_ucx.cuda_initialized = true;
    }
#endif

query:
    /* Query UCX attributes */
    attr.field_mask  = UCP_ATTR_FIELD_REQUEST_SIZE;
#if HAVE_UCP_ATTR_MEMORY_TYPES
    attr.field_mask |= UCP_ATTR_FIELD_MEMORY_TYPES;
#endif
    status = ucp_context_query(opal_common_ucx.ucp_context, &attr);
    if (UCS_OK != status) {
        goto cleanup_ctx;
    }

    *request_size = attr.request_size;

    return OPAL_SUCCESS;

cleanup_ctx:
    ucp_cleanup(opal_common_ucx.ucp_context);
    return OPAL_ERROR;
}

int opal_common_ucx_close(void)
{
    MCA_COMMON_UCX_VERBOSE(1, "opal_common_ucx_close");

#if OPAL_CUDA_SUPPORT
    if (opal_common_ucx.cuda_initialized) {
        mca_common_cuda_fini();
    }
#endif

    if (opal_common_ucx.ucp_context != NULL) {
        ucp_cleanup(opal_common_ucx.ucp_context);
        opal_common_ucx.ucp_context = NULL;
    }

    return OPAL_SUCCESS;
}

static int opal_common_ucx_init_worker(int enable_mpi_threads)
{
    ucp_worker_params_t params;
    ucp_worker_attr_t attr;
    ucs_status_t status;
    int rc;


    /* TODO check MPI thread mode */
    params.field_mask  = UCP_WORKER_PARAM_FIELD_THREAD_MODE;
    if (enable_mpi_threads) {
        params.thread_mode = UCS_THREAD_MODE_MULTI;
    } else {
        params.thread_mode = UCS_THREAD_MODE_SINGLE;
    }

    status = ucp_worker_create(opal_common_ucx.ucp_context, &params,
                               &opal_common_ucx.ucp_worker);
    if (UCS_OK != status) {
        MCA_COMMON_UCX_ERROR("Failed to create UCP worker");
        return OPAL_ERROR;
    }

    attr.field_mask = UCP_WORKER_ATTR_FIELD_THREAD_MODE;
    status = ucp_worker_query(opal_common_ucx.ucp_worker, &attr);
    if (UCS_OK != status) {
        MCA_COMMON_UCX_ERROR("Failed to query UCP worker thread level");
        rc = OPAL_ERROR;
        goto err_destroy_worker;
    }

    if (enable_mpi_threads && (attr.thread_mode != UCS_THREAD_MODE_MULTI)) {
        /* UCX does not support multithreading, disqualify current PML for now */
        /* TODO: we should let OMPI to fallback to THREAD_SINGLE mode */
        MCA_COMMON_UCX_VERBOSE(1, "UCP worker does not support MPI_THREAD_MULTIPLE");
        rc = OPAL_ERR_NOT_SUPPORTED;
        goto err_destroy_worker;
    }

    MCA_COMMON_UCX_VERBOSE(2, "created ucp context %p, worker %p",
                           (void *)opal_common_ucx.ucp_context,
                           (void *)opal_common_ucx.ucp_worker);

    return OPAL_SUCCESS;

err_destroy_worker:
    ucp_worker_destroy(opal_common_ucx.ucp_worker);
    return rc;
}

static int opal_common_ucx_progress(void)
{
    return (int) ucp_worker_progress(opal_common_ucx.ucp_worker);
}

int opal_common_ucx_init(int enable_mpi_threads,
                         const mca_base_component_t *version)
{
    int rc;

    if (opal_common_ucx.ref_count++ > 0) {
        return (opal_common_ucx.first_version == NULL) ?
                OPAL_ERROR : OPAL_SUCCESS;
    }

    rc = opal_common_ucx_init_worker(enable_mpi_threads);
    if (rc < 0) {
        return rc;
    }

    /* Early bird gets the worm */
    rc = opal_common_ucx_send_worker_address(version);
    if (rc < 0) {
        MCA_COMMON_UCX_ERROR("Failed to send worker address")
        ucp_worker_destroy(opal_common_ucx.ucp_worker);
    } else {
        opal_common_ucx.first_version = version;
    }

    opal_progress_register(opal_common_ucx_progress);

    return rc;
}

int opal_common_ucx_cleanup(void)
{
    MCA_COMMON_UCX_ASSERT(opal_common_ucx.ref_count > 0);

    if (--opal_common_ucx.ref_count > 0) {
        return OPAL_SUCCESS;
    }

    opal_progress_unregister(opal_common_ucx_progress);

    if (opal_common_ucx.ucp_worker != NULL) {
        ucp_worker_destroy(opal_common_ucx.ucp_worker);
        opal_common_ucx.ucp_worker = NULL;
    }

    return OPAL_SUCCESS;
}
