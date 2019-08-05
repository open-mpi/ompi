/*
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#define _GNU_SOURCE
#include <stdio.h>

#include <sys/types.h>
#include <unistd.h>

#include "oshmem_config.h"
#include "orte/util/show_help.h"
#include "shmem.h"
#include "oshmem/runtime/params.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/mca/spml/base/base.h"
#include "spml_ucx_component.h"
#include "oshmem/mca/spml/ucx/spml_ucx.h"

#include "orte/util/show_help.h"
#include "opal/util/opal_environ.h"
#include "opal/runtime/opal_progress_threads.h"

static int mca_spml_ucx_component_register(void);
static int mca_spml_ucx_component_open(void);
static int mca_spml_ucx_component_close(void);
static mca_spml_base_module_t*
mca_spml_ucx_component_init(int* priority,
                              bool enable_progress_threads,
                              bool enable_mpi_threads);
static int mca_spml_ucx_component_fini(void);
mca_spml_base_component_2_0_0_t mca_spml_ucx_component = {

    /* First, the mca_base_component_t struct containing meta
       information about the component itself */

    .spmlm_version = {
        MCA_SPML_BASE_VERSION_2_0_0,

        .mca_component_name            = "ucx",
        .mca_component_major_version   = OSHMEM_MAJOR_VERSION,
        .mca_component_minor_version   = OSHMEM_MINOR_VERSION,
        .mca_component_release_version = OSHMEM_RELEASE_VERSION,
        .mca_open_component            = mca_spml_ucx_component_open,
        .mca_close_component           = mca_spml_ucx_component_close,
        .mca_query_component           = NULL,
        .mca_register_component_params = mca_spml_ucx_component_register
    },
    .spmlm_data = {
        /* The component is checkpoint ready */
        .param_field                   = MCA_BASE_METADATA_PARAM_CHECKPOINT
    },

    .spmlm_init                        = mca_spml_ucx_component_init,
    .spmlm_finalize                    = mca_spml_ucx_component_fini
};


static inline void mca_spml_ucx_param_register_int(const char* param_name,
                                                    int default_value,
                                                    const char *help_msg,
                                                    int *storage)
{
    *storage = default_value;
    (void) mca_base_component_var_register(&mca_spml_ucx_component.spmlm_version,
                                           param_name,
                                           help_msg,
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           storage);
}

static inline void  mca_spml_ucx_param_register_string(const char* param_name,
                                                    char* default_value,
                                                    const char *help_msg,
                                                    char **storage)
{
    *storage = default_value;
    (void) mca_base_component_var_register(&mca_spml_ucx_component.spmlm_version,
                                           param_name,
                                           help_msg,
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           storage);
}

static inline void  mca_spml_ucx_param_register_bool(const char* param_name,
                                                     bool default_value,
                                                     const char *help_msg,
                                                     bool *storage)
{
    *storage = default_value;
    (void) mca_base_component_var_register(&mca_spml_ucx_component.spmlm_version,
                                           param_name,
                                           help_msg,
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           storage);
}

static int mca_spml_ucx_component_register(void)
{
    mca_spml_ucx_param_register_int("priority", 21,
                                    "[integer] ucx priority",
                                    &mca_spml_ucx.priority);

    mca_spml_ucx_param_register_int("num_disconnect", 1,
                                    "How may disconnects go in parallel",
                                    &mca_spml_ucx.num_disconnect);

    mca_spml_ucx_param_register_int("heap_reg_nb", 0,
                                    "Use non-blocking memory registration for shared heap",
                                    &mca_spml_ucx.heap_reg_nb);

    mca_spml_ucx_param_register_bool("async_progress", 0,
                                     "Enable asynchronous progress thread",
                                     &mca_spml_ucx.async_progress);

    mca_spml_ucx_param_register_int("async_tick_usec", 3000,
                                    "Asynchronous progress tick granularity (in usec)",
                                    &mca_spml_ucx.async_tick);

    mca_spml_ucx_param_register_bool("synchronized_quiet", 0,
                                     "Use synchronized quiet on shmem_quiet or shmem_barrier_all operations",
                                     &mca_spml_ucx.synchronized_quiet);

    opal_common_ucx_mca_var_register(&mca_spml_ucx_component.spmlm_version);

    return OSHMEM_SUCCESS;
}

int spml_ucx_ctx_progress(void)
{
    int i;
    for (i = 0; i < mca_spml_ucx.active_array.ctxs_count; i++) {
        ucp_worker_progress(mca_spml_ucx.active_array.ctxs[i]->ucp_worker);
    }
    return 1;
}

int spml_ucx_default_progress(void)
{
    ucp_worker_progress(mca_spml_ucx_ctx_default.ucp_worker);
    return 1;
}

int spml_ucx_progress_aux_ctx(void)
{
    unsigned count;

    if (OPAL_UNLIKELY(!mca_spml_ucx.aux_ctx)) {
        return 0;
    }

    if (pthread_spin_trylock(&mca_spml_ucx.async_lock)) {
        return 0;
    }

    count = ucp_worker_progress(mca_spml_ucx.aux_ctx->ucp_worker);
    pthread_spin_unlock(&mca_spml_ucx.async_lock);

    return count;
}

void mca_spml_ucx_async_cb(int fd, short event, void *cbdata)
{
    int count = 0;

    if (pthread_spin_trylock(&mca_spml_ucx.async_lock)) {
        return;
    }

    do {
        count = ucp_worker_progress(mca_spml_ucx.aux_ctx->ucp_worker);
    }  while (count);

    pthread_spin_unlock(&mca_spml_ucx.async_lock);
}

static int mca_spml_ucx_component_open(void)
{
    return OSHMEM_SUCCESS;
}

static int mca_spml_ucx_component_close(void)
{
    return OSHMEM_SUCCESS;
}

static int spml_ucx_init(void)
{
    ucs_status_t err;
    ucp_config_t *ucp_config;
    ucp_params_t params;
    ucp_context_attr_t attr;
    ucp_worker_params_t wkr_params;
    ucp_worker_attr_t wkr_attr;

    err = ucp_config_read("OSHMEM", NULL, &ucp_config);
    if (UCS_OK != err) {
        return OSHMEM_ERROR;
    }

    opal_common_ucx_mca_register();

    memset(&params, 0, sizeof(params));
    params.field_mask        = UCP_PARAM_FIELD_FEATURES          |
                               UCP_PARAM_FIELD_ESTIMATED_NUM_EPS |
                               UCP_PARAM_FIELD_MT_WORKERS_SHARED;
    params.features          = UCP_FEATURE_RMA   |
                               UCP_FEATURE_AMO32 |
                               UCP_FEATURE_AMO64;
    params.estimated_num_eps = ompi_proc_world_size();
    if (oshmem_mpi_thread_requested == SHMEM_THREAD_MULTIPLE) {
        params.mt_workers_shared = 1;
    } else {
        params.mt_workers_shared = 0;
    }

#if HAVE_DECL_UCP_PARAM_FIELD_ESTIMATED_NUM_PPN
    params.estimated_num_ppn = opal_process_info.num_local_peers + 1;
    params.field_mask       |= UCP_PARAM_FIELD_ESTIMATED_NUM_PPN;
#endif

    err = ucp_init(&params, ucp_config, &mca_spml_ucx.ucp_context);
    ucp_config_release(ucp_config);
    if (UCS_OK != err) {
        return OSHMEM_ERROR;
    }

    attr.field_mask = UCP_ATTR_FIELD_THREAD_MODE;
    err = ucp_context_query(mca_spml_ucx.ucp_context, &attr);
    if (err != UCS_OK) {
        return OSHMEM_ERROR;
    }

    if (oshmem_mpi_thread_requested == SHMEM_THREAD_MULTIPLE &&
        attr.thread_mode != UCS_THREAD_MODE_MULTI) {
        oshmem_mpi_thread_provided = SHMEM_THREAD_SINGLE;
    }

    mca_spml_ucx.active_array.ctxs_count = mca_spml_ucx.idle_array.ctxs_count = 0;
    mca_spml_ucx.active_array.ctxs_num = mca_spml_ucx.idle_array.ctxs_num = MCA_SPML_UCX_CTXS_ARRAY_SIZE;
    mca_spml_ucx.active_array.ctxs = calloc(mca_spml_ucx.active_array.ctxs_num,
                                            sizeof(mca_spml_ucx_ctx_t *));
    mca_spml_ucx.idle_array.ctxs = calloc(mca_spml_ucx.idle_array.ctxs_num,
                                          sizeof(mca_spml_ucx_ctx_t *));

    SHMEM_MUTEX_INIT(mca_spml_ucx.internal_mutex);
    pthread_mutex_init(&mca_spml_ucx.ctx_create_mutex, NULL);

    wkr_params.field_mask  = UCP_WORKER_PARAM_FIELD_THREAD_MODE;
    if (oshmem_mpi_thread_requested == SHMEM_THREAD_MULTIPLE) {
        wkr_params.thread_mode = UCS_THREAD_MODE_MULTI;
    } else {
        wkr_params.thread_mode = UCS_THREAD_MODE_SINGLE;
    }

    err = ucp_worker_create(mca_spml_ucx.ucp_context, &wkr_params,
                            &mca_spml_ucx_ctx_default.ucp_worker);
    if (UCS_OK != err) {
        return OSHMEM_ERROR;
    }

    wkr_attr.field_mask = UCP_WORKER_ATTR_FIELD_THREAD_MODE;
    err = ucp_worker_query(mca_spml_ucx_ctx_default.ucp_worker, &wkr_attr);

    if (oshmem_mpi_thread_requested == SHMEM_THREAD_MULTIPLE &&
        wkr_attr.thread_mode != UCS_THREAD_MODE_MULTI) {
        oshmem_mpi_thread_provided = SHMEM_THREAD_SINGLE;
    }

    if (mca_spml_ucx.async_progress) {
        pthread_spin_init(&mca_spml_ucx.async_lock, 0);
        mca_spml_ucx.async_event_base = opal_progress_thread_init(NULL);
        if (NULL == mca_spml_ucx.async_event_base) {
            SPML_UCX_ERROR("failed to init async progress thread");
            return OSHMEM_ERROR;
        }

        mca_spml_ucx.tick_event = opal_event_alloc();
        opal_event_set(mca_spml_ucx.async_event_base, mca_spml_ucx.tick_event,
                       -1, EV_PERSIST, mca_spml_ucx_async_cb, NULL);
    }

    mca_spml_ucx.aux_ctx    = NULL;
    mca_spml_ucx.aux_refcnt = 0;

    oshmem_ctx_default = (shmem_ctx_t) &mca_spml_ucx_ctx_default;

    return OSHMEM_SUCCESS;
}

static mca_spml_base_module_t*
mca_spml_ucx_component_init(int* priority,
                              bool enable_progress_threads,
                              bool enable_mpi_threads)
{
    SPML_UCX_VERBOSE( 10, "in ucx, my priority is %d\n", mca_spml_ucx.priority);

    if ((*priority) > mca_spml_ucx.priority) {
        *priority = mca_spml_ucx.priority;
        return NULL ;
    }
    *priority = mca_spml_ucx.priority;

    if (OSHMEM_SUCCESS != spml_ucx_init())
        return NULL ;

    SPML_UCX_VERBOSE(50, "*** ucx initialized ****");
    return &mca_spml_ucx.super;
}

static void _ctx_cleanup(mca_spml_ucx_ctx_t *ctx)
{
    int i, j, nprocs = oshmem_num_procs();
    opal_common_ucx_del_proc_t *del_procs;

    del_procs = malloc(sizeof(*del_procs) * nprocs);

    for (i = 0; i < nprocs; ++i) {
        for (j = 0; j < memheap_map->n_segments; j++) {
            if (ctx->ucp_peers[i].mkeys[j].key.rkey != NULL) {
                ucp_rkey_destroy(ctx->ucp_peers[i].mkeys[j].key.rkey);
            }
        }

        del_procs[i].ep   = ctx->ucp_peers[i].ucp_conn;
        del_procs[i].vpid = i;
        ctx->ucp_peers[i].ucp_conn = NULL;
    }

    opal_common_ucx_del_procs_nofence(del_procs, nprocs, oshmem_my_proc_id(),
                                      mca_spml_ucx.num_disconnect,
                                      ctx->ucp_worker);
    free(del_procs);
    mca_spml_ucx_clear_put_op_mask(ctx);
    free(ctx->ucp_peers);
}

static int mca_spml_ucx_component_fini(void)
{
    int fenced = 0, i;
    int ret = OSHMEM_SUCCESS;

    opal_progress_unregister(spml_ucx_default_progress);
    if (mca_spml_ucx.active_array.ctxs_count) {
        opal_progress_unregister(spml_ucx_ctx_progress);
    }

    if(!mca_spml_ucx.enabled)
        return OSHMEM_SUCCESS; /* never selected.. return success.. */

    if (mca_spml_ucx.async_progress) {
        opal_progress_thread_finalize(NULL);
        opal_event_evtimer_del(mca_spml_ucx.tick_event);
        if (mca_spml_ucx.aux_ctx != NULL) {
            _ctx_cleanup(mca_spml_ucx.aux_ctx);
        }
        opal_progress_unregister(spml_ucx_progress_aux_ctx);
        pthread_spin_destroy(&mca_spml_ucx.async_lock);
    }

    /* delete context objects from list */
    for (i = 0; i < mca_spml_ucx.active_array.ctxs_count; i++) {
        _ctx_cleanup(mca_spml_ucx.active_array.ctxs[i]);
    }

    for (i = 0; i < mca_spml_ucx.idle_array.ctxs_count; i++) {
        _ctx_cleanup(mca_spml_ucx.idle_array.ctxs[i]);
    }


    ret = opal_common_ucx_mca_pmix_fence_nb(&fenced);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    while (!fenced) {
        for (i = 0; i < mca_spml_ucx.active_array.ctxs_count; i++) {
            ucp_worker_progress(mca_spml_ucx.active_array.ctxs[i]->ucp_worker);
        }

        for (i = 0; i < mca_spml_ucx.idle_array.ctxs_count; i++) {
            ucp_worker_progress(mca_spml_ucx.idle_array.ctxs[i]->ucp_worker);
        }

        ucp_worker_progress(mca_spml_ucx_ctx_default.ucp_worker);

        if (mca_spml_ucx.aux_ctx != NULL) {
            ucp_worker_progress(mca_spml_ucx.aux_ctx->ucp_worker);
        }
    }

    /* delete all workers */
    for (i = 0; i < mca_spml_ucx.active_array.ctxs_count; i++) {
        ucp_worker_destroy(mca_spml_ucx.active_array.ctxs[i]->ucp_worker);
        free(mca_spml_ucx.active_array.ctxs[i]);
    }

    for (i = 0; i < mca_spml_ucx.idle_array.ctxs_count; i++) {
        ucp_worker_destroy(mca_spml_ucx.idle_array.ctxs[i]->ucp_worker);
        free(mca_spml_ucx.idle_array.ctxs[i]);
    }

    if (mca_spml_ucx_ctx_default.ucp_worker) {
        ucp_worker_destroy(mca_spml_ucx_ctx_default.ucp_worker);
    }

    if (mca_spml_ucx.aux_ctx != NULL) {
        ucp_worker_destroy(mca_spml_ucx.aux_ctx->ucp_worker);
    }

    mca_spml_ucx.enabled = false;  /* not anymore */

    free(mca_spml_ucx.active_array.ctxs);
    free(mca_spml_ucx.idle_array.ctxs);
    free(mca_spml_ucx.aux_ctx);

    SHMEM_MUTEX_DESTROY(mca_spml_ucx.internal_mutex);
    pthread_mutex_destroy(&mca_spml_ucx.ctx_create_mutex);

    if (mca_spml_ucx.ucp_context) {
        ucp_cleanup(mca_spml_ucx.ucp_context);
        mca_spml_ucx.ucp_context = NULL;
    }

    return OSHMEM_SUCCESS;
}

