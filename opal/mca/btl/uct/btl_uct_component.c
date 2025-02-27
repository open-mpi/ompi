/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2014-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2018-2024 Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019-2025 Google, LLC. All rights reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2022      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "btl_uct_discover.h"
#include "btl_uct_modex.h"
#include "opal/mca/btl/base/base.h"
#include "opal/mca/btl/btl.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/mca/memory/base/base.h"
#include "opal/memoryhooks/memory.h"
#include "opal/util/argv.h"
#include <ucm/api/ucm.h>

#include "opal/util/printf.h"

#include <string.h>

#include "btl_uct_am.h"
#include "btl_uct_device_context.h"

static void mca_btl_uct_cleanup(void)
{
    if (!mca_btl_uct_component.initialized) {
        return;
    }

    BTL_VERBOSE(("in UCT btl cleanup"));

    OBJ_DESTRUCT(&mca_btl_uct_component.memory_domain_list);
    OBJ_DESTRUCT(&mca_btl_uct_component.connection_domain_list);

    OPAL_LIST_DESTRUCT(&mca_btl_uct_component.md_list);

#if UCT_API >= UCT_VERSION(1, 7)
    if (NULL != mca_btl_uct_component.uct_components) {
        uct_release_component_list(mca_btl_uct_component.uct_components);
        mca_btl_uct_component.uct_components = NULL;
        mca_btl_uct_component.num_uct_components = 0;
    }
#endif

    mca_btl_uct_component.initialized = false;
}

static int mca_btl_uct_component_register(void)
{
    mca_btl_uct_component.memory_domains = "mlx5_0,mlx4_0,rocep0s4,irdma0";
    (void) mca_base_component_var_register(
        &mca_btl_uct_component.super.btl_version, "memory_domains",
        "Comma-delimited list of memory domains of the form "
        "to use for communication. Memory domains MUST provide transports that "
        "support put, get, and amos. Special values: all (all available), none."
        " (default: mlx5_0,mlx4_0,rocep0s4,irdma0)",
        MCA_BASE_VAR_TYPE_STRING, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_LOCAL, &mca_btl_uct_component.memory_domains);

    mca_btl_uct_component.allowed_transports = "dc_mlx5,rc_mlx5,rc_verbs,ud,ud_verbs,ugni_rdma,ugni_smsg,any";
    (void) mca_base_component_var_register(
        &mca_btl_uct_component.super.btl_version, "transports",
        "Comma-delimited list of transports to use sorted by increasing "
        "priority. The list of transports available can be queried using ucx_info. Special"
        "values: any (any available) (default: dc_mlx5,rc_mlx5,ud,any)",
        MCA_BASE_VAR_TYPE_STRING, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_LOCAL, &mca_btl_uct_component.allowed_transports);

    mca_btl_uct_component.connection_domains = "tcp";
    (void) mca_base_component_var_register(
        &mca_btl_uct_component.super.btl_version, "connection_domains",
        "Comma-delimited list of connection-only domains to use sorted by increasing "
        "priority. The list of transports available can be queried using ucx_info. Special"
        "values: any (any available) (default: tcp)",
        MCA_BASE_VAR_TYPE_STRING, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_LOCAL, &mca_btl_uct_component.connection_domains);

    mca_btl_uct_component.num_contexts_per_module = 0;
    (void) mca_base_component_var_register(
        &mca_btl_uct_component.super.btl_version, "num_contexts_per_module",
        "Number of UCT worker contexts "
        "to create for each BTL module. Larger numbers will improve "
        "multi-threaded performance but may increase memory usage. "
        "A good rule of thumb is one context per application thread "
        "that will be calling into MPI. (default: 0 -- autoselect "
        "based on the number of cores)",
        MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_ALL, &mca_btl_uct_component.num_contexts_per_module);

    mca_btl_uct_component.disable_ucx_memory_hooks = true;
    (void)
        mca_base_component_var_register(&mca_btl_uct_component.super.btl_version,
                                        "disable_ucx_memory_hooks",
                                        "Disable the munmap memory hook "
                                        "inside UCX. These hooks are not necessary when using the "
                                        "uct btl and tend to cause performance problems when using "
                                        "multiple threads (default: true)",
                                        MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                        OPAL_INFO_LVL_3, MCA_BASE_VAR_SCOPE_ALL,
                                        &mca_btl_uct_component.disable_ucx_memory_hooks);

#if OPAL_C_HAVE__THREAD_LOCAL
    mca_btl_uct_component.bind_threads_to_contexts = true;
    (void) mca_base_component_var_register(
        &mca_btl_uct_component.super.btl_version, "bind_threads_to_contexts",
        "Bind threads to device contexts. "
        "In general this should improve the multi-threaded performance "
        "when threads are used. (default: true)",
        MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_ALL, &mca_btl_uct_component.bind_threads_to_contexts);
#endif

    /* timeout between connection message attempts in µs */
    mca_btl_uct_component.connection_retry_timeout = 2000;
    (void) mca_base_component_var_register(
        &mca_btl_uct_component.super.btl_version, "connection_retry_timeout",
        "Timeout between attempts to send connection messages for connect-to-"
        "endpoint connections. The timeout is measured in µs and is only"
        "necessary when using unreliable transports for connections (ex: UD). "
        "(default: 2000µs)",
        MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_4,
        MCA_BASE_VAR_SCOPE_LOCAL, &mca_btl_uct_component.connection_retry_timeout);

    OBJ_CONSTRUCT(&mca_btl_uct_component.md_list, opal_list_t);
    OBJ_CONSTRUCT(&mca_btl_uct_component.memory_domain_list, mca_btl_uct_include_list_t);
    OBJ_CONSTRUCT(&mca_btl_uct_component.connection_domain_list, mca_btl_uct_include_list_t);

    int rc = mca_btl_uct_component_discover_mds();
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    rc = mca_btl_uct_component_generate_modules(&mca_btl_uct_component.md_list);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    mca_btl_uct_component.initialized = true;
    opal_finalize_register_cleanup(mca_btl_uct_cleanup);

    return OPAL_SUCCESS;
}

static void mca_btl_uct_mem_release_cb(void *buf, size_t length, void *cbdata, bool from_alloc)
{
    ucm_vm_munmap(buf, length);
}

static int mca_btl_uct_component_open(void)
{
    if (0 == mca_btl_uct_component.num_contexts_per_module) {
        /* use the core count and the number of local processes to determine
         * how many UCT workers to create */
        int core_count = 36;

        (void) opal_hwloc_base_get_topology();
        if (0 > (core_count = hwloc_get_nbobjs_by_type(opal_hwloc_topology, HWLOC_OBJ_CORE))) {
            return OPAL_ERROR;
        }

        if ((uint32_t)core_count <= opal_process_info.num_local_peers || !opal_using_threads()) {
            /* there is probably no benefit to using multiple device contexts when not
             * using threads or oversubscribing the node with mpi processes. */
            mca_btl_uct_component.num_contexts_per_module = 1;
        } else {
            mca_btl_uct_component.num_contexts_per_module = core_count
                                                            / (opal_process_info.num_local_peers
                                                               + 1);
        }
    }

    if (mca_btl_uct_component.num_contexts_per_module > MCA_BTL_UCT_MAX_WORKERS) {
        mca_btl_uct_component.num_contexts_per_module = MCA_BTL_UCT_MAX_WORKERS;
    }

    if (mca_btl_uct_component.disable_ucx_memory_hooks
        && ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT)
            == ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT)
                & opal_mem_hooks_support_level()))) {
        ucm_set_external_event(UCM_EVENT_VM_UNMAPPED);
        opal_mem_hooks_register_release(mca_btl_uct_mem_release_cb, NULL);
    }

    return OPAL_SUCCESS;
}

/*
 * component cleanup - sanity checking of queue lengths
 */
static int mca_btl_uct_component_close(void)
{
    mca_btl_uct_component.conn_tl = NULL;

    if (mca_btl_uct_component.disable_ucx_memory_hooks) {
        opal_mem_hooks_unregister_release(mca_btl_uct_mem_release_cb);
    }

    /* complete delayed cleanup */
    mca_btl_uct_cleanup();

    return OPAL_SUCCESS;
}

ucs_status_t mca_btl_uct_am_handler(void *arg, void *data, size_t length, unsigned flags)
{
    mca_btl_uct_device_context_t *tl_context = (mca_btl_uct_device_context_t *) arg;
    mca_btl_uct_module_t *uct_btl = tl_context->uct_btl;
    mca_btl_uct_am_header_t *header = (mca_btl_uct_am_header_t *) data;
    mca_btl_active_message_callback_t *reg = mca_btl_base_active_message_trigger + header->data.tag;
    mca_btl_base_segment_t seg = {.seg_addr = {.pval = (void *) ((intptr_t) data
                                                                 + sizeof(*header))},
                                  .seg_len = length - sizeof(*header)};
    mca_btl_base_receive_descriptor_t desc = {.endpoint = NULL,
                                              .des_segments = &seg,
                                              .des_segment_count = 1,
                                              .tag = header->data.tag,
                                              .cbdata = reg->cbdata};

    /* prevent recursion */
    tl_context->in_am_callback = true;
    reg->cbfunc(&uct_btl->super, &desc);
    tl_context->in_am_callback = false;

    return UCS_OK;
}

/*
 *  UCT component initialization:
 *  (1) read interface list from kernel and compare against component parameters
 *      then create a BTL instance for selected interfaces
 *  (2) setup UCT listen socket for incoming connection attempts
 *  (3) register BTL parameters with the MCA
 */

static mca_btl_base_module_t **mca_btl_uct_component_init(int *num_btl_modules,
                                                          bool enable_progress_threads,
                                                          bool enable_mpi_threads)
{
    /* for this BTL to be useful the interface needs to support RDMA and certain atomic operations
     */
    struct mca_btl_base_module_t **base_modules;
    int rc;

    BTL_VERBOSE(("initializing uct btl"));

    if (NULL == mca_btl_uct_component.memory_domains
        || 0 == strlen(mca_btl_uct_component.memory_domains)
        || 0 == strcmp(mca_btl_uct_component.memory_domains, "none")) {
        BTL_VERBOSE(("no uct memory domains specified"));
        return NULL;
    }

    rc = mca_btl_uct_enable_modules(mca_btl_uct_component.modules, mca_btl_uct_component.module_count);
    if (OPAL_SUCCESS != rc) {
        return NULL;
    }

    rc = mca_btl_uct_component_maybe_setup_conn_tl();
    if (OPAL_SUCCESS != rc && OPAL_ERR_NOT_FOUND != rc) {
        return NULL;
    }

    rc = mca_btl_uct_component_filter_mds();
    if (OPAL_SUCCESS != rc) {
        return NULL;
    }

    rc = mca_btl_uct_component_modex_send();
    if (OPAL_SUCCESS != rc) {
        return NULL;
    }

    /* pass module array back to caller */
    base_modules = calloc(mca_btl_uct_component.module_count, sizeof(*base_modules));
    if (NULL == base_modules) {
        return NULL;
    }

    memcpy(base_modules, mca_btl_uct_component.modules,
           mca_btl_uct_component.module_count * sizeof(mca_btl_uct_component.modules[0]));

    *num_btl_modules = mca_btl_uct_component.module_count;

    BTL_VERBOSE(("uct btl initialization complete. found %d suitable memory domains",
                 mca_btl_uct_component.module_count));

    return base_modules;
}

static int mca_btl_uct_tl_progress(mca_btl_uct_tl_t *tl, int starting_index)
{
    unsigned int ret = 0;

    if (NULL == tl) {
        return 0;
    }

    for (int j = 0; j < tl->max_device_contexts; ++j) {
        if (tl->uct_dev_contexts[j]) {
            ret += mca_btl_uct_context_progress(tl->uct_dev_contexts[j]);
        }
    }

    return ret;
}

static int mca_btl_uct_component_progress_pending(mca_btl_uct_module_t *uct_btl)
{
    mca_btl_uct_base_frag_t *frag, *next;
    int completed = 0;
    size_t count;

    if (0 == (count = opal_list_get_size(&uct_btl->pending_frags))) {
        return 0;
    }

    OPAL_THREAD_LOCK(&uct_btl->lock);
    OPAL_LIST_FOREACH_SAFE (frag, next, &uct_btl->pending_frags, mca_btl_uct_base_frag_t) {
        if (!frag->ready) {
            continue;
        }

        opal_list_remove_item(&uct_btl->pending_frags, (opal_list_item_t *) frag);

        if (OPAL_SUCCESS > mca_btl_uct_send_frag(uct_btl, frag, /*append=*/false)) {
            opal_list_prepend(&uct_btl->pending_frags, (opal_list_item_t *) frag);
        } else {
            completed++;
        }
    }
    OPAL_THREAD_UNLOCK(&uct_btl->lock);

    return completed;
}

static int mca_btl_uct_component_progress_connections (mca_btl_uct_tl_t *conn_tl) {
    mca_btl_uct_pending_connection_request_t *request;
    int ret;

    if (conn_tl == NULL) {
        return 0;
    }

    ret = mca_btl_uct_tl_progress(conn_tl, 0);

    while (NULL
           != (request = (mca_btl_uct_pending_connection_request_t *) opal_fifo_pop_atomic(
                                                                                           &conn_tl->pending_connection_reqs))) {
        mca_btl_uct_conn_req_t *conn_req = (mca_btl_uct_conn_req_t *) request->request_data;
        BTL_VERBOSE(("processing connection request...."));
        if (conn_req->module_index >= mca_btl_uct_component.module_count) {
            BTL_ERROR(("invalid connection request received"));
            abort();
        }
        int rc = mca_btl_uct_process_connection_request(mca_btl_uct_component.modules[conn_req->module_index], conn_req);
        if (rc != OPAL_SUCCESS) {
            opal_fifo_push_atomic(&conn_tl->pending_connection_reqs, &request->super);
            break;
        }
        OBJ_RELEASE(request);
    }

    return ret;
}

/**
 * @brief UCT BTL progress function
 *
 * This function explicitly progresses all workers.
 */
static int mca_btl_uct_component_progress(void)
{
    int starting_index = mca_btl_uct_get_context_index();
    unsigned ret = 0;

    mca_btl_uct_md_t *md;
    OPAL_LIST_FOREACH(md, &mca_btl_uct_component.md_list, mca_btl_uct_md_t) {
        /* unlike ucp, uct actually tells us something useful! its almost like it was "inspired"
         * by the btl progress functions.... */
        mca_btl_uct_tl_t *tl;
        OPAL_LIST_FOREACH(tl, &md->tls, mca_btl_uct_tl_t) {
            ret += mca_btl_uct_tl_progress(tl, starting_index);
        }
    }
 
    for (int i = 0; i < mca_btl_uct_component.module_count; ++i) {
        mca_btl_uct_module_t *module = mca_btl_uct_component.modules[i];

        if (0 != opal_list_get_size(&module->pending_frags)) {
            mca_btl_uct_component_progress_pending(module);
        }
    }

    if (NULL != mca_btl_uct_component.conn_tl) {
        ret += mca_btl_uct_component_progress_connections (mca_btl_uct_component.conn_tl);
    }

    return (int) ret;
}

/** UCT btl component */
mca_btl_uct_component_t mca_btl_uct_component = {
    .super = {
        .btl_version =
            {
                MCA_BTL_DEFAULT_VERSION("uct"),
                .mca_open_component = mca_btl_uct_component_open,
                .mca_close_component = mca_btl_uct_component_close,
                .mca_register_component_params = mca_btl_uct_component_register,
            },
        .btl_data =
            {/* The component is not checkpoint ready */
             .param_field = MCA_BASE_METADATA_PARAM_NONE},

        .btl_init = mca_btl_uct_component_init,
        .btl_progress = mca_btl_uct_component_progress,
    }};
MCA_BASE_COMPONENT_INIT(opal, btl, uct)

static void safety_valve(void) __opal_attribute_destructor__;
void safety_valve(void) {
    opal_mem_hooks_unregister_release(mca_btl_uct_mem_release_cb);
}
