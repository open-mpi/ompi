/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019-2025 Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_uct_am.h"
#include "btl_uct_device_context.h"
#include "opal/util/argv.h"
#include "opal/util/bit_ops.h"
#include "opal/util/minmax.h"

/**
 * @brief Convert UCT capabilities to BTL flags
 */
static uint64_t mca_btl_uct_cap_to_btl_flag[][2] = {
    {UCT_IFACE_FLAG_AM_SHORT, MCA_BTL_FLAGS_SEND},
    {UCT_IFACE_FLAG_PUT_ZCOPY, MCA_BTL_FLAGS_PUT},
    {UCT_IFACE_FLAG_GET_ZCOPY, MCA_BTL_FLAGS_GET},
    {0, 0},
};

/**
 * @brief Convert UCT capability flags to BTL flags
 *
 * @param[in] cap_flags  UCT capability flags
 *
 * @returns equivalent BTL flags
 */
static int32_t mca_btl_uct_module_flags(uint64_t cap_flags)
{
    uint32_t flags = 0;

    for (int i = 0; mca_btl_uct_cap_to_btl_flag[i][0] > 0; ++i) {
        if (cap_flags & mca_btl_uct_cap_to_btl_flag[i][0]) {
            flags |= (uint32_t) mca_btl_uct_cap_to_btl_flag[i][1];
        }
    }
    return flags;
}

#if OPAL_HAVE_UCT_EP_ATOMIC64_POST
/**
 * @brief Convert UCT capabilities to BTL atomic flags
 */
static uint64_t mca_btl_uct_cap_to_btl_atomic_flag[][2] = {
    {UCS_BIT(UCT_ATOMIC_OP_ADD), MCA_BTL_ATOMIC_SUPPORTS_ADD},
    {UCS_BIT(UCT_ATOMIC_OP_AND), MCA_BTL_ATOMIC_SUPPORTS_AND},
    {UCS_BIT(UCT_ATOMIC_OP_OR), MCA_BTL_ATOMIC_SUPPORTS_OR},
    {UCS_BIT(UCT_ATOMIC_OP_XOR), MCA_BTL_ATOMIC_SUPPORTS_XOR},
    {UCS_BIT(UCT_ATOMIC_OP_SWAP), MCA_BTL_ATOMIC_SUPPORTS_SWAP},
    {UCS_BIT(UCT_ATOMIC_OP_CSWAP), MCA_BTL_ATOMIC_SUPPORTS_CSWAP},
    {
        0,
    },
};

static void mca_btl_uct_module_set_atomic_flags(mca_btl_uct_module_t *module)
{
    mca_btl_uct_tl_t *tl = module->rdma_tl;
    uint64_t cap_flags = tl->uct_iface_attr.cap.flags;

    /* NTH: only use the fetching atomics for now */
    uint64_t atomic_flags32 = tl->uct_iface_attr.cap.atomic32.fop_flags;
    uint64_t atomic_flags64 = tl->uct_iface_attr.cap.atomic64.fop_flags;

    uint64_t all_flags = atomic_flags64 | atomic_flags32;

    module->super.btl_atomic_flags = (0 != atomic_flags32) ? MCA_BTL_ATOMIC_SUPPORTS_32BIT : 0;
    
    if (cap_flags & UCT_IFACE_FLAG_ATOMIC_CPU) {
        module->super.btl_atomic_flags |= MCA_BTL_ATOMIC_SUPPORTS_GLOB;
    }

    for (int i = 0; mca_btl_uct_cap_to_btl_atomic_flag[i][0]; ++i) {
        if (all_flags & mca_btl_uct_cap_to_btl_atomic_flag[i][0]) {
            module->super.btl_atomic_flags |= mca_btl_uct_cap_to_btl_atomic_flag[i][1];
        }
    }

    if (0 != module->super.btl_atomic_flags) {
        /* some atomics are supported */
        module->super.btl_flags |= MCA_BTL_FLAGS_ATOMIC_FOPS | MCA_BTL_FLAGS_ATOMIC_OPS;
    }
}

#else
/**
 * @brief Convert UCT capabilities to BTL atomic flags
 */
static uint64_t mca_btl_uct_cap_to_btl_atomic_flag[][2] = {
    {UCT_IFACE_FLAG_ATOMIC_ADD64, MCA_BTL_ATOMIC_SUPPORTS_ADD},
    {UCT_IFACE_FLAG_ATOMIC_ADD32, MCA_BTL_ATOMIC_SUPPORTS_32BIT},
    {UCT_IFACE_FLAG_ATOMIC_CSWAP64, MCA_BTL_ATOMIC_SUPPORTS_CSWAP},
    {UCT_IFACE_FLAG_ATOMIC_SWAP64, MCA_BTL_ATOMIC_SUPPORTS_SWAP},
    {UCT_IFACE_FLAG_ATOMIC_CPU, MCA_BTL_ATOMIC_SUPPORTS_GLOB},
    {
        0,
    },
};

/**
 * @brief Convert UCT capability flags to BTL atomic flags
 *
 * @param[in] cap_flags  UCT capability flags
 *
 * @returns equivalent BTL atomic flags
 */
static void mca_btl_uct_module_set_atomic_flags(mca_btl_uct_module_t *module)
{
    mca_btl_uct_tl_t *tl = module->rdma_tl;
    uint64_t cap_flags = tl->uct_iface_attr.cap.flags;

    module->super.btl_atomic_flags = 0;

    for (int i = 0; mca_btl_uct_cap_to_btl_atomic_flag[i][0] > 0; ++i) {
        if (cap_flags & mca_btl_uct_cap_to_btl_atomic_flag[i][0]) {
            module->super.btl_atomic_flags |= (uint32_t) mca_btl_uct_cap_to_btl_atomic_flag[i][1];
        }
    }

    if (0 != module->super.btl_atomic_flags) {
        /* some atomics are supported */
        module->super.btl_flags |= MCA_BTL_FLAGS_ATOMIC_FOPS | MCA_BTL_FLAGS_ATOMIC_OPS;
    }
}

#endif

static void mca_btl_uct_tl_constructor(mca_btl_uct_tl_t *tl)
{
    memset((void *) ((uintptr_t) tl + sizeof(tl->super)), 0, sizeof(*tl) - sizeof(tl->super));
    OBJ_CONSTRUCT(&tl->tl_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&tl->pending_connection_reqs, opal_fifo_t);
}

static void mca_btl_uct_tl_destructor(mca_btl_uct_tl_t *tl)
{
    assert(((opal_object_t *) tl)->obj_reference_count == 0);

    for (int context_id = 0; context_id < MCA_BTL_UCT_MAX_WORKERS; ++context_id) {
        if (NULL != tl->uct_dev_contexts[context_id]) {
            mca_btl_uct_context_destroy(tl->uct_dev_contexts[context_id]);
        }
    }

    if (tl->ucs_async) {
        ucs_async_context_destroy(tl->ucs_async);
    }

    free(tl->uct_tl_name);
    free(tl->uct_dev_name);

    if (NULL != tl->uct_tl_config) {
        uct_config_release(tl->uct_tl_config);
    }

    OBJ_DESTRUCT(&tl->tl_lock);
    OBJ_DESTRUCT(&tl->pending_connection_reqs);
}

OBJ_CLASS_INSTANCE(mca_btl_uct_tl_t, opal_list_item_t, mca_btl_uct_tl_constructor,
                   mca_btl_uct_tl_destructor);

static ucs_status_t mca_btl_uct_conn_req_cb(void *arg, void *data, size_t length, unsigned flags)
{
    mca_btl_uct_tl_t *tl = (mca_btl_uct_tl_t *) arg;
    mca_btl_uct_pending_connection_request_t *request = calloc(1, length + sizeof(request->super));

    /* it is not safe to process the connection request from the callback so just save it for
     * later processing */
    OBJ_CONSTRUCT(request, mca_btl_uct_pending_connection_request_t);
    memcpy(&request->request_data, (void *) ((intptr_t) data + 8), length);
    opal_fifo_push_atomic(&tl->pending_connection_reqs, &request->super);

    return UCS_OK;
}

OBJ_CLASS_INSTANCE(mca_btl_uct_pending_connection_request_t, opal_list_item_t, NULL, NULL);

int mca_btl_uct_process_connection_request(mca_btl_uct_module_t *module,
                                           mca_btl_uct_conn_req_t *req)
{
    struct opal_proc_t *remote_proc = opal_proc_for_name(req->proc_name);
    mca_btl_base_endpoint_t *endpoint = mca_btl_uct_get_ep(&module->super, remote_proc);
    mca_btl_uct_tl_endpoint_t *tl_endpoint = endpoint->uct_eps[req->context_id] + req->tl_index;
    int32_t ep_flags;
    int rc;

    if (NULL == endpoint) {
        BTL_ERROR(("could not create endpoint for connection request"));
        return UCS_ERR_UNREACHABLE;
    }

    assert(req->type < 2);

    ep_flags = opal_atomic_fetch_or_32(&tl_endpoint->flags, MCA_BTL_UCT_ENDPOINT_FLAG_CONN_REC);

    BTL_VERBOSE(("got connection request for endpoint %p. type = %d. context id = %d. ep_flags = %x",
                 (void *) endpoint, req->type, req->context_id, ep_flags));

    if (!(ep_flags & MCA_BTL_UCT_ENDPOINT_FLAG_CONN_REC)) {
        /* create any necessary resources */
        rc = mca_btl_uct_endpoint_connect(module, endpoint, req->context_id, req->ep_addr,
                                          req->tl_index);
        if (OPAL_SUCCESS != rc && OPAL_ERR_OUT_OF_RESOURCE != rc) {
            BTL_ERROR(("could not setup rdma endpoint. rc = %d", rc));
            return rc;
        }
    }

    /* the connection is ready once we have received the connection data and also a connection ready
     * message. this might be overkill but there is little documentation at the UCT level on when
     * an endpoint can be used. */
    if (req->type == 1) {
        /* remote side is connected */
        /* to avoid a race with send adding pending frags grab the lock here */
        OPAL_THREAD_SCOPED_LOCK(&endpoint->ep_lock, {
            BTL_VERBOSE(("connection ready. sending %" PRIsize_t " frags",
                         opal_list_get_size(&module->pending_frags)));
            mca_btl_uct_endpoint_set_flag(module, endpoint, req->context_id, tl_endpoint,
                                          MCA_BTL_UCT_ENDPOINT_FLAG_CONN_REM_READY);
        });
    }

    return OPAL_SUCCESS;
}

static int mca_btl_uct_setup_connection_tl(mca_btl_uct_tl_t *tl)
{
    ucs_status_t ucs_status;

    if (NULL == tl) {
        return OPAL_ERR_NOT_SUPPORTED;
    }

    mca_btl_uct_device_context_t *context =
        mca_btl_uct_module_get_tl_context_specific(/*module=*/NULL, tl,
                                                   /*context_id=*/0);

    ucs_status = uct_iface_set_am_handler(context->uct_iface,
                                          MCA_BTL_UCT_CONNECT_RDMA, mca_btl_uct_conn_req_cb,
                                          tl, UCT_CB_FLAG_ASYNC);
    if (UCS_OK != ucs_status) {
        BTL_ERROR(("could not set active message handler for uct tl"));
    }

    return UCS_OK == ucs_status ? OPAL_SUCCESS : OPAL_ERROR;
}

static int mca_btl_uct_populate_tl_attr(mca_btl_uct_tl_t *tl) {
#if UCT_API >= UCT_VERSION(1, 6)
    uct_iface_params_t iface_params = {.field_mask = UCT_IFACE_PARAM_FIELD_OPEN_MODE
                                                     | UCT_IFACE_PARAM_FIELD_DEVICE,
                                       .open_mode = UCT_IFACE_OPEN_MODE_DEVICE,
                                       .mode = {.device = {.tl_name = tl->uct_tl_name,
                                                           .dev_name = tl->uct_dev_name}}};
#else
    uct_iface_params_t iface_params = {.rndv_cb = NULL,
                                       .eager_cb = NULL,
                                       .stats_root = NULL,
                                       .rx_headroom = 0,
                                       .open_mode = UCT_IFACE_OPEN_MODE_DEVICE,
                                       .mode = {.device = {.tl_name = tl->uct_tl_name,
                                                           .dev_name = tl->uct_dev_name}}};
#endif
    ucs_status_t ucs_status;

    /* do the bare minimum to get tl attributes */
    uct_worker_h uct_worker;
    ucs_status = uct_worker_create(tl->ucs_async, UCS_THREAD_MODE_SINGLE, &uct_worker);
    if (OPAL_UNLIKELY(UCS_OK != ucs_status)) {
        BTL_VERBOSE(("could not create a UCT worker"));
        return OPAL_ERROR;
    }

    uct_iface_h uct_iface;
    ucs_status = uct_iface_open(tl->uct_md->uct_md, uct_worker, &iface_params,
                                tl->uct_tl_config, &uct_iface);
    if (OPAL_UNLIKELY(UCS_OK != ucs_status)) {
        BTL_VERBOSE(("could not open UCT interface. error code: %d", ucs_status));
        uct_worker_destroy(uct_worker);
        return OPAL_ERROR;
    }

    int rc = OPAL_SUCCESS;
    ucs_status = uct_iface_query(uct_iface, &tl->uct_iface_attr);
    if (UCS_OK != ucs_status) {
        BTL_VERBOSE(("Error querying UCT interface"));
        rc = OPAL_ERROR;
    }

    uct_iface_close(uct_iface);
    uct_worker_destroy(uct_worker);
    return rc;
}

static mca_btl_uct_tl_t *mca_btl_uct_create_tl(mca_btl_uct_md_t *md,
                                               uct_tl_resource_desc_t *tl_desc, int priority)
{
    mca_btl_uct_tl_t *tl = OBJ_NEW(mca_btl_uct_tl_t);

    if (OPAL_UNLIKELY(NULL == tl)) {
        return NULL;
    }

    /* initialize btl tl structure */
    tl->uct_md = md;

    tl->uct_tl_name = strdup(tl_desc->tl_name);
    tl->uct_dev_name = strdup(tl_desc->dev_name);
    tl->dev_type = tl_desc->dev_type;
    tl->priority = priority;

    (void) uct_md_iface_config_read(md->uct_md, tl_desc->tl_name, NULL, NULL, &tl->uct_tl_config);

    ucs_status_t ucs_status = ucs_async_context_create(UCS_ASYNC_MODE_THREAD, &tl->ucs_async);
    if (UCS_OK != ucs_status) {
        BTL_VERBOSE(("Could not create a UCT async context"));
        OBJ_RELEASE(tl);
        return NULL;
    }

    int rc = mca_btl_uct_populate_tl_attr(tl);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        OBJ_RELEASE(tl);
        return NULL;
    }

    BTL_VERBOSE(("Interface CAPS for tl %s::%s::%s 0x%lx", md->md_name, tl_desc->tl_name,
                 tl_desc->dev_name, (unsigned long) tl->uct_iface_attr.cap.flags));

    return tl;
}

static void mca_btl_uct_set_tl_rdma(mca_btl_uct_module_t *module, mca_btl_uct_tl_t *tl)
{
    BTL_VERBOSE(("tl %s is suitable for RDMA", tl->uct_tl_name));

    module->rdma_tl = tl;

    mca_btl_uct_module_set_atomic_flags(module);

    module->super.btl_get_limit = opal_min(tl->uct_iface_attr.cap.get.max_zcopy,
                                           module->super.btl_get_limit);
    if (tl->uct_iface_attr.cap.get.max_bcopy) {
        module->super.btl_get_alignment = 0;
        module->super.btl_get_local_registration_threshold = tl->uct_iface_attr
                                                                 .cap.get.max_bcopy;
    } else {
        /* this is overkill in terms of alignment but we have no way to enforce a minimum get size
         */
        module->super.btl_get_alignment = opal_next_poweroftwo_inclusive(
            tl->uct_iface_attr.cap.get.min_zcopy);
    }

    module->super.btl_put_limit = opal_min(tl->uct_iface_attr.cap.put.max_zcopy,
                                           module->super.btl_put_limit);
    module->super.btl_put_alignment = 0;

    /* no registration needed when using short/bcopy put */
    module->super.btl_put_local_registration_threshold = tl->uct_iface_attr
                                                             .cap.put.max_bcopy;

    tl->tl_index = (module->am_tl && tl != module->am_tl) ? 1 : 0;
    if (tl->max_device_contexts <= 1) {
        tl->max_device_contexts = mca_btl_uct_component.num_contexts_per_module;
    }
}

static void mca_btl_uct_set_tl_am(mca_btl_uct_module_t *module, mca_btl_uct_tl_t *tl)
{
    BTL_VERBOSE(("tl %s is suitable for active-messaging", tl->uct_tl_name));
    module->am_tl = tl;

    tl->tl_index = (module->rdma_tl && tl != module->rdma_tl) ? 1 : 0;
    if (tl->max_device_contexts <= 1) {
        tl->max_device_contexts = mca_btl_uct_component.num_contexts_per_module;
    }

    size_t max_eager_limit = tl->uct_iface_attr.cap.am.max_bcopy
        - sizeof(mca_btl_uct_am_header_t);
    size_t max_send_size = max_eager_limit;

    if (tl->uct_iface_attr.cap.flags & UCT_IFACE_FLAG_AM_ZCOPY) {
        max_send_size = opal_max(max_send_size, tl->uct_iface_attr.cap.am.max_zcopy
                                 - sizeof(mca_btl_uct_am_header_t));
    }

    module->super.btl_eager_limit = opal_min(module->super.btl_eager_limit, max_eager_limit);
    module->super.btl_max_send_size = opal_min(module->super.btl_max_send_size, max_send_size);
}

int mca_btl_uct_enable_tl_conn(mca_btl_uct_tl_t *tl)
{
    int rc;

    BTL_VERBOSE(("tl %s is suitable for making connections", tl->uct_tl_name));

    rc = mca_btl_uct_setup_connection_tl(tl);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    if (!tl->max_device_contexts) {
        /* if a tl is only being used to create connections do not bother with multiple
         * contexts */
        tl->max_device_contexts = 1;
    }

    return OPAL_SUCCESS;
}

int mca_btl_uct_evaluate_tl(mca_btl_uct_module_t *module, mca_btl_uct_tl_t *tl)
{
    BTL_VERBOSE(("evaluating tl %s::%s", tl->uct_md->md_name, tl->uct_tl_name));
    if (NULL == module->rdma_tl && mca_btl_uct_tl_supports_rdma(tl)) {
        mca_btl_uct_set_tl_rdma(module, tl);
    }

    if (NULL == module->am_tl && mca_btl_uct_tl_support_am(tl)) {
        mca_btl_uct_set_tl_am(module, tl);
    }

    if (tl == module->rdma_tl || tl == module->am_tl) {
        BTL_VERBOSE(("tl has flags 0x%" PRIx64, tl->uct_iface_attr.cap.flags));
        module->super.btl_flags |= mca_btl_uct_module_flags(tl->uct_iface_attr.cap.flags);
        module->super.btl_flags |= MCA_BTL_FLAGS_RDMA_REMOTE_COMPLETION;

        /* the bandwidth and latency numbers relate to both rdma and active messages. need to
         * come up with a better estimate. */

        /* UCT bandwidth is in bytes/sec, BTL is in MB/sec */
#if UCT_API >= UCT_VERSION(1, 7)
        module->super.btl_bandwidth = (uint32_t)((tl->uct_iface_attr.bandwidth.dedicated
                                                  + tl->uct_iface_attr.bandwidth.shared
                                                        / (opal_process_info.num_local_peers + 1))
                                                 / 1048576.0);
#else
        module->super.btl_bandwidth = (uint32_t)(tl->uct_iface_attr.bandwidth / 1048576.0);
#endif
        /* TODO -- figure out how to translate UCT latency to us */
        module->super.btl_latency = 1;
    }

    return OPAL_SUCCESS;
}

int mca_btl_uct_populate_tls(mca_btl_uct_md_t *md, uct_tl_resource_desc_t *tl_descs, unsigned tl_count)
{
    BTL_VERBOSE(("processing %u tls in memory domain %s", tl_count, md->md_name));

    for (unsigned i = 0; i < tl_count; ++i) {
        BTL_VERBOSE(("processing tl %s::%s::%s", md->md_name, tl_descs[i].tl_name, tl_descs[i].dev_name));

        /* the priority will be set during module creation */
        mca_btl_uct_tl_t *tl = mca_btl_uct_create_tl(md, tl_descs + i, /*priority=*/0);
        if (tl) {
            opal_list_append(&md->tls, &tl->super);
        }
    }

    if (0 == opal_list_get_size(&md->tls)) {
        BTL_VERBOSE(("no suitable tls match filter: %s", mca_btl_uct_component.allowed_transports));
        return OPAL_ERR_NOT_AVAILABLE;
    }

    return OPAL_SUCCESS;
}
