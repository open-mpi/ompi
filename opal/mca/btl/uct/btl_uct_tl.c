/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_uct_device_context.h"
#include "btl_uct_am.h"
#include "opal/util/bit_ops.h"
#include "opal/util/argv.h"

/**
 * @brief Convert UCT capabilities to BTL flags
 */
static uint64_t mca_btl_uct_cap_to_btl_flag[][2] = {
    {UCT_IFACE_FLAG_AM_ZCOPY, MCA_BTL_FLAGS_SEND},
    {UCT_IFACE_FLAG_PUT_ZCOPY, MCA_BTL_FLAGS_PUT},
    {UCT_IFACE_FLAG_GET_ZCOPY, MCA_BTL_FLAGS_GET},
    {0,0},
};

/**
 * @brief Convert UCT capability flags to BTL flags
 *
 * @param[in] cap_flags  UCT capability flags
 *
 * @returns equivalent BTL flags
 */
static int32_t mca_btl_uct_module_flags (uint64_t cap_flags)
{
    uint32_t flags = 0;

    for (int i = 0 ; mca_btl_uct_cap_to_btl_flag[i][0] > 0 ; ++i) {
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
    {0, },
};

static void mca_btl_uct_module_set_atomic_flags (mca_btl_uct_module_t *module, mca_btl_uct_tl_t *tl)
{
    uint64_t cap_flags = tl->uct_iface_attr.cap.flags;

    /* NTH: only use the fetching atomics for now */
    uint64_t atomic_flags32 = tl->uct_iface_attr.cap.atomic32.fop_flags;
    uint64_t atomic_flags64 = tl->uct_iface_attr.cap.atomic64.fop_flags;

    /* NTH: don't really have a way to seperate 32-bit and 64-bit right now */
    uint64_t all_flags = atomic_flags32 & atomic_flags64;

    module->super.btl_atomic_flags = 0;

    if (cap_flags & UCT_IFACE_FLAG_ATOMIC_CPU) {
        module->super.btl_atomic_flags |= MCA_BTL_ATOMIC_SUPPORTS_GLOB;
    }

    for (int i = 0 ; mca_btl_uct_cap_to_btl_atomic_flag[i][0] ; ++i) {
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
    {0, },
};

/**
 * @brief Convert UCT capability flags to BTL atomic flags
 *
 * @param[in] cap_flags  UCT capability flags
 *
 * @returns equivalent BTL atomic flags
 */
static void mca_btl_uct_module_set_atomic_flags (mca_btl_uct_module_t *module, mca_btl_uct_tl_t *tl)
{
    uint64_t cap_flags = tl->uct_iface_attr.cap.flags;

    module->super.btl_atomic_flags = 0;

    for (int i = 0 ; mca_btl_uct_cap_to_btl_atomic_flag[i][0] > 0 ; ++i) {
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

static void mca_btl_uct_tl_constructor (mca_btl_uct_tl_t *tl)
{
    memset ((void *)((uintptr_t) tl + sizeof (tl->super)), 0, sizeof (*tl) - sizeof (tl->super));
    OBJ_CONSTRUCT(&tl->tl_lock, opal_mutex_t);
}

static void mca_btl_uct_tl_destructor (mca_btl_uct_tl_t *tl)
{
    assert (((opal_object_t *) tl)->obj_reference_count == 0);

    for (int context_id = 0 ; context_id < MCA_BTL_UCT_MAX_WORKERS ; ++context_id) {
        if (NULL != tl->uct_dev_contexts[context_id]) {
            mca_btl_uct_context_destroy (tl->uct_dev_contexts[context_id]);
        }
    }

    if (tl->uct_md) {
        OBJ_RELEASE(tl->uct_md);
    }

    free (tl->uct_dev_contexts);
    free (tl->uct_tl_name);
    free (tl->uct_dev_name);

    if (NULL != tl->uct_tl_config) {
        uct_config_release (tl->uct_tl_config);
    }

    OBJ_DESTRUCT(&tl->tl_lock);
}

OBJ_CLASS_INSTANCE(mca_btl_uct_tl_t, opal_list_item_t, mca_btl_uct_tl_constructor, mca_btl_uct_tl_destructor);

static ucs_status_t mca_btl_uct_conn_req_cb (void *arg, void *data, size_t length, unsigned flags)
{
    mca_btl_uct_module_t *module = (mca_btl_uct_module_t *) arg;
    mca_btl_uct_conn_req_t *req = (mca_btl_uct_conn_req_t *) ((uintptr_t) data + 8);
    struct opal_proc_t *remote_proc = opal_proc_for_name (req->proc_name);
    mca_btl_base_endpoint_t *endpoint = mca_btl_uct_get_ep (&module->super, remote_proc);
    mca_btl_uct_tl_endpoint_t *tl_endpoint = endpoint->uct_eps[req->context_id] + req->tl_index;
    int64_t type = *((int64_t *) data);
    int32_t ep_flags;
    int rc;

    BTL_VERBOSE(("got connection request for endpoint %p. length = %lu", (void *) endpoint, length));

    if (NULL == endpoint) {
        BTL_ERROR(("could not create endpoint for connection request"));
        return UCS_ERR_UNREACHABLE;
    }

    assert (type < 2);

    if (0 == type) {
        /* create any necessary resources */
        rc = mca_btl_uct_endpoint_connect (module, endpoint, req->context_id, req->ep_addr, req->tl_index);
        if (OPAL_SUCCESS != rc && OPAL_ERR_OUT_OF_RESOURCE != rc) {
            BTL_ERROR(("could not setup rdma endpoint"));
            return UCS_ERR_UNREACHABLE;
        }

        ep_flags = opal_atomic_or_fetch_32 (&tl_endpoint->flags, MCA_BTL_UCT_ENDPOINT_FLAG_CONN_REC);
    } else {
        ep_flags = opal_atomic_or_fetch_32 (&tl_endpoint->flags, MCA_BTL_UCT_ENDPOINT_FLAG_CONN_REM_READY);
    }

    /* the connection is ready once we have received the connection data and also a connection ready
     * message. this might be overkill but there is little documentation at the UCT level on when
     * an endpoint can be used. */
    if ((ep_flags & (MCA_BTL_UCT_ENDPOINT_FLAG_CONN_REM_READY | MCA_BTL_UCT_ENDPOINT_FLAG_CONN_REC)) ==
        (MCA_BTL_UCT_ENDPOINT_FLAG_CONN_REM_READY | MCA_BTL_UCT_ENDPOINT_FLAG_CONN_REC)) {
        mca_btl_uct_base_frag_t *frag;

        /* to avoid a race with send adding pending frags grab the lock here */
        OPAL_THREAD_LOCK(&endpoint->ep_lock);
        (void) opal_atomic_or_fetch_32 (&tl_endpoint->flags, MCA_BTL_UCT_ENDPOINT_FLAG_CONN_READY);
        OPAL_THREAD_UNLOCK(&endpoint->ep_lock);

        opal_atomic_wmb ();

        OPAL_THREAD_SCOPED_LOCK(&module->lock, {
                OPAL_LIST_FOREACH(frag, &module->pending_frags, mca_btl_uct_base_frag_t) {
                    if (frag->context_id == req->context_id && endpoint == frag->endpoint) {
                        frag->ready = true;
                    }
                }
            });
    }

    return UCS_OK;
}

static int mca_btl_uct_setup_connection_tl (mca_btl_uct_module_t *module)
{
    ucs_status_t ucs_status;

    if (NULL == module->conn_tl) {
        return OPAL_ERR_NOT_SUPPORTED;
    }

    ucs_status = uct_iface_set_am_handler (module->conn_tl->uct_dev_contexts[0]->uct_iface, MCA_BTL_UCT_CONNECT_RDMA,
                                           mca_btl_uct_conn_req_cb, module, UCT_CB_FLAG_ASYNC);
    if (UCS_OK != ucs_status) {
        BTL_ERROR(("could not set active message handler for uct tl"));
    }

    return UCS_OK == ucs_status ? OPAL_SUCCESS : OPAL_ERROR;
}

mca_btl_uct_device_context_t *mca_btl_uct_context_create (mca_btl_uct_module_t *module, mca_btl_uct_tl_t *tl, int context_id)
{
    uct_iface_params_t iface_params = {.rndv_cb = NULL, .eager_cb = NULL, .stats_root = NULL,
                                       .rx_headroom = 0, .open_mode = UCT_IFACE_OPEN_MODE_DEVICE,
                                       .mode = {.device = {.tl_name = tl->uct_tl_name,
                                                           .dev_name = tl->uct_dev_name}}};
    mca_btl_uct_device_context_t *context;
    ucs_status_t ucs_status;

    context = calloc (1, sizeof (*context));
    if (OPAL_UNLIKELY(NULL == context)) {
        return NULL;
    }

    context->context_id = context_id;
    context->uct_btl = module;
    OBJ_CONSTRUCT(&context->completion_fifo, opal_fifo_t);
    OBJ_CONSTRUCT(&context->mutex, opal_recursive_mutex_t);

    do {
        /* apparently (in contradiction to the spec) UCT is *not* thread safe. because we have to
         * use our own locks just go ahead and use UCS_THREAD_MODE_SINGLE. if they ever fix their
         * api then change this back to UCS_THREAD_MODE_MULTI and remove the locks around the
         * various UCT calls. */
        ucs_status = uct_worker_create (module->ucs_async, UCS_THREAD_MODE_SINGLE, &context->uct_worker);
        if (UCS_OK != ucs_status) {
            BTL_VERBOSE(("could not create a UCT worker"));
            mca_btl_uct_context_destroy (context);
            context = NULL;
            break;
        }

        ucs_status = uct_iface_open (tl->uct_md->uct_md, context->uct_worker, &iface_params,
                                     tl->uct_tl_config, &context->uct_iface);
        if (UCS_OK != ucs_status) {
            BTL_VERBOSE(("could not open UCT interface. error code: %d", ucs_status));
            mca_btl_uct_context_destroy (context);
            context = NULL;
            break;
        }

        BTL_VERBOSE(("enabling progress for tl %p context id %d", (void *) tl, context_id));

#if HAVE_DECL_UCT_PROGRESS_THREAD_SAFE
        uct_iface_progress_enable (context->uct_iface, UCT_PROGRESS_THREAD_SAFE | UCT_PROGRESS_SEND |
                                   UCT_PROGRESS_RECV);
#else
        uct_iface_progress_enable (context->uct_iface, UCT_PROGRESS_SEND | UCT_PROGRESS_RECV);
#endif

	if (context_id > 0 && tl == module->am_tl) {
	    BTL_VERBOSE(("installing AM handler for tl %p context id %d", (void *) tl, context_id));
	    uct_iface_set_am_handler (context->uct_iface, MCA_BTL_UCT_FRAG, mca_btl_uct_am_handler,
                                      context, UCT_CB_FLAG_SYNC);
	}
    } while (0);

    return context;
}

void mca_btl_uct_context_destroy (mca_btl_uct_device_context_t *context)
{
    if (context->uct_iface) {
        uct_iface_close (context->uct_iface);
        context->uct_iface = NULL;
    }

    if (context->uct_worker) {
        uct_worker_destroy (context->uct_worker);
        context->uct_worker = NULL;
    }

    OBJ_DESTRUCT(&context->completion_fifo);
    free (context);
}

static int tl_compare (opal_list_item_t **a, opal_list_item_t **b)
{
    mca_btl_uct_tl_t *tl_a = (mca_btl_uct_tl_t *) *a;
    mca_btl_uct_tl_t *tl_b = (mca_btl_uct_tl_t *) *b;

    return tl_a->priority - tl_b->priority;
}

static mca_btl_uct_tl_t *mca_btl_uct_create_tl (mca_btl_uct_module_t *module, mca_btl_uct_md_t *md, uct_tl_resource_desc_t *tl_desc, int priority)
{
    mca_btl_uct_tl_t *tl = OBJ_NEW(mca_btl_uct_tl_t);
    ucs_status_t ucs_status;

    if (OPAL_UNLIKELY(NULL == tl)) {
        return NULL;
    }

    /* initialize btl tl structure */
    tl->uct_md = md;
    OBJ_RETAIN(md);

    tl->uct_tl_name = strdup (tl_desc->tl_name);
    tl->uct_dev_name = strdup (tl_desc->dev_name);
    tl->priority = priority;

    tl->uct_dev_contexts = calloc (MCA_BTL_UCT_MAX_WORKERS, sizeof (tl->uct_dev_contexts[0]));
    if (NULL == tl->uct_dev_contexts) {
        OBJ_RELEASE(tl);
        return NULL;
    }

    (void) uct_md_iface_config_read (md->uct_md, tl_desc->tl_name, NULL, NULL, &tl->uct_tl_config);

    /* always create a 0 context (needed to query) */
    tl->uct_dev_contexts[0] = mca_btl_uct_context_create (module, tl, 0);
    if (NULL == tl->uct_dev_contexts[0]) {
        BTL_VERBOSE(("could not create a uct device context"));
        OBJ_RELEASE(tl);
        return NULL;
    }

    /* only need to query one of the interfaces to get the attributes */
    ucs_status = uct_iface_query (tl->uct_dev_contexts[0]->uct_iface, &tl->uct_iface_attr);
    if (UCS_OK != ucs_status) {
        BTL_VERBOSE(("Error querying UCT interface"));
        OBJ_RELEASE(tl);
        return NULL;
    }

    BTL_VERBOSE(("Interface CAPS for tl %s::%s: 0x%lx", module->md_name, tl_desc->tl_name, (unsigned long) tl->uct_iface_attr.cap.flags));

    return tl;
}

static void mca_btl_uct_set_tl_rdma (mca_btl_uct_module_t *module, mca_btl_uct_tl_t *tl)
{
    BTL_VERBOSE(("tl %s is suitable for RDMA", tl->uct_tl_name));

    mca_btl_uct_module_set_atomic_flags (module, tl);

    module->super.btl_get_limit = tl->uct_iface_attr.cap.get.max_zcopy;
    if (tl->uct_iface_attr.cap.get.max_bcopy) {
        module->super.btl_get_alignment = 0;
        module->super.btl_get_local_registration_threshold = tl->uct_iface_attr.cap.get.max_bcopy;
    } else {
        /* this is overkill in terms of alignment but we have no way to enforce a minimum get size */
        module->super.btl_get_alignment = opal_next_poweroftwo_inclusive (tl->uct_iface_attr.cap.get.min_zcopy);
    }

    module->super.btl_put_limit = tl->uct_iface_attr.cap.put.max_zcopy;
    module->super.btl_put_alignment = 0;

    /* no registration needed when using short put */
    if (tl->uct_iface_attr.cap.put.max_bcopy > tl->uct_iface_attr.cap.put.max_short) {
        module->super.btl_put_local_registration_threshold = tl->uct_iface_attr.cap.put.max_bcopy;
    } else {
        module->super.btl_put_local_registration_threshold = tl->uct_iface_attr.cap.put.max_short;
    }

    module->rdma_tl = tl;
    OBJ_RETAIN(tl);

    tl->tl_index = (module->am_tl && tl != module->am_tl) ? 1 : 0;
    module->comm_tls[tl->tl_index] = tl;
    if (tl->max_device_contexts <= 1) {
	tl->max_device_contexts = mca_btl_uct_component.num_contexts_per_module;
    }
}

static void mca_btl_uct_set_tl_am (mca_btl_uct_module_t *module, mca_btl_uct_tl_t *tl)
{
    BTL_VERBOSE(("tl %s is suitable for active-messaging", tl->uct_tl_name));

    if (module->rdma_tl == tl) {
	module->shared_endpoints = true;
    }
    module->am_tl = tl;
    OBJ_RETAIN(tl);

    uct_iface_set_am_handler (tl->uct_dev_contexts[0]->uct_iface, MCA_BTL_UCT_FRAG,
                              mca_btl_uct_am_handler, tl->uct_dev_contexts[0], UCT_CB_FLAG_ASYNC);

    tl->tl_index = (module->rdma_tl && tl != module->rdma_tl) ? 1 : 0;
    module->comm_tls[tl->tl_index] = tl;
    if (tl->max_device_contexts <= 1) {
	tl->max_device_contexts = mca_btl_uct_component.num_contexts_per_module;
    }
}

static int mca_btl_uct_set_tl_conn (mca_btl_uct_module_t *module, mca_btl_uct_tl_t *tl)
{
    int rc;

    BTL_VERBOSE(("tl %s is suitable for making connections", tl->uct_tl_name));

    module->conn_tl = tl;
    rc = mca_btl_uct_setup_connection_tl (module);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    OBJ_RETAIN(tl);

    if (!tl->max_device_contexts) {
	/* if a tl is only being used to create connections do not bother with multiple
	 * contexts */
	tl->max_device_contexts = 1;
    }

    return OPAL_SUCCESS;
}

static int mca_btl_uct_evaluate_tl (mca_btl_uct_module_t *module, mca_btl_uct_tl_t *tl)
{
    int rc;

    BTL_VERBOSE(("evaluating tl %s", tl->uct_tl_name));
    if (NULL == module->rdma_tl && mca_btl_uct_tl_supports_rdma (tl)) {
	mca_btl_uct_set_tl_rdma (module, tl);
    }

    if (NULL == module->am_tl && mca_btl_uct_tl_support_am (tl)) {
	mca_btl_uct_set_tl_am (module, tl);
    }

    if (NULL == module->conn_tl && mca_btl_uct_tl_supports_conn (tl)) {
	rc = mca_btl_uct_set_tl_conn (module, tl);
        if (OPAL_SUCCESS != rc) {
            return rc;
        }
    }

    if (tl == module->rdma_tl || tl == module->am_tl) {
        BTL_VERBOSE(("tl has flags 0x%" PRIx64, tl->uct_iface_attr.cap.flags));
        module->super.btl_flags |= mca_btl_uct_module_flags (tl->uct_iface_attr.cap.flags);

	/* the bandwidth and latency numbers relate to both rdma and active messages. need to
	 * come up with a better estimate. */

	/* UCT bandwidth is in bytes/sec, BTL is in MB/sec */
	module->super.btl_bandwidth = (uint32_t) (tl->uct_iface_attr.bandwidth / 1048576.0);
	/* TODO -- figure out how to translate UCT latency to us */
	module->super.btl_latency = 1;
    }

    return OPAL_SUCCESS;
}

int mca_btl_uct_query_tls (mca_btl_uct_module_t *module, mca_btl_uct_md_t *md, uct_tl_resource_desc_t *tl_descs, unsigned tl_count)
{
    bool include = true, any = false;
    mca_btl_uct_tl_t *tl;
    opal_list_t tl_list;
    char **tl_filter;

    OBJ_CONSTRUCT(&tl_list, opal_list_t);

    tl_filter = opal_argv_split (mca_btl_uct_component.allowed_transports, ',');

    if ('^' == tl_filter[0][0]) {
	/* user has negated the include list */
	char *tmp = strdup (tl_filter[0] + 1);

	free (tl_filter[0]);
	tl_filter[0] = tmp;
	include = false;
    } else if (0 == strcmp (tl_filter[0], "any")) {
	any = true;
    }

    for (unsigned i = 0 ; i < tl_count ; ++i) {
	bool try_tl = any;
	int priority = 0;

	for (unsigned j = 0 ; tl_filter[j] && !try_tl ; ++j) {
	    try_tl = (0 == strcmp (tl_filter[j], tl_descs[i].tl_name)) == include;
	    priority = j;
	}

	if (!try_tl) {
	    continue;
	}

	tl = mca_btl_uct_create_tl (module, md, tl_descs + i, priority);

	if (tl) {
	    opal_list_append (&tl_list, &tl->super);
	}
    }

    if (0 == opal_list_get_size (&tl_list)) {
	BTL_VERBOSE(("no suitable tls match filter: %s", mca_btl_uct_component.allowed_transports));
	OBJ_DESTRUCT(&tl_list);
	return OPAL_ERR_NOT_AVAILABLE;
    }

    opal_list_sort (&tl_list, tl_compare);

    OPAL_LIST_FOREACH(tl, &tl_list, mca_btl_uct_tl_t) {
	mca_btl_uct_evaluate_tl (module, tl);
	if (NULL != module->am_tl && NULL != module->rdma_tl &&
	    (NULL != module->conn_tl || !(mca_btl_uct_tl_requires_connection_tl (module->am_tl) ||
					  mca_btl_uct_tl_requires_connection_tl (module->rdma_tl)))) {
	    /* all done */
	    break;
	}
    }

    if (NULL == module->rdma_tl) {
	/* no rdma tls */
	BTL_VERBOSE(("no rdma tl matched supplied filter. disabling RDMA support"));

	module->super.btl_put = NULL;
	module->super.btl_get = NULL;
	module->super.btl_atomic_fop = NULL;
	module->super.btl_atomic_op = NULL;
    }

    if (NULL == module->am_tl) {
	/* no active message tls == no send/recv */
	BTL_VERBOSE(("no active message tl matched supplied filter. disabling send/recv support"));

	module->super.btl_send = NULL;
	module->super.btl_sendi = NULL;
	module->super.btl_alloc = NULL;
	module->super.btl_free = NULL;
    }

    OPAL_LIST_DESTRUCT(&tl_list);

    if (!(NULL != module->am_tl && mca_btl_uct_tl_requires_connection_tl (module->am_tl)) &&
	!(NULL != module->rdma_tl && mca_btl_uct_tl_requires_connection_tl (module->rdma_tl)) &&
	module->conn_tl) {
	/* no connection tl needed for selected transports */
	OBJ_RELEASE(module->conn_tl);
	module->conn_tl = NULL;
    } else if (NULL == module->conn_tl) {
        BTL_VERBOSE(("a connection tl is required but no tls match the filter %s",
                     mca_btl_uct_component.allowed_transports));
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}
