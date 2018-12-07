#include "opal_config.h"

#include "common_ucx.h"
#include "common_ucx_wpool.h"
#include "common_ucx_wpool_int.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/base/mca_base_framework.h"
#include "opal/mca/pmix/pmix.h"
#include "opal/memoryhooks/memory.h"

#include <ucm/api/ucm.h>

/*******************************************************************************
 *******************************************************************************
 *
 * Worker Pool (wpool) framework
 * Used to manage multi-threaded implementation of UCX for ompi/OSC & OSHMEM
 *
 *******************************************************************************
 ******************************************************************************/

OBJ_CLASS_INSTANCE(_winfo_list_item_t, opal_list_item_t, NULL, NULL);
OBJ_CLASS_INSTANCE(_ctx_record_list_item_t, opal_list_item_t, NULL, NULL);
OBJ_CLASS_INSTANCE(_mem_record_list_item_t, opal_list_item_t, NULL, NULL);
OBJ_CLASS_INSTANCE(_tlocal_table_t, opal_list_item_t, NULL, NULL);

// TODO: Remove once debug is completed
#ifdef OPAL_COMMON_UCX_WPOOL_DBG
__thread FILE *tls_pf = NULL;
__thread int initialized = 0;
#endif

/* -----------------------------------------------------------------------------
 * Worker information (winfo) management functionality
 *----------------------------------------------------------------------------*/

static opal_common_ucx_winfo_t *
_winfo_create(opal_common_ucx_wpool_t *wpool)
{
    ucp_worker_params_t worker_params;
    ucp_worker_h worker;
    ucs_status_t status;
    opal_common_ucx_winfo_t *winfo = NULL;

    memset(&worker_params, 0, sizeof(worker_params));
    worker_params.field_mask = UCP_WORKER_PARAM_FIELD_THREAD_MODE;
    worker_params.thread_mode = UCS_THREAD_MODE_SINGLE;
    status = ucp_worker_create(wpool->ucp_ctx, &worker_params, &worker);
    if (UCS_OK != status) {
        MCA_COMMON_UCX_ERROR("ucp_worker_create failed: %d", status);
        goto exit;
    }

    winfo = calloc(1, sizeof(*winfo));
    if (NULL == winfo) {
        MCA_COMMON_UCX_ERROR("Cannot allocate memory for worker info");
        goto release_worker;
    }

    OBJ_CONSTRUCT(&winfo->mutex, opal_mutex_t);
    winfo->worker = worker;
    winfo->endpoints = NULL;
    winfo->comm_size = 0;
    winfo->released = 0;

    WPOOL_DBG_OUT(_dbg_winfo, "winfo = %p, worker = %p\n",
                  (void*)winfo, (void *)winfo->worker);
    return winfo;

release_worker:
    ucp_worker_destroy(worker);
exit:
    return winfo;
}

static void
_winfo_reset(opal_common_ucx_winfo_t *winfo)
{
    if(winfo->comm_size != 0) {
        size_t i;
        for (i = 0; i < winfo->comm_size; i++) {
            if (NULL != winfo->endpoints[i]){
                ucp_ep_destroy(winfo->endpoints[i]);
            }
        }
        free(winfo->endpoints);
    }
    winfo->endpoints = NULL;
    winfo->comm_size = 0;
    winfo->released = 0;
    WPOOL_DBG_OUT(_dbg_winfo, "winfo = %p, worker = %p\n",
                  (void*)winfo, (void*)winfo->worker);
}

static void
_winfo_release(opal_common_ucx_winfo_t *winfo)
{
    WPOOL_DBG_OUT(_dbg_winfo, "winfo = %p, worker = %p\n",
                  (void*)winfo, (void *)winfo->worker);
    OBJ_DESTRUCT(&winfo->mutex);
    ucp_worker_destroy(winfo->worker);
    free(winfo);
}

/* -----------------------------------------------------------------------------
 * Worker Pool management functionality
 *----------------------------------------------------------------------------*/

OPAL_DECLSPEC opal_common_ucx_wpool_t *
opal_common_ucx_wpool_allocate(void)
{
    opal_common_ucx_wpool_t *ptr = calloc(1, sizeof(opal_common_ucx_wpool_t));
    ptr->refcnt = 0;

    WPOOL_DBG_OUT(_dbg_wpool, "wpool = %p\n", (void *)ptr);
    return ptr;
}

OPAL_DECLSPEC void
opal_common_ucx_wpool_free(opal_common_ucx_wpool_t *wpool)
{
    assert(wpool->refcnt == 0);
    WPOOL_DBG_OUT(_dbg_wpool, "wpool = %p\n", (void *)wpool);
    free(wpool);
}

OPAL_DECLSPEC int
opal_common_ucx_wpool_init(opal_common_ucx_wpool_t *wpool,
                               int proc_world_size,
                               ucp_request_init_callback_t req_init_ptr,
                               size_t req_size, bool enable_mt)
{
    ucp_config_t *config = NULL;
    ucp_params_t context_params;
    opal_common_ucx_winfo_t *winfo;
    ucs_status_t status;
    int rc = OPAL_SUCCESS;

    wpool->refcnt++;
    WPOOL_DBG_OUT(_dbg_wpool, "wpool = %p, refctnrt = %d\n",
                  (void *)wpool, wpool->refcnt);

    if (1 < wpool->refcnt) {
        return rc;
    }

    wpool->cur_ctxid = wpool->cur_memid = 0;
    OBJ_CONSTRUCT(&wpool->mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&wpool->tls_list, opal_list_t);

    status = ucp_config_read("MPI", NULL, &config);
    if (UCS_OK != status) {
        MCA_COMMON_UCX_VERBOSE(1, "ucp_config_read failed: %d", status);
        return OPAL_ERROR;
    }

    /* initialize UCP context */
    memset(&context_params, 0, sizeof(context_params));
    context_params.field_mask = UCP_PARAM_FIELD_FEATURES |
                                UCP_PARAM_FIELD_MT_WORKERS_SHARED |
                                UCP_PARAM_FIELD_ESTIMATED_NUM_EPS |
                                UCP_PARAM_FIELD_REQUEST_INIT |
                                UCP_PARAM_FIELD_REQUEST_SIZE;
    context_params.features = UCP_FEATURE_RMA | UCP_FEATURE_AMO32 |
                              UCP_FEATURE_AMO64;
    context_params.mt_workers_shared = (enable_mt ? 1 : 0);
    context_params.estimated_num_eps = proc_world_size;
    context_params.request_init = req_init_ptr;
    context_params.request_size = req_size;

    status = ucp_init(&context_params, config, &wpool->ucp_ctx);
    ucp_config_release(config);
    if (UCS_OK != status) {
        MCA_COMMON_UCX_VERBOSE(1, "ucp_init failed: %d", status);
        rc = OPAL_ERROR;
        goto err_ucp_init;
    }

    /* create recv worker and add to idle pool */
    OBJ_CONSTRUCT(&wpool->idle_workers, opal_list_t);
    OBJ_CONSTRUCT(&wpool->active_workers, opal_list_t);

    winfo = _winfo_create(wpool);
    if (NULL == winfo) {
        MCA_COMMON_UCX_ERROR("Failed to create receive worker");
        rc = OPAL_ERROR;
        goto err_worker_create;
    }
    wpool->recv_worker = winfo->worker;

    status = ucp_worker_get_address(wpool->recv_worker,
                                    &wpool->recv_waddr, &wpool->recv_waddr_len);
    if (status != UCS_OK) {
        MCA_COMMON_UCX_VERBOSE(1, "ucp_worker_get_address failed: %d", status);
        rc = OPAL_ERROR;
        goto err_get_addr;
    }

    rc = _wpool_list_put(wpool, &wpool->idle_workers, winfo);
    if (rc) {
        goto err_wpool_add;
    }

    pthread_key_create(&wpool->tls_key, _tlocal_cleanup);

    WPOOL_DBG_OUT(_dbg_wpool, "wpool = %p. Done\n", (void *)wpool);
    return rc;

err_wpool_add:
    free(wpool->recv_waddr);
err_get_addr:
    if (NULL != wpool->recv_worker) {
        ucp_worker_destroy(wpool->recv_worker);
    }
 err_worker_create:
    ucp_cleanup(wpool->ucp_ctx);
 err_ucp_init:
    return rc;
}

OPAL_DECLSPEC
void opal_common_ucx_wpool_finalize(opal_common_ucx_wpool_t *wpool)
{
    _tlocal_table_t *tls_item = NULL, *tls_next;

    WPOOL_DBG_OUT(_dbg_wpool, "wpool = %p. Start\n", (void *)wpool);

    wpool->refcnt--;
    if (wpool->refcnt > 0) {
        WPOOL_DBG_OUT(_dbg_wpool, "wpool = %p. Still in use\n", (void *)wpool);
        return;
    }

    /* After this have been called no thread cleanup callback
     * will be called */
    pthread_key_delete(wpool->tls_key);

    /* Go over remaining TLS structures and release it */
    OPAL_LIST_FOREACH_SAFE(tls_item, tls_next, &wpool->tls_list,
                           _tlocal_table_t) {
        opal_list_remove_item(&wpool->tls_list, &tls_item->super);
        _common_ucx_tls_cleanup(tls_item);
        WPOOL_DBG_OUT(_dbg_wpool, "wpool = %p. Cleanup TLS = %p\n",
                      (void *)wpool, (void*)tls_item);
    }
    OBJ_DESTRUCT(&wpool->tls_list);

    /* Release the address here. recv worker will be released
     * below along with other idle workers */
    ucp_worker_release_address(wpool->recv_worker, wpool->recv_waddr);

    /* Go over the list, free idle list items */
    if (!opal_list_is_empty(&wpool->idle_workers)) {
        _winfo_list_item_t *item, *next;
        OPAL_LIST_FOREACH_SAFE(item, next, &wpool->idle_workers,
                               _winfo_list_item_t) {
            opal_list_remove_item(&wpool->idle_workers, &item->super);
            WPOOL_DBG_OUT(_dbg_wpool, "wpool = %p. Cleanup idle winfo = %p\n",
                          (void *)wpool, (void*)item->ptr);
            _winfo_release(item->ptr);
            OBJ_RELEASE(item);
        }
    }
    OBJ_DESTRUCT(&wpool->idle_workers);

    /* Release active workers. They are no longer active actually
     * because opal_common_ucx_wpool_finalize is being called. */
    if (!opal_list_is_empty(&wpool->active_workers)) {
        _winfo_list_item_t *item, *next;
        OPAL_LIST_FOREACH_SAFE(item, next, &wpool->active_workers,
                               _winfo_list_item_t) {
            opal_list_remove_item(&wpool->active_workers, &item->super);
            _winfo_reset(item->ptr);
            _winfo_release(item->ptr);
            WPOOL_DBG_OUT(_dbg_wpool, "wpool = %p. Cleanup active winfo = %p\n",
                          (void *)wpool, (void*)item->ptr);
            OBJ_RELEASE(item);
        }
    }
    OBJ_DESTRUCT(&wpool->active_workers);

    OBJ_DESTRUCT(&wpool->mutex);
    ucp_cleanup(wpool->ucp_ctx);
    WPOOL_DBG_OUT(_dbg_wpool, "wpool = %p. Done\n", (void *)wpool);
    return;
}

OPAL_DECLSPEC void
opal_common_ucx_wpool_progress(opal_common_ucx_wpool_t *wpool)
{
    _winfo_list_item_t *item = NULL, *next = NULL;

    /* Go over all active workers and progress them
     * TODO: may want to have some partitioning to progress only part of
     * workers */
    opal_mutex_lock(&wpool->mutex);
    OPAL_LIST_FOREACH_SAFE(item, next, &wpool->active_workers,
                           _winfo_list_item_t) {
        opal_common_ucx_winfo_t *winfo = item->ptr;
        opal_mutex_lock(&winfo->mutex);
        if( OPAL_UNLIKELY(winfo->released) ) {
            /* Do garbage collection of worker info's if needed */
            opal_list_remove_item(&wpool->active_workers, &item->super);
            _winfo_reset(winfo);
            opal_list_append(&wpool->idle_workers, &item->super);
        } else {
            /* Progress worker until there are existing events */
            while(ucp_worker_progress(winfo->worker));
        }
        opal_mutex_unlock(&winfo->mutex);
    }

    opal_mutex_unlock(&wpool->mutex);
}

static int
_wpool_list_put(opal_common_ucx_wpool_t *wpool, opal_list_t *list,
                opal_common_ucx_winfo_t *winfo)
{
    _winfo_list_item_t *item;

    WPOOL_DBG_OUT(_dbg_wpool, "wpool = %p, winfo = %p\n",
                  (void *)wpool, (void *)winfo);

    item = OBJ_NEW(_winfo_list_item_t);
    if (NULL == item) {
        MCA_COMMON_UCX_ERROR("Cannot allocate memory for winfo list item");
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    item->ptr = winfo;

    opal_mutex_lock(&wpool->mutex);
    opal_list_append(list, &item->super);
    opal_mutex_unlock(&wpool->mutex);

    return OPAL_SUCCESS;
}

static opal_common_ucx_winfo_t*
_wpool_list_get(opal_common_ucx_wpool_t *wpool, opal_list_t *list)
{
    opal_common_ucx_winfo_t *winfo = NULL;
    _winfo_list_item_t *item = NULL;

    opal_mutex_lock(&wpool->mutex);
    if (!opal_list_is_empty(list)) {
        item = (_winfo_list_item_t *)opal_list_get_first(list);
        opal_list_remove_item(list, &item->super);
    }
    opal_mutex_unlock(&wpool->mutex);

    if (item != NULL) {
        winfo = item->ptr;
        OBJ_RELEASE(item);
    }
    WPOOL_DBG_OUT(_dbg_wpool, "wpool = %p, winfo = %p\n",
                  (void *)wpool, (void *)winfo);
    return winfo;
}

static opal_common_ucx_winfo_t *
_wpool_get_idle(opal_common_ucx_wpool_t *wpool, size_t comm_size)
{
    opal_common_ucx_winfo_t *winfo;
    winfo = _wpool_list_get(wpool, &wpool->idle_workers);
    if (!winfo) {
        winfo = _winfo_create(wpool);
        if (!winfo) {
            MCA_COMMON_UCX_ERROR("Failed to allocate worker info structure");
            return NULL;
        }
    }

    WPOOL_DBG_OUT(_dbg_wpool, "wpool = %p, winfo = %p\n",
                  (void *)wpool, (void *)winfo);

    winfo->endpoints = calloc(comm_size, sizeof(ucp_ep_h));
    winfo->comm_size = comm_size;
    return winfo;
}

static int
_wpool_add_active(opal_common_ucx_wpool_t *wpool, opal_common_ucx_winfo_t *winfo)
{
    WPOOL_DBG_OUT(_dbg_wpool, "wpool = %p, winfo = %p\n",
                  (void *)wpool, (void *)winfo);
    return _wpool_list_put(wpool, &wpool->active_workers, winfo);
}

/* -----------------------------------------------------------------------------
 * Worker Pool Communication context management functionality
 *----------------------------------------------------------------------------*/

OPAL_DECLSPEC int
opal_common_ucx_wpctx_create(opal_common_ucx_wpool_t *wpool, int comm_size,
                             opal_common_ucx_exchange_func_t exchange_func,
                             void *exchange_metadata,
                             opal_common_ucx_ctx_t **ctx_ptr)
{
    opal_common_ucx_ctx_t *ctx = calloc(1, sizeof(*ctx));
    int ret = OPAL_SUCCESS;

    ctx->ctx_id = OPAL_ATOMIC_ADD_FETCH32(&wpool->cur_ctxid, 1);
    WPOOL_DBG_OUT(_dbg_ctx, "ctx_create: ctx_id = %d\n", (int)ctx->ctx_id);

    OBJ_CONSTRUCT(&ctx->mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&ctx->tls_workers, opal_list_t);
    ctx->released = 0;
    ctx->refcntr = 1; /* application holding the context */
    ctx->wpool = wpool;
    ctx->comm_size = comm_size;

    ctx->recv_worker_addrs = NULL;
    ctx->recv_worker_displs = NULL;
    ret = exchange_func(wpool->recv_waddr, wpool->recv_waddr_len,
                        &ctx->recv_worker_addrs,
                        &ctx->recv_worker_displs, exchange_metadata);
    if (ret != OPAL_SUCCESS) {
        goto error;
    }

    (*ctx_ptr) = ctx;
    WPOOL_DBG_OUT(_dbg_ctx,"wpool = %p, ctx = %p\n",
                  (void *)wpool, (void *)(*ctx_ptr));
    return ret;
error:
    OBJ_DESTRUCT(&ctx->mutex);
    OBJ_DESTRUCT(&ctx->tls_workers);
    free(ctx);
    (*ctx_ptr) = NULL;
    return ret;
}

OPAL_DECLSPEC void
opal_common_ucx_wpctx_release(opal_common_ucx_ctx_t *ctx)
{
    int my_refcntr = -1;

    WPOOL_DBG_OUT(_dbg_ctx, "ctx = %p\n", (void *)ctx);

    /* Application is expected to guarantee that no operation
     * is performed on the context that is being released */

    /* Mark that this context was released by application
     * Threads will use this flag to perform deferred cleanup */
    ctx->released = 1;

    /* Decrement the reference counter */
    my_refcntr = OPAL_ATOMIC_ADD_FETCH32(&ctx->refcntr, -1);

    /* Make sure that all the loads/stores are complete */
    opal_atomic_mb();

    /* If there is no more references to this handler
     * we can release it */
    if (0 == my_refcntr) {
        _common_ucx_wpctx_free(ctx);
    }
}

/* Final cleanup of the context structure
 * once all references cleared */
static void
_common_ucx_wpctx_free(opal_common_ucx_ctx_t *ctx)
{
    free(ctx->recv_worker_addrs);
    free(ctx->recv_worker_displs);
    OBJ_DESTRUCT(&ctx->mutex);
    OBJ_DESTRUCT(&ctx->tls_workers);
    WPOOL_DBG_OUT(_dbg_ctx, "ctx = %p\n", (void *)ctx);
    free(ctx);
}

/* Subscribe a new TLS to this context */
static int
_common_ucx_wpctx_append(opal_common_ucx_ctx_t *ctx,
                         opal_common_ucx_winfo_t *winfo)
{
    _ctx_record_list_item_t *item = OBJ_NEW(_ctx_record_list_item_t);
    if (NULL == item) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    /* Increment the reference counter */
    OPAL_ATOMIC_ADD_FETCH32(&ctx->refcntr, 1);

    /* Add new worker to the context */
    item->ptr = winfo;
    opal_mutex_lock(&ctx->mutex);
    opal_list_append(&ctx->tls_workers, &item->super);
    opal_mutex_unlock(&ctx->mutex);

    WPOOL_DBG_OUT(_dbg_ctx, "ctx = %p, winfo = %p\n",
                  (void *)ctx, (void *)winfo);
    return OPAL_SUCCESS;
}

/* Unsubscribe a particular TLS to this context */
static void
_common_ucx_wpctx_remove(opal_common_ucx_ctx_t *ctx,
                         opal_common_ucx_winfo_t *winfo)
{
    _ctx_record_list_item_t *item = NULL, *next;
    int my_refcntr = -1;

    opal_mutex_lock(&ctx->mutex);

    OPAL_LIST_FOREACH_SAFE(item, next, &ctx->tls_workers,
                           _ctx_record_list_item_t) {
        if (winfo == item->ptr) {
            opal_list_remove_item(&ctx->tls_workers, &item->super);
            opal_mutex_lock(&winfo->mutex);
            winfo->released = 1;
            opal_mutex_unlock(&winfo->mutex);
            OBJ_RELEASE(item);
            break;
        }
    }
    opal_mutex_unlock(&ctx->mutex);

    /* Make sure that all the loads/stores are complete */
    opal_atomic_rmb();

    /* Decrement the reference counter */
    my_refcntr = OPAL_ATOMIC_ADD_FETCH32(&ctx->refcntr, -1);

    /* a counterpart to the rmb above */
    opal_atomic_wmb();

    if (0 == my_refcntr) {
        /* All references to this data structure were removed
         * We can safely release communication context structure */
        _common_ucx_wpctx_free(ctx);
    }
    WPOOL_DBG_OUT(_dbg_ctx, "ctx = %p\n", (void *)ctx);
    return;
}

/* -----------------------------------------------------------------------------
 * Worker Pool Memory management functionality
 *----------------------------------------------------------------------------*/

OPAL_DECLSPEC
int opal_common_ucx_wpmem_create(opal_common_ucx_ctx_t *ctx,
                               void **mem_base, size_t mem_size,
                               opal_common_ucx_mem_type_t mem_type,
                               opal_common_ucx_exchange_func_t exchange_func,
                               void *exchange_metadata,
                               opal_common_ucx_wpmem_t **mem_ptr)
{
    opal_common_ucx_wpmem_t *mem = calloc(1, sizeof(*mem));
    void *rkey_addr = NULL;
    size_t rkey_addr_len;
    ucs_status_t status;
    int ret = OPAL_SUCCESS;

    mem->mem_id = OPAL_ATOMIC_ADD_FETCH32(&ctx->wpool->cur_memid, 1);

    WPOOL_DBG_OUT(_dbg_mem, "ctx = %p, mem_id = %d\n",
                  (void *)ctx, (int)mem->mem_id);

    mem->released = 0;
    mem->refcntr = 1; /* application holding this memory handler */
    mem->ctx = ctx;
    mem->mem_addrs = NULL;
    mem->mem_displs = NULL;

    ret = _comm_ucx_wpmem_map(ctx->wpool, mem_base, mem_size, &mem->memh,
                            mem_type);
    if (ret != OPAL_SUCCESS) {
        MCA_COMMON_UCX_VERBOSE(1, "_comm_ucx_mem_map failed: %d", ret);
        goto error_mem_map;
    }
    WPOOL_DBG_OUT(_dbg_mem, "\tbase = %p, memh = %p\n",
                  (void *)(*mem_base), (void *)(mem->memh));

    status = ucp_rkey_pack(ctx->wpool->ucp_ctx, mem->memh,
                           &rkey_addr, &rkey_addr_len);
    if (status != UCS_OK) {
        MCA_COMMON_UCX_VERBOSE(1, "ucp_rkey_pack failed: %d", status);
        ret = OPAL_ERROR;
        goto error_rkey_pack;
    }
    WPOOL_DBG_OUT(_dbg_mem, "\trkey_addr = %p, rkey_addr_len = %d\n",
                  (void *)rkey_addr, (int)rkey_addr_len);

    ret = exchange_func(rkey_addr, rkey_addr_len,
                        &mem->mem_addrs, &mem->mem_displs, exchange_metadata);
    WPOOL_DBG_OUT(_dbg_mem, "\tcomplete exchange");

    ucp_rkey_buffer_release(rkey_addr);
    if (ret != OPAL_SUCCESS) {
        goto error_rkey_pack;
    }

    /* Dont need the destructor here, will use
     * wpool-level destructor */
    pthread_key_create(&mem->mem_tls_key, NULL);

    (*mem_ptr) = mem;

    WPOOL_DBG_OUT(_dbg_mem, "mem = %p. Done\n", (void *)mem);
    return ret;

 error_rkey_pack:
    ucp_mem_unmap(ctx->wpool->ucp_ctx, mem->memh);
 error_mem_map:
    free(mem);
    (*mem_ptr) = NULL;
    return ret;
}

OPAL_DECLSPEC int
opal_common_ucx_wpmem_free(opal_common_ucx_wpmem_t *mem)
{
    int my_refcntr = -1;

    WPOOL_DBG_OUT(_dbg_mem, "mem = %p\n", (void *)mem);

    /* Mark that this memory handler has been called */
    mem->released = 1;

    /* Decrement the reference counter */
    my_refcntr = OPAL_ATOMIC_ADD_FETCH32(&mem->refcntr, -1);

    /* Make sure that all the loads/stores are complete */
    opal_atomic_wmb();

    if (0 == my_refcntr) {
        _common_ucx_wpmem_free(mem);
    }
    return OPAL_SUCCESS;
}


static int _comm_ucx_wpmem_map(opal_common_ucx_wpool_t *wpool,
                             void **base, size_t size, ucp_mem_h *memh_ptr,
                             opal_common_ucx_mem_type_t mem_type)
{
    ucp_mem_map_params_t mem_params;
    ucp_mem_attr_t mem_attrs;
    ucs_status_t status;
    int ret = OPAL_SUCCESS;

    WPOOL_DBG_OUT(_dbg_mem, "wpool = %p\n", (void *)wpool);

    memset(&mem_params, 0, sizeof(ucp_mem_map_params_t));
    mem_params.field_mask = UCP_MEM_MAP_PARAM_FIELD_ADDRESS |
                            UCP_MEM_MAP_PARAM_FIELD_LENGTH |
                            UCP_MEM_MAP_PARAM_FIELD_FLAGS;
    mem_params.length = size;
    if (mem_type == OPAL_COMMON_UCX_MEM_ALLOCATE_MAP) {
        mem_params.address = NULL;
        mem_params.flags = UCP_MEM_MAP_ALLOCATE;
    } else {
        mem_params.address = (*base);
    }

    status = ucp_mem_map(wpool->ucp_ctx, &mem_params, memh_ptr);
    if (status != UCS_OK) {
        MCA_COMMON_UCX_VERBOSE(1, "ucp_mem_map failed: %d", status);
        ret = OPAL_ERROR;
        return ret;
    }
    WPOOL_DBG_OUT(_dbg_mem, "\tmemh = %p\n", (void *)(*memh_ptr));

    mem_attrs.field_mask = UCP_MEM_ATTR_FIELD_ADDRESS | UCP_MEM_ATTR_FIELD_LENGTH;
    status = ucp_mem_query((*memh_ptr), &mem_attrs);
    if (status != UCS_OK) {
        MCA_COMMON_UCX_VERBOSE(1, "ucp_mem_query failed: %d", status);
        ret = OPAL_ERROR;
        goto error;
    }
    WPOOL_DBG_OUT(_dbg_mem, "\tmemh = %p\n", (void *)(*memh_ptr));

    assert(mem_attrs.length >= size);
    if (mem_type != OPAL_COMMON_UCX_MEM_ALLOCATE_MAP) {
        assert(mem_attrs.address == (*base));
    } else {
        (*base) = mem_attrs.address;
    }

    WPOOL_DBG_OUT(_dbg_mem, "\twpool = %p, addr = %p size = %d memh = %p\n",
                  (void *)wpool, (void *)(*base), (int)size, (void *)(*memh_ptr));
    return ret;
 error:
    ucp_mem_unmap(wpool->ucp_ctx, (*memh_ptr));
    return ret;
}

static void _common_ucx_wpmem_free(opal_common_ucx_wpmem_t *mem)
{
    pthread_key_delete(mem->mem_tls_key);
    free(mem->mem_addrs);
    free(mem->mem_displs);
    ucp_mem_unmap(mem->ctx->wpool->ucp_ctx, mem->memh);
    WPOOL_DBG_OUT(_dbg_mem, "mem = %p\n", (void *)mem);
    free(mem);
}

static int
_common_ucx_wpmem_signup(opal_common_ucx_wpmem_t *mem)
{
    /* Increment the reference counter */
    OPAL_ATOMIC_ADD_FETCH32(&mem->refcntr, 1);

    WPOOL_DBG_OUT(_dbg_mem, "mem = %p\n", (void *)mem);
    return OPAL_SUCCESS;
}

static void
_common_ucx_mem_signout(opal_common_ucx_wpmem_t *mem)
{
    int my_refcntr = -1;

    /* Make sure that all the loads are complete at this
     * point so if somebody else will see refcntr ==0
     * and release the structure we would have all we need
     */
    opal_atomic_rmb();

    /* Decrement the reference counter */
    my_refcntr = OPAL_ATOMIC_ADD_FETCH32(&mem->refcntr, -1);

    /* a counterpart to the rmb above */
    opal_atomic_wmb();

    if (0 == my_refcntr) {
        _common_ucx_wpmem_free(mem);
    }

    WPOOL_DBG_OUT(_dbg_mem, "mem = %p\n", (void *)mem);
    return;
}

/* -----------------------------------------------------------------------------
 * Worker Pool TLS management functions management functionality
 *----------------------------------------------------------------------------*/

static _tlocal_table_t* _common_ucx_tls_init(opal_common_ucx_wpool_t *wpool)
{
    _tlocal_table_t *tls = OBJ_NEW(_tlocal_table_t);

    if (tls == NULL) {
        // return OPAL_ERR_OUT_OF_RESOURCE
        return NULL;
    }

    tls->ctx_tbl = NULL;
    tls->ctx_tbl_size = 0;
    tls->mem_tbl = NULL;
    tls->mem_tbl_size = 0;

    /* Add this TLS to the global wpool structure for future
     * cleanup purposes */
    tls->wpool = wpool;
    opal_mutex_lock(&wpool->mutex);
    opal_list_append(&wpool->tls_list, &tls->super);
    opal_mutex_unlock(&wpool->mutex);

    if(_tlocal_tls_ctxtbl_extend(tls, 4)){
        MCA_COMMON_UCX_ERROR("Failed to allocate Worker Pool context table");
        return NULL;
    }
    if(_tlocal_tls_memtbl_extend(tls, 4)) {
        MCA_COMMON_UCX_ERROR("Failed to allocate Worker Pool memory table");
        return NULL;
    }

    pthread_setspecific(wpool->tls_key, tls);

    WPOOL_DBG_OUT(_dbg_tls, "tls = %p, wpool = %p\n",
                  (void *)tls, (void*)wpool);

    return tls;
}

static inline _tlocal_table_t *
_tlocal_get_tls(opal_common_ucx_wpool_t *wpool){
    _tlocal_table_t *tls = pthread_getspecific(wpool->tls_key);
    if( OPAL_UNLIKELY(NULL == tls) ) {
        tls = _common_ucx_tls_init(wpool);
    }
    WPOOL_DBG_OUT(_dbg_tls, "tls = %p, wpool = %p\n",
                  (void *)tls, (void*)wpool);
    return tls;
}

static void _tlocal_cleanup(void *arg)
{
    _tlocal_table_t *item = NULL, *next;
    _tlocal_table_t *tls = (_tlocal_table_t *)arg;
    opal_common_ucx_wpool_t *wpool = NULL;

    if (NULL == tls) {
        return;
    }
    wpool = tls->wpool;

    WPOOL_DBG_OUT(_dbg_tls, "tls = %p, wpool = %p\n",
                  (void *)tls, (void*)wpool);

    /* 1. Remove us from tls_list */
    tls->wpool = wpool;
    opal_mutex_lock(&wpool->mutex);
    OPAL_LIST_FOREACH_SAFE(item, next, &wpool->tls_list, _tlocal_table_t) {
        if (item == tls) {
            opal_list_remove_item(&wpool->tls_list, &item->super);
            break;
        }
    }
    opal_mutex_unlock(&wpool->mutex);
    _common_ucx_tls_cleanup(tls);
}

// TODO: don't want to inline this function
static void _common_ucx_tls_cleanup(_tlocal_table_t *tls)
{
    size_t i, size;

    // Cleanup memory table
    size = tls->mem_tbl_size;
    for (i = 0; i < size; i++) {
        if (!tls->mem_tbl[i]->mem_id){
            continue;
        }
        _tlocal_mem_record_cleanup(tls->mem_tbl[i]);
        free(tls->mem_tbl[i]);
    }

    // Cleanup ctx table
    size = tls->ctx_tbl_size;
    for (i = 0; i < size; i++) {
        if (!tls->ctx_tbl[i]->ctx_id){
            continue;
        }
        _tlocal_ctx_record_cleanup(tls->ctx_tbl[i]);
        free(tls->ctx_tbl[i]);
    }

    pthread_setspecific(tls->wpool->tls_key, NULL);

    WPOOL_DBG_OUT(_dbg_tls, "tls = %p, wpool = %p\n",
                  (void *)tls, (void*)tls->wpool);
    OBJ_RELEASE(tls);
    return;
}

static int
_tlocal_tls_ctxtbl_extend(_tlocal_table_t *tbl, size_t append)
{
    size_t i;
    size_t newsize = (tbl->ctx_tbl_size + append);
    tbl->ctx_tbl = realloc(tbl->ctx_tbl, newsize * sizeof(*tbl->ctx_tbl));
    for (i = tbl->ctx_tbl_size; i < newsize; i++) {
        tbl->ctx_tbl[i] = calloc(1, sizeof(*tbl->ctx_tbl[i]));
        if (NULL == tbl->ctx_tbl[i]) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

    }
    tbl->ctx_tbl_size = newsize;
    WPOOL_DBG_OUT(_dbg_tls, "new size = %d\n", (int)newsize);
    return OPAL_SUCCESS;
}

static int
_tlocal_tls_memtbl_extend(_tlocal_table_t *tbl, size_t append)
{
    size_t i;
    size_t newsize = (tbl->mem_tbl_size + append);

    tbl->mem_tbl = realloc(tbl->mem_tbl, newsize * sizeof(*tbl->mem_tbl));
    for (i = tbl->mem_tbl_size; i < tbl->mem_tbl_size + append; i++) {
        tbl->mem_tbl[i] = calloc(1, sizeof(*tbl->mem_tbl[i]));
        if (NULL == tbl->mem_tbl[i]) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
    }
    tbl->mem_tbl_size = newsize;
    WPOOL_DBG_OUT(_dbg_tls, "new size = %d\n", (int)newsize);
    return OPAL_SUCCESS;
}


static inline _tlocal_ctx_t *
_tlocal_ctx_search(_tlocal_table_t *tls, int ctx_id)
{
    size_t i;
    for(i=0; i<tls->ctx_tbl_size; i++) {
        if( tls->ctx_tbl[i]->ctx_id == ctx_id){
            return tls->ctx_tbl[i];
        }
    }
    WPOOL_DBG_OUT(_dbg_tls, "tls = %p, ctx_id = %d\n",
                  (void *)tls, (int)ctx_id);
    return NULL;
}

static int
_tlocal_ctx_record_cleanup(_tlocal_ctx_t *ctx_rec)
{
    if (0 == ctx_rec->ctx_id) {
        return OPAL_SUCCESS;
    }
    /* Remove myself from the communication context structure
     * This may result in context release as we are using
     * delayed cleanup */
    _common_ucx_wpctx_remove(ctx_rec->gctx, ctx_rec->winfo);

    WPOOL_DBG_OUT(_dbg_tls, "wpool = %p, winfo = %p, worker = %p\n",
                  (void*)ctx_rec->gctx->wpool, (void*)ctx_rec->winfo,
                  (void*)ctx_rec->winfo->worker);

    /* Erase the record so it can be reused */
    memset(ctx_rec, 0, sizeof(*ctx_rec));

    return OPAL_SUCCESS;
}

// TODO: Don't want to inline this (slow path)
static _tlocal_ctx_t *
_tlocal_add_ctx(_tlocal_table_t *tls, opal_common_ucx_ctx_t *ctx)
{
    size_t i, free_idx = -1;
    int rc;

    /* Try to find available record in the TLS table
     * In parallel perform deferred cleanups */
    for (i=0; i<tls->ctx_tbl_size; i++) {
        if (tls->ctx_tbl[i]->gctx->released ) {
            /* Found dirty record, need to clean first */
            _tlocal_ctx_record_cleanup(tls->ctx_tbl[i]);
        }
        if ((0 == tls->ctx_tbl[i]->ctx_id) && (0 > free_idx)) {
            /* Found clean record */
            free_idx = i;
        }
    }

    /* if needed - extend the table */
    if (0 > free_idx) {
        free_idx = tls->ctx_tbl_size;
        rc = _tlocal_tls_ctxtbl_extend(tls, 4);
        if (rc) {
            //TODO: error out
            return NULL;
        }
    }

    tls->ctx_tbl[free_idx]->ctx_id = ctx->ctx_id;
    tls->ctx_tbl[free_idx]->gctx = ctx;
    tls->ctx_tbl[free_idx]->winfo = _wpool_get_idle(tls->wpool, ctx->comm_size);
    if (NULL == tls->ctx_tbl[free_idx]->winfo) {
        MCA_COMMON_UCX_ERROR("Failed to allocate new worker");
        return NULL;
    }

    /* Make sure that we completed all the data structures before
     * placing the item to the list
     * NOTE: essentially we don't need this as list append is an
     * operation protected by mutex
     */
    opal_atomic_wmb();

    /* Add this worker to the active worker list */
    _wpool_add_active(tls->wpool, tls->ctx_tbl[free_idx]->winfo);

    /* add this worker into the context list */
    rc = _common_ucx_wpctx_append(ctx, tls->ctx_tbl[free_idx]->winfo);
    if (rc) {
        //TODO: error out
        return NULL;
    }

    WPOOL_DBG_OUT(_dbg_tls || _dbg_ctx, "tls = %p, ctx_rec = %p, winfo = %p\n",
                  (void *)tls, (void *)&tls->ctx_tbl[free_idx],
                  (void *)tls->ctx_tbl[free_idx]->winfo);

    /* All good - return the record */
    return tls->ctx_tbl[free_idx];
}

static int _tlocal_ctx_connect(_tlocal_ctx_t *ctx_rec, int target)
{
    ucp_ep_params_t ep_params;
    opal_common_ucx_winfo_t *winfo = ctx_rec->winfo;
    opal_common_ucx_ctx_t *gctx = ctx_rec->gctx;
    ucs_status_t status;
    int displ;

    memset(&ep_params, 0, sizeof(ucp_ep_params_t));
    ep_params.field_mask = UCP_EP_PARAM_FIELD_REMOTE_ADDRESS;

    opal_mutex_lock(&winfo->mutex);
    displ = gctx->recv_worker_displs[target];
    ep_params.address = (ucp_address_t *)&(gctx->recv_worker_addrs[displ]);
    status = ucp_ep_create(winfo->worker, &ep_params, &winfo->endpoints[target]);
    if (status != UCS_OK) {
        opal_mutex_unlock(&winfo->mutex);
        MCA_COMMON_UCX_VERBOSE(1, "ucp_ep_create failed: %d", status);
        return OPAL_ERROR;
    }
    WPOOL_DBG_OUT(_dbg_tls || _dbg_ctx, "worker = %p ep = %p\n",
                  (void *)winfo->worker, (void *)winfo->endpoints[target]);
    opal_mutex_unlock(&winfo->mutex);
    return OPAL_SUCCESS;
}

/* TLS memory management */

static inline _tlocal_mem_t *
_tlocal_search_mem(_tlocal_table_t *tls, int mem_id)
{
    size_t i;
    WPOOL_DBG_OUT(_dbg_tls || _dbg_mem, "tls = %p mem_id = %d\n",
                  (void *)tls, (int)mem_id);
    for(i=0; i<tls->mem_tbl_size; i++) {
        if( tls->mem_tbl[i]->mem_id == mem_id){
            return tls->mem_tbl[i];
        }
    }
    return NULL;
}

static void
_tlocal_mem_record_cleanup(_tlocal_mem_t *mem_rec)
{
    size_t i;
    WPOOL_DBG_OUT(_dbg_tls || _dbg_mem, "record=%p, is_freed = %d\n",
                  (void *)mem_rec, mem_rec->gmem->released);
    if (mem_rec->gmem->released) {
        return;
    }
    /* Remove myself from the memory context structure
     * This may result in context release as we are using
     * delayed cleanup */
    _common_ucx_mem_signout(mem_rec->gmem);
    WPOOL_DBG_OUT(_dbg_tls || _dbg_mem, "gmem = %p mem_rec = %p\n",
                  (void *)mem_rec->gmem, (void *)mem_rec);

    for(i = 0; i < mem_rec->gmem->ctx->comm_size; i++) {
        if (mem_rec->mem->rkeys[i]) {
            ucp_rkey_destroy(mem_rec->mem->rkeys[i]);
            WPOOL_DBG_OUT(_dbg_tls || _dbg_mem, "rkey_entry = %p\n",
                          (void *)mem_rec->mem->rkeys[i]);
        }
    }
    free(mem_rec->mem->rkeys);

    /* Release fast-path pointers */
    if (NULL != mem_rec->mem_tls_ptr) {
        free(mem_rec->mem_tls_ptr);
    }

    free(mem_rec->mem);

    memset(mem_rec, 0, sizeof(*mem_rec));
}

static _tlocal_mem_t *_tlocal_add_mem(_tlocal_table_t *tls,
                                       opal_common_ucx_wpmem_t *mem)
{
    size_t i, free_idx = -1;
    _tlocal_ctx_t *ctx_rec = NULL;
    int rc = OPAL_SUCCESS;

    /* Try to find available spot in the table */
    for (i=0; i<tls->mem_tbl_size; i++) {
        if (tls->mem_tbl[i]->gmem->released) {
            /* Found a dirty record. Need to clean it first */
            _tlocal_mem_record_cleanup(tls->mem_tbl[i]);
            break;
        }
        if ((0 == tls->mem_tbl[i]->mem_id) && (0 > free_idx)) {
            /* Found a clear record */
            free_idx = i;
        }
    }

    if (0 > free_idx){
        free_idx = tls->mem_tbl_size;
        rc = _tlocal_tls_memtbl_extend(tls, 4);
        if (rc != OPAL_SUCCESS) {
            //TODO: error out
            return NULL;
        }
        WPOOL_DBG_OUT(_dbg_tls || _dbg_mem, "tls = %p\n", (void *)tls);
    }
    tls->mem_tbl[free_idx]->mem_id = mem->mem_id;
    tls->mem_tbl[free_idx]->gmem = mem;
    tls->mem_tbl[free_idx]->mem = calloc(1, sizeof(*tls->mem_tbl[free_idx]->mem));

    ctx_rec = _tlocal_ctx_search(tls, mem->ctx->ctx_id);
    if (NULL == ctx_rec) {
        // TODO: act accordingly - cleanup
        return NULL;
    }
    WPOOL_DBG_OUT("tls = %p, ctx_id = %d\n",
                  (void *)tls, (int)mem->ctx->ctx_id);

    tls->mem_tbl[free_idx]->mem->worker = ctx_rec->winfo;
    tls->mem_tbl[free_idx]->mem->rkeys = calloc(mem->ctx->comm_size,
                                         sizeof(*tls->mem_tbl[free_idx]->mem->rkeys));

    tls->mem_tbl[free_idx]->mem_tls_ptr =
            calloc(1, sizeof(*tls->mem_tbl[free_idx]->mem_tls_ptr));
    tls->mem_tbl[free_idx]->mem_tls_ptr->winfo = ctx_rec->winfo;
    tls->mem_tbl[free_idx]->mem_tls_ptr->rkeys = tls->mem_tbl[free_idx]->mem->rkeys;
    pthread_setspecific(mem->mem_tls_key, tls->mem_tbl[free_idx]->mem_tls_ptr);

    /* Make sure that we completed all the data structures before
     * placing the item to the list
     * NOTE: essentially we don't need this as list append is an
     * operation protected by mutex
     */
    opal_atomic_wmb();

    rc = _common_ucx_wpmem_signup(mem);
    if (rc) {
        // TODO: error handling
        return NULL;
    }
    WPOOL_DBG_OUT(_dbg_tls || _dbg_mem,
                  "mem = %p, mem_tbl_entry = %p\n",
                  (void *)mem, (void *)tls->mem_tbl[free_idx]);

    return tls->mem_tbl[free_idx];
}

static int
_tlocal_mem_create_rkey(_tlocal_mem_t *mem_rec, ucp_ep_h ep, int target)
{
    _mem_info_t *minfo = mem_rec->mem;
    opal_common_ucx_wpmem_t *gmem = mem_rec->gmem;
    int displ = gmem->mem_displs[target];
    ucs_status_t status;

    status = ucp_ep_rkey_unpack(ep, &gmem->mem_addrs[displ],
                                &minfo->rkeys[target]);
    if (status != UCS_OK) {
        MCA_COMMON_UCX_VERBOSE(1, "ucp_ep_rkey_unpack failed: %d", status);
        return OPAL_ERROR;
    }
    WPOOL_DBG_OUT(_dbg_tls || _dbg_mem, "mem_rec = %p ep = %p target = %d\n",
                  (void *)mem_rec, (void *)ep, target);
    return OPAL_SUCCESS;
}

/* Get the TLS in case of slow path (not everything has been yet initialized */
OPAL_DECLSPEC int
opal_common_ucx_tlocal_fetch_spath(opal_common_ucx_wpmem_t *mem, int target)
{
    _tlocal_table_t *tls = NULL;
    _tlocal_ctx_t *ctx_rec = NULL;
    opal_common_ucx_winfo_t *winfo = NULL;
    _tlocal_mem_t *mem_rec = NULL;
    _mem_info_t *mem_info = NULL;
    ucp_ep_h ep;
    int rc = OPAL_SUCCESS;

    tls = _tlocal_get_tls(mem->ctx->wpool);
    WPOOL_DBG_OUT(_dbg_tls || _dbg_mem, "tls = %p\n",(void*)tls);

    /* Obtain the worker structure */
    ctx_rec = _tlocal_ctx_search(tls, mem->ctx->ctx_id);

    WPOOL_DBG_OUT(_dbg_tls || _dbg_mem, "ctx_id = %d, ctx_rec=%p\n",
                  (int)mem->ctx->ctx_id, (void *)ctx_rec);
    if (OPAL_UNLIKELY(NULL == ctx_rec)) {
        ctx_rec = _tlocal_add_ctx(tls, mem->ctx);
        if (NULL == ctx_rec) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
        WPOOL_DBG_OUT("_tlocal_fetch(after _tlocal_add_ctx): tls = %p ctx = %p\n",
                      (void *)tls, (void *)mem->ctx);
    }
    winfo = ctx_rec->winfo;
    WPOOL_DBG_OUT(_dbg_tls || _dbg_mem, "winfo = %p ctx=%p\n",
                  (void *)winfo, (void *)mem->ctx);

    /* Obtain the endpoint */
    if (OPAL_UNLIKELY(NULL == winfo->endpoints[target])) {
        rc = _tlocal_ctx_connect(ctx_rec, target);
        if (rc != OPAL_SUCCESS) {
            return rc;
        }
        WPOOL_DBG_OUT(_dbg_tls || _dbg_mem, "ctx_rec = %p target = %d\n",
                      (void *)ctx_rec, target);
    }
    ep = winfo->endpoints[target];
    WPOOL_DBG_OUT(_dbg_tls || _dbg_mem, "ep = %p\n", (void *)ep);

    /* Obtain the memory region info */
    mem_rec = _tlocal_search_mem(tls, mem->mem_id);
    WPOOL_DBG_OUT(_dbg_tls || _dbg_mem, "tls = %p mem_rec = %p mem_id = %d\n",
                  (void *)tls, (void *)mem_rec, (int)mem->mem_id);
    if (OPAL_UNLIKELY(mem_rec == NULL)) {
        mem_rec = _tlocal_add_mem(tls, mem);
        WPOOL_DBG_OUT(_dbg_tls || _dbg_mem, "tls = %p mem = %p\n",
                      (void *)tls, (void *)mem);
        if (NULL == mem_rec) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
    }
    mem_info = mem_rec->mem;
    WPOOL_DBG_OUT(_dbg_tls || _dbg_mem, "mem_info = %p\n", (void *)mem_info);

    /* Obtain the rkey */
    if (OPAL_UNLIKELY(NULL == mem_info->rkeys[target])) {
        /* Create the rkey */
        rc = _tlocal_mem_create_rkey(mem_rec, ep, target);
        if (rc) {
            return rc;
        }
        WPOOL_DBG_OUT(_dbg_tls || _dbg_mem, "creating rkey ...\n");
    }

    return OPAL_SUCCESS;
}

OPAL_DECLSPEC int
opal_common_ucx_wpmem_flush(opal_common_ucx_wpmem_t *mem,
                          opal_common_ucx_flush_scope_t scope,
                          int target)
{
    _ctx_record_list_item_t *item;
    opal_common_ucx_ctx_t *ctx = mem->ctx;
    int rc = OPAL_SUCCESS;

    WPOOL_DBG_OUT(_dbg_tls || _dbg_mem, "mem = %p, target = %d\n",
                  (void *)mem, target);

    opal_mutex_lock(&ctx->mutex);

    OPAL_LIST_FOREACH(item, &ctx->tls_workers, _ctx_record_list_item_t) {
        switch (scope) {
        case OPAL_COMMON_UCX_SCOPE_WORKER:
            opal_mutex_lock(&item->ptr->mutex);
            rc = opal_common_ucx_worker_flush(item->ptr->worker);
            if (rc != OPAL_SUCCESS) {
                MCA_COMMON_UCX_ERROR("opal_common_ucx_worker_flush failed: %d",
                                     rc);
                rc = OPAL_ERROR;
            }
            WPOOL_DBG_OUT(_dbg_tls || _dbg_mem, "worker = %p\n",
                          (void *)item->ptr->worker);
            opal_mutex_unlock(&item->ptr->mutex);
            break;
        case OPAL_COMMON_UCX_SCOPE_EP:
            if (NULL != item->ptr->endpoints[target] ) {
                opal_mutex_lock(&item->ptr->mutex);
                rc = opal_common_ucx_ep_flush(item->ptr->endpoints[target],
                                              item->ptr->worker);
                if (rc != OPAL_SUCCESS) {
                    MCA_COMMON_UCX_ERROR("opal_common_ucx_ep_flush failed: %d",
                                         rc);
                    rc = OPAL_ERROR;
                }
                WPOOL_DBG_OUT(_dbg_tls || _dbg_mem,
                              "target = %d, ep = %p worker = %p\n",
                              (int)target,
                              (void *)item->ptr->endpoints[target],
                              (void *)item->ptr->worker);
                opal_mutex_unlock(&item->ptr->mutex);
            }
        }
    }
    opal_mutex_unlock(&ctx->mutex);

    return rc;
}

OPAL_DECLSPEC int
opal_common_ucx_wpmem_fence(opal_common_ucx_wpmem_t *mem) {
    /* TODO */
    return OPAL_SUCCESS;
}

