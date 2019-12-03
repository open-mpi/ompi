/*
*2019.12.30-Modification for coll_ucx
*        Huawei Technologies Co., Ltd. 2019.
*/

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

    OBJ_CONSTRUCT(&winfo->mutex, opal_recursive_mutex_t);
    winfo->worker = worker;
    winfo->endpoints = NULL;
    winfo->comm_size = 0;
    winfo->released = 0;
    winfo->inflight_ops = NULL;
    winfo->global_inflight_ops = 0;
    winfo->inflight_req = UCS_OK;

    return winfo;

release_worker:
    ucp_worker_destroy(worker);
exit:
    return winfo;
}

static void
_winfo_reset(opal_common_ucx_winfo_t *winfo)
{
    if (winfo->inflight_req != UCS_OK) {
        opal_common_ucx_wait_request_mt(winfo->inflight_req,
                                        OPAL_COMMON_UCX_REQUEST_TYPE_UCP,
                                        "opal_common_ucx_flush");
        winfo->inflight_req = UCS_OK;
    }

    assert(winfo->global_inflight_ops == 0);

    if(winfo->comm_size != 0) {
        size_t i;
        for (i = 0; i < winfo->comm_size; i++) {
            if (NULL != winfo->endpoints[i]){
                ucp_ep_destroy(winfo->endpoints[i]);
            }
            assert(winfo->inflight_ops[i] == 0);
        }
        free(winfo->endpoints);
        free(winfo->inflight_ops);
    }
    winfo->endpoints = NULL;
    winfo->comm_size = 0;
    winfo->released = 0;
}

static void
_winfo_release(opal_common_ucx_winfo_t *winfo)
{
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

    return ptr;
}

OPAL_DECLSPEC void
opal_common_ucx_wpool_free(opal_common_ucx_wpool_t *wpool)
{
    assert(wpool->refcnt == 0);
    free(wpool);
}

OPAL_DECLSPEC int
opal_common_ucx_wpool_init(opal_common_ucx_wpool_t *wpool,
                           int proc_world_size, bool enable_mt)
{
    ucp_config_t *config = NULL;
    ucp_params_t context_params;
    opal_common_ucx_winfo_t *winfo;
    ucs_status_t status;
    int rc = OPAL_SUCCESS;

    wpool->refcnt++;

    if (1 < wpool->refcnt) {
        return rc;
    }

    OBJ_CONSTRUCT(&wpool->mutex, opal_recursive_mutex_t);
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
    context_params.request_init = opal_common_ucx_req_init;
    context_params.request_size = sizeof(opal_common_ucx_request_t);

#if HAVE_DECL_UCP_PARAM_FIELD_ESTIMATED_NUM_PPN
    context_params.estimated_num_ppn = opal_process_info.num_local_peers + 1;
    context_params.field_mask       |= UCP_PARAM_FIELD_ESTIMATED_NUM_PPN;
#endif

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
    wpool->dflt_worker = winfo->worker;

    status = ucp_worker_get_address(wpool->dflt_worker,
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

    opal_tsd_key_create(&wpool->tls_key, _tlocal_cleanup);

    return rc;

err_wpool_add:
    free(wpool->recv_waddr);
err_get_addr:
    if (NULL != wpool->dflt_worker) {
        ucp_worker_destroy(wpool->dflt_worker);
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

    wpool->refcnt--;
    if (wpool->refcnt > 0) {
        return;
    }

    /* After this have been called no thread cleanup callback
     * will be called */
    opal_tsd_key_delete(wpool->tls_key);

    /* Go over remaining TLS structures and release it */
    OPAL_LIST_FOREACH_SAFE(tls_item, tls_next, &wpool->tls_list,
                           _tlocal_table_t) {
        opal_list_remove_item(&wpool->tls_list, &tls_item->super);
        _common_ucx_tls_cleanup(tls_item);
    }
    OBJ_DESTRUCT(&wpool->tls_list);

    /* Release the address here. recv worker will be released
     * below along with other idle workers */
    ucp_worker_release_address(wpool->dflt_worker, wpool->recv_waddr);

    /* Go over the list, free idle list items */
    if (!opal_list_is_empty(&wpool->idle_workers)) {
        _winfo_list_item_t *item, *next;
        OPAL_LIST_FOREACH_SAFE(item, next, &wpool->idle_workers,
                               _winfo_list_item_t) {
            opal_list_remove_item(&wpool->idle_workers, &item->super);
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
            OBJ_RELEASE(item);
        }
    }
    OBJ_DESTRUCT(&wpool->active_workers);

    OBJ_DESTRUCT(&wpool->mutex);
    ucp_cleanup(wpool->ucp_ctx);
    return;
}

OPAL_DECLSPEC void
opal_common_ucx_wpool_progress(opal_common_ucx_wpool_t *wpool)
{
    _winfo_list_item_t *item = NULL, *next = NULL;

    /* Go over all active workers and progress them
     * TODO: may want to have some partitioning to progress only part of
     * workers */
    if (!opal_mutex_trylock (&wpool->mutex)) {
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
}

static int
_wpool_list_put(opal_common_ucx_wpool_t *wpool, opal_list_t *list,
                opal_common_ucx_winfo_t *winfo)
{
    _winfo_list_item_t *item;

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

    winfo->endpoints = calloc(comm_size, sizeof(ucp_ep_h));
    winfo->inflight_ops = calloc(comm_size, sizeof(short));
    winfo->comm_size = comm_size;
    return winfo;
}

static int
_wpool_add_active(opal_common_ucx_wpool_t *wpool, opal_common_ucx_winfo_t *winfo)
{
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

    OBJ_CONSTRUCT(&ctx->mutex, opal_recursive_mutex_t);
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
                                 char **my_mem_addr,
                                 int *my_mem_addr_size,
                               opal_common_ucx_wpmem_t **mem_ptr)
{
    opal_common_ucx_wpmem_t *mem = calloc(1, sizeof(*mem));
    void *rkey_addr = NULL;
    size_t rkey_addr_len;
    ucs_status_t status;
    int ret = OPAL_SUCCESS;

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

    status = ucp_rkey_pack(ctx->wpool->ucp_ctx, mem->memh,
                           &rkey_addr, &rkey_addr_len);
    if (status != UCS_OK) {
        MCA_COMMON_UCX_VERBOSE(1, "ucp_rkey_pack failed: %d", status);
        ret = OPAL_ERROR;
        goto error_rkey_pack;
    }

    ret = exchange_func(rkey_addr, rkey_addr_len,
                        &mem->mem_addrs, &mem->mem_displs, exchange_metadata);
    if (ret != OPAL_SUCCESS) {
        goto error_rkey_pack;
    }

    /* Dont need the destructor here, will use
     * wpool-level destructor */
    opal_tsd_key_create(&mem->mem_tls_key, NULL);

    (*mem_ptr) = mem;
    (*my_mem_addr) = rkey_addr;
    (*my_mem_addr_size) = rkey_addr_len;

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

    mem_attrs.field_mask = UCP_MEM_ATTR_FIELD_ADDRESS | UCP_MEM_ATTR_FIELD_LENGTH;
    status = ucp_mem_query((*memh_ptr), &mem_attrs);
    if (status != UCS_OK) {
        MCA_COMMON_UCX_VERBOSE(1, "ucp_mem_query failed: %d", status);
        ret = OPAL_ERROR;
        goto error;
    }

    assert(mem_attrs.length >= size);
    if (mem_type != OPAL_COMMON_UCX_MEM_ALLOCATE_MAP) {
        assert(mem_attrs.address == (*base));
    } else {
        (*base) = mem_attrs.address;
    }

    return ret;
 error:
    ucp_mem_unmap(wpool->ucp_ctx, (*memh_ptr));
    return ret;
}

static void _common_ucx_wpmem_free(opal_common_ucx_wpmem_t *mem)
{
    opal_tsd_key_delete(mem->mem_tls_key);
    free(mem->mem_addrs);
    free(mem->mem_displs);
    ucp_mem_unmap(mem->ctx->wpool->ucp_ctx, mem->memh);
    free(mem);
}

static int
_common_ucx_wpmem_signup(opal_common_ucx_wpmem_t *mem)
{
    /* Increment the reference counter */
    OPAL_ATOMIC_ADD_FETCH32(&mem->refcntr, 1);
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

    opal_tsd_setspecific(wpool->tls_key, tls);

    return tls;
}

static inline _tlocal_table_t *
_tlocal_get_tls(opal_common_ucx_wpool_t *wpool){
    _tlocal_table_t *tls;
    int rc = opal_tsd_getspecific(wpool->tls_key, (void**)&tls);

    if (OPAL_SUCCESS != rc) {
        return NULL;
    }

    if (OPAL_UNLIKELY(NULL == tls)) {
        tls = _common_ucx_tls_init(wpool);
    }
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
        if (NULL != tls->mem_tbl[i]->gmem){
            _tlocal_mem_record_cleanup(tls->mem_tbl[i]);
        }

        free(tls->mem_tbl[i]);
    }

    // Cleanup ctx table
    size = tls->ctx_tbl_size;
    for (i = 0; i < size; i++) {
        if (NULL != tls->ctx_tbl[i]->gctx){
            assert(tls->ctx_tbl[i]->refcnt == 0);
            _tlocal_ctx_record_cleanup(tls->ctx_tbl[i]);
        }
        free(tls->ctx_tbl[i]);
    }

    opal_tsd_setspecific(tls->wpool->tls_key, NULL);

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
    return OPAL_SUCCESS;
}


static inline _tlocal_ctx_t *
_tlocal_ctx_search(_tlocal_table_t *tls, opal_common_ucx_ctx_t *ctx)
{
    size_t i;
    for(i=0; i<tls->ctx_tbl_size; i++) {
        if (tls->ctx_tbl[i]->gctx == ctx){
            return tls->ctx_tbl[i];
        }
    }
    return NULL;
}

static int
_tlocal_ctx_record_cleanup(_tlocal_ctx_t *ctx_rec)
{
    if (NULL == ctx_rec->gctx) {
        return OPAL_SUCCESS;
    }

    if (ctx_rec->refcnt > 0) {
        return OPAL_SUCCESS;
    }

    /* Remove myself from the communication context structure
     * This may result in context release as we are using
     * delayed cleanup */
    _common_ucx_wpctx_remove(ctx_rec->gctx, ctx_rec->winfo);

    /* Erase the record so it can be reused */
    memset(ctx_rec, 0, sizeof(*ctx_rec));

    return OPAL_SUCCESS;
}

// TODO: Don't want to inline this (slow path)
static _tlocal_ctx_t *
_tlocal_add_ctx(_tlocal_table_t *tls, opal_common_ucx_ctx_t *ctx)
{
    size_t i, free_idx = -1;
    int rc, found = 0;

    /* Try to find available record in the TLS table
     * In parallel perform deferred cleanups */
    for (i=0; i<tls->ctx_tbl_size; i++) {
        if (NULL != tls->ctx_tbl[i]->gctx && tls->ctx_tbl[i]->refcnt == 0) {
            if (tls->ctx_tbl[i]->gctx->released ) {
                /* Found dirty record, need to clean first */
                _tlocal_ctx_record_cleanup(tls->ctx_tbl[i]);
            }
        }
        if ((NULL == tls->ctx_tbl[i]->gctx) && !found) {
            /* Found clean record */
            free_idx = i;
            found = 1;
        }
    }

    /* if needed - extend the table */
    if (!found) {
        free_idx = tls->ctx_tbl_size;
        rc = _tlocal_tls_ctxtbl_extend(tls, 4);
        if (rc) {
            //TODO: error out
            return NULL;
        }
    }

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
    opal_mutex_unlock(&winfo->mutex);
    return OPAL_SUCCESS;
}

/* TLS memory management */

static inline _tlocal_mem_t *
_tlocal_search_mem(_tlocal_table_t *tls, opal_common_ucx_wpmem_t *gmem)
{
    size_t i;
    for(i=0; i<tls->mem_tbl_size; i++) {
        if( tls->mem_tbl[i]->gmem == gmem){
            return tls->mem_tbl[i];
        }
    }
    return NULL;
}

static void
_tlocal_mem_record_cleanup(_tlocal_mem_t *mem_rec)
{
    size_t i;

    for(i = 0; i < mem_rec->gmem->ctx->comm_size; i++) {
        if (mem_rec->mem->rkeys[i]) {
            ucp_rkey_destroy(mem_rec->mem->rkeys[i]);
        }
    }
    free(mem_rec->mem->rkeys);

    /* Remove myself from the memory context structure
     * This may result in context release as we are using
     * delayed cleanup */
    _common_ucx_mem_signout(mem_rec->gmem);

    /* Release fast-path pointers */
    if (NULL != mem_rec->mem_tls_ptr) {
        free(mem_rec->mem_tls_ptr);
    }

    assert(mem_rec->ctx_rec != NULL);
    OPAL_ATOMIC_ADD_FETCH32(&mem_rec->ctx_rec->refcnt, -1);
    assert(mem_rec->ctx_rec->refcnt >= 0);

    free(mem_rec->mem);

    memset(mem_rec, 0, sizeof(*mem_rec));
}

static _tlocal_mem_t *_tlocal_add_mem(_tlocal_table_t *tls,
                                       opal_common_ucx_wpmem_t *mem)
{
    size_t i, free_idx = -1;
    _tlocal_ctx_t *ctx_rec = NULL;
    int rc = OPAL_SUCCESS, found = 0;

    /* Try to find available spot in the table */
    for (i=0; i<tls->mem_tbl_size; i++) {
        if (NULL != tls->mem_tbl[i]->gmem) {
            if (tls->mem_tbl[i]->gmem->released) {
                /* Found a dirty record. Need to clean it first */
                _tlocal_mem_record_cleanup(tls->mem_tbl[i]);
            }
        }
        if ((NULL == tls->mem_tbl[i]->gmem) && !found) {
            /* Found a clear record */
            free_idx = i;
            found = 1;
        }
    }

    if (!found){
        free_idx = tls->mem_tbl_size;
        rc = _tlocal_tls_memtbl_extend(tls, 4);
        if (rc != OPAL_SUCCESS) {
            //TODO: error out
            return NULL;
        }
    }

    tls->mem_tbl[free_idx]->gmem = mem;
    tls->mem_tbl[free_idx]->mem = calloc(1, sizeof(*tls->mem_tbl[free_idx]->mem));

    ctx_rec = _tlocal_ctx_search(tls, mem->ctx);
    if (NULL == ctx_rec) {
        // TODO: act accordingly - cleanup
        return NULL;
    }

    tls->mem_tbl[free_idx]->ctx_rec = ctx_rec;
    OPAL_ATOMIC_ADD_FETCH32(&ctx_rec->refcnt, 1);

    tls->mem_tbl[free_idx]->mem->worker = ctx_rec->winfo;
    tls->mem_tbl[free_idx]->mem->rkeys = calloc(mem->ctx->comm_size,
                                         sizeof(*tls->mem_tbl[free_idx]->mem->rkeys));

    tls->mem_tbl[free_idx]->mem_tls_ptr =
            calloc(1, sizeof(*tls->mem_tbl[free_idx]->mem_tls_ptr));
    tls->mem_tbl[free_idx]->mem_tls_ptr->winfo = ctx_rec->winfo;
    tls->mem_tbl[free_idx]->mem_tls_ptr->rkeys = tls->mem_tbl[free_idx]->mem->rkeys;
    opal_tsd_setspecific(mem->mem_tls_key, tls->mem_tbl[free_idx]->mem_tls_ptr);

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

    /* Obtain the worker structure */
    ctx_rec = _tlocal_ctx_search(tls, mem->ctx);

    if (OPAL_UNLIKELY(NULL == ctx_rec)) {
        ctx_rec = _tlocal_add_ctx(tls, mem->ctx);
        if (NULL == ctx_rec) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
    }
    winfo = ctx_rec->winfo;

    /* Obtain the endpoint */
    if (OPAL_UNLIKELY(NULL == winfo->endpoints[target])) {
        rc = _tlocal_ctx_connect(ctx_rec, target);
        if (rc != OPAL_SUCCESS) {
            return rc;
        }
    }
    ep = winfo->endpoints[target];

    /* Obtain the memory region info */
    mem_rec = _tlocal_search_mem(tls, mem);
    if (OPAL_UNLIKELY(mem_rec == NULL)) {
        mem_rec = _tlocal_add_mem(tls, mem);
        if (NULL == mem_rec) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
    }
    mem_info = mem_rec->mem;

    /* Obtain the rkey */
    if (OPAL_UNLIKELY(NULL == mem_info->rkeys[target])) {
        /* Create the rkey */
        rc = _tlocal_mem_create_rkey(mem_rec, ep, target);
        if (rc) {
            return rc;
        }
    }

    return OPAL_SUCCESS;
}

OPAL_DECLSPEC int
opal_common_ucx_winfo_flush(opal_common_ucx_winfo_t *winfo, int target,
                            opal_common_ucx_flush_type_t type,
                            opal_common_ucx_flush_scope_t scope,
                            ucs_status_ptr_t *req_ptr)
{
    ucs_status_ptr_t req;
    ucs_status_t status = UCS_OK;
    int rc = OPAL_SUCCESS;

#if HAVE_DECL_UCP_EP_FLUSH_NB
    if (scope == OPAL_COMMON_UCX_SCOPE_EP) {
        req = ucp_ep_flush_nb(winfo->endpoints[target], 0, opal_common_ucx_empty_complete_cb);
    } else {
        req = ucp_worker_flush_nb(winfo->worker, 0, opal_common_ucx_empty_complete_cb);
    }
    if (UCS_PTR_IS_PTR(req)) {
        ((opal_common_ucx_request_t *)req)->winfo = winfo;
    }

    if(OPAL_COMMON_UCX_FLUSH_B) {
        rc = opal_common_ucx_wait_request_mt(req,
                                             OPAL_COMMON_UCX_REQUEST_TYPE_UCP,
                                             "ucp_ep_flush_nb");
    } else {
        *req_ptr = req;
    }
    return rc;
#endif
    switch (type) {
    case OPAL_COMMON_UCX_FLUSH_NB_PREFERRED:
    case OPAL_COMMON_UCX_FLUSH_B:
        if (scope == OPAL_COMMON_UCX_SCOPE_EP) {
            status = ucp_ep_flush(winfo->endpoints[target]);
        } else {
            status = ucp_worker_flush(winfo->worker);
        }
        rc = (status == UCS_OK) ? OPAL_SUCCESS : OPAL_ERROR;
    case OPAL_COMMON_UCX_FLUSH_NB:
    default:
        rc = OPAL_ERROR;
    }
    return rc;
}


OPAL_DECLSPEC int
opal_common_ucx_wpmem_flush(opal_common_ucx_wpmem_t *mem,
                          opal_common_ucx_flush_scope_t scope,
                          int target)
{
    _ctx_record_list_item_t *item;
    opal_common_ucx_ctx_t *ctx = mem->ctx;
    int rc = OPAL_SUCCESS;

    opal_mutex_lock(&ctx->mutex);

    OPAL_LIST_FOREACH(item, &ctx->tls_workers, _ctx_record_list_item_t) {
        if ((scope == OPAL_COMMON_UCX_SCOPE_EP) &&
                (NULL == item->ptr->endpoints[target])) {
            continue;
        }
        opal_mutex_lock(&item->ptr->mutex);
        rc = opal_common_ucx_winfo_flush(item->ptr, target, OPAL_COMMON_UCX_FLUSH_B,
                                         scope, NULL);
        switch (scope) {
        case OPAL_COMMON_UCX_SCOPE_WORKER:
            item->ptr->global_inflight_ops = 0;
            memset(item->ptr->inflight_ops, 0, item->ptr->comm_size * sizeof(short));
            break;
        case OPAL_COMMON_UCX_SCOPE_EP:
            item->ptr->global_inflight_ops -= item->ptr->inflight_ops[target];
            item->ptr->inflight_ops[target] = 0;
            break;
        }
        opal_mutex_unlock(&item->ptr->mutex);

        if (rc != OPAL_SUCCESS) {
            MCA_COMMON_UCX_ERROR("opal_common_ucx_flush failed: %d",
                                 rc);
            rc = OPAL_ERROR;
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

OPAL_DECLSPEC void
opal_common_ucx_req_init(void *request) {
    opal_common_ucx_request_t *req = (opal_common_ucx_request_t *)request;
    req->ext_req = NULL;
    req->ext_cb = NULL;
    req->winfo = NULL;
}

OPAL_DECLSPEC void
opal_common_ucx_req_completion(void *request, ucs_status_t status) {
    opal_common_ucx_request_t *req = (opal_common_ucx_request_t *)request;
    if (req->ext_cb != NULL) {
        (*req->ext_cb)(req->ext_req);
    }
    ucp_request_release(req);
}
