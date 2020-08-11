#include "opal_config.h"

#include "common_ucx.h"
#include "common_ucx_wpool.h"
#include "common_ucx_wpool_int.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/base/mca_base_framework.h"
#include "opal/mca/pmix/pmix-internal.h"
#include "opal/memoryhooks/memory.h"
#include "opal/util/proc.h"

#include <ucm/api/ucm.h>

/*******************************************************************************
 *******************************************************************************
 *
 * Worker Pool (wpool) framework
 * Used to manage multi-threaded implementation of UCX for ompi/OSC & OSHMEM
 *
 *******************************************************************************
 ******************************************************************************/

OBJ_CLASS_INSTANCE(opal_common_ucx_winfo_t, opal_list_item_t, NULL, _winfo_destructor);
OBJ_CLASS_INSTANCE(_ctx_record_t, opal_list_item_t, NULL, NULL);
OBJ_CLASS_INSTANCE(_mem_record_t, opal_list_item_t, NULL, NULL);

// TODO: Remove once debug is completed
#ifdef OPAL_COMMON_UCX_WPOOL_DBG
__thread FILE *tls_pf = NULL;
__thread int initialized = 0;
#endif
   
static _ctx_record_t *
_tlocal_add_ctx_rec(opal_common_ucx_ctx_t *ctx, size_t comm_size);
static inline _ctx_record_t *
_tlocal_get_ctx_rec(opal_tsd_tracked_key_t tls_key);
static inline _mem_record_t *
_tlocal_get_mem_rec(opal_tsd_tracked_key_t tls_key);
static void _tlocal_ctx_rec_cleanup(_ctx_record_t *ctx_rec);
static void _tlocal_mem_rec_cleanup(_mem_record_t *mem_rec);
static void _ctx_rec_destructor(void *arg);
static void _mem_rec_destructor(void *arg);

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

    winfo = OBJ_NEW(opal_common_ucx_winfo_t);
    if (NULL == winfo) {
        MCA_COMMON_UCX_ERROR("Cannot allocate memory for worker info");
        goto release_worker;
    }

    OBJ_CONSTRUCT(&winfo->mutex, opal_recursive_mutex_t);
    winfo->worker = worker;
    OBJ_CONSTRUCT(&winfo->ep_iops, opal_hash_table_t);
    opal_hash_table_init(&winfo->ep_iops, 256);

    winfo->global_inflight_ops = 0;
    winfo->inflight_req = UCS_OK;

    return winfo;

release_worker:
    ucp_worker_destroy(worker);
exit:
    return winfo;
}

static void
_winfo_destructor(opal_common_ucx_winfo_t *winfo)
{
    if (winfo->inflight_req != UCS_OK) {
        opal_common_ucx_wait_request_mt(winfo->inflight_req,
                                        "opal_common_ucx_flush");
        winfo->inflight_req = UCS_OK;
    }

    assert(winfo->global_inflight_ops == 0);
    winfo_ep_iop_t *ep_iop;
    void *iterator = NULL;
    size_t proc_vpid;
    int ret = OPAL_ERROR;

    ret = opal_hash_table_get_first_key_uint64(&winfo->ep_iops, &proc_vpid,
                                               (void **)&ep_iop, &iterator);
    if (OPAL_SUCCESS == ret) {
        do {
            ucp_ep_destroy(ep_iop->endpoint);
            ep_iop->inflight_ops = 0;
            free(ep_iop);
            ret = opal_hash_table_get_next_key_uint64(&winfo->ep_iops, &proc_vpid,
                                                      (void **) &ep_iop,
                                                      &iterator, &iterator);
        } while(OPAL_SUCCESS == ret);
    }
    opal_hash_table_remove_all(&winfo->ep_iops);
    OBJ_DESTRUCT(&winfo->ep_iops);
    OBJ_DESTRUCT(&winfo->mutex);
    ucp_worker_destroy(winfo->worker);
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

OPAL_DECLSPEC opal_common_ucx_ctx_t *
opal_common_ucx_wpctx_allocate(void)
{
    opal_common_ucx_ctx_t *ptr = calloc(1, sizeof(opal_common_ucx_ctx_t));

    return ptr;
}

OPAL_DECLSPEC void
opal_common_ucx_wpool_free(opal_common_ucx_wpool_t *wpool)
{
    assert(wpool->refcnt == 0);
    free(wpool);
}

static int _wpool_list_put(opal_list_t *list, opal_common_ucx_winfo_t *winfo);

OPAL_DECLSPEC int
opal_common_ucx_wpool_update_addr(opal_common_ucx_wpool_t *wpool, size_t comm_size,
                                opal_common_ucx_exchange_func_t exchange_func,
                                opal_common_ucx_get_proc_vpid_func_t get_proc_vpid_func,
                                void *exchange_metadata)
{
    unsigned int i = 0, proc_vpid = 0;
    int ret = OPAL_SUCCESS;
    int *recv_worker_lens = NULL, *recv_worker_displs = NULL;
    char *recv_worker_addrs = NULL, *curr_addr = NULL;

    opal_mutex_lock(&wpool->mutex);

    ret = exchange_func(wpool->worker_addr.addr, wpool->worker_addr.len,
                        &recv_worker_addrs,
                        &recv_worker_displs,
                        &recv_worker_lens,
                        exchange_metadata);

    curr_addr = recv_worker_addrs;
    /* loop through ranks of current comm and update wpool world_comm addresses */
    for (i = 0; i < comm_size; i++) {
        proc_vpid = get_proc_vpid_func(exchange_metadata, i);

        if (NULL == wpool->addrs_tbl[proc_vpid].worker_addrs ) {
            wpool->addrs_tbl[proc_vpid].worker_addrs = calloc(recv_worker_lens[i], sizeof(char));
            memcpy(wpool->addrs_tbl[proc_vpid].worker_addrs, curr_addr, recv_worker_lens[i]);
            wpool->addrs_tbl[proc_vpid].worker_displs = recv_worker_displs[i];
            wpool->addrs_tbl[proc_vpid].worker_lens = recv_worker_lens[i];
        }
        curr_addr=(curr_addr+recv_worker_lens[i]);
    }

    opal_mutex_unlock(&wpool->mutex);

    free(recv_worker_addrs);
    free(recv_worker_displs);
    free(recv_worker_lens);
    return ret;
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

    OBJ_CONSTRUCT(&wpool->mutex, opal_mutex_t);

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
    wpool->dflt_winfo = winfo;
    OBJ_RETAIN(wpool->dflt_winfo);

    status = ucp_worker_get_address(wpool->dflt_worker,
                                    &wpool->worker_addr.addr, &wpool->worker_addr.len);
    if (status != UCS_OK) {
        MCA_COMMON_UCX_VERBOSE(1, "ucp_worker_get_address failed: %d", status);
        rc = OPAL_ERROR;
        goto err_get_addr;
    }

    rc = _wpool_list_put(&wpool->idle_workers, winfo);
    if (rc) {
        goto err_wpool_add;
    }

    wpool->addrs_tbl = calloc(proc_world_size, sizeof(*wpool->addrs_tbl));
    return rc;

err_wpool_add:
    free(wpool->worker_addr.addr);
err_get_addr:
    OBJ_RELEASE(winfo);
    OBJ_RELEASE(wpool->dflt_winfo);
    wpool->dflt_winfo = NULL;
 err_worker_create:
    OBJ_DESTRUCT(&wpool->idle_workers);
    OBJ_DESTRUCT(&wpool->active_workers);
    ucp_cleanup(wpool->ucp_ctx);
 err_ucp_init:
    return rc;
}

OPAL_DECLSPEC
void opal_common_ucx_wpool_finalize(opal_common_ucx_wpool_t *wpool)
{
    wpool->refcnt--;
    if (wpool->refcnt > 0) {
        return;
    }

    /* Release the address here. recv worker will be released
     * below along with other idle workers */
    ucp_worker_release_address(wpool->dflt_worker, wpool->worker_addr.addr);

    /* Go over the list, free idle list items */
    if (!opal_list_is_empty(&wpool->idle_workers)) {
        opal_common_ucx_winfo_t *winfo, *next;
        OPAL_LIST_FOREACH_SAFE(winfo, next, &wpool->idle_workers,
                               opal_common_ucx_winfo_t) {
            opal_list_remove_item(&wpool->idle_workers, &winfo->super);
            OBJ_RELEASE(winfo);
        }
    }
    OBJ_DESTRUCT(&wpool->idle_workers);

    /* Release active workers. They are no longer active actually
     * because opal_common_ucx_wpool_finalize is being called. */
    if (!opal_list_is_empty(&wpool->active_workers)) {
        opal_common_ucx_winfo_t *winfo, *next;
        OPAL_LIST_FOREACH_SAFE(winfo, next, &wpool->active_workers,
                               opal_common_ucx_winfo_t) {
            opal_list_remove_item(&wpool->active_workers, &winfo->super);
            OBJ_RELEASE(winfo);
        }
    }
    OBJ_DESTRUCT(&wpool->active_workers);

    OBJ_RELEASE(wpool->dflt_winfo);
    wpool->dflt_winfo = NULL;

    OBJ_DESTRUCT(&wpool->mutex);
    ucp_cleanup(wpool->ucp_ctx);
    return;
}

OPAL_DECLSPEC int
opal_common_ucx_wpool_progress(opal_common_ucx_wpool_t *wpool)
{
    opal_common_ucx_winfo_t *winfo = NULL, *next = NULL;
    int completed = 0, progressed = 0;

    /* Go over all active workers and progress them
     * TODO: may want to have some partitioning to progress only part of
     * workers */
    if (0 != opal_mutex_trylock(&wpool->mutex)) {
        return completed;
    }

    bool progress_dflt_worker = true;
    OPAL_LIST_FOREACH_SAFE(winfo, next, &wpool->active_workers,
                           opal_common_ucx_winfo_t) {
        if (0 != opal_mutex_trylock(&winfo->mutex)) {
            continue;
        }
        do {
            if (winfo == wpool->dflt_winfo) {
                progress_dflt_worker = false;
            }
            progressed = ucp_worker_progress(winfo->worker);
            completed += progressed;
        } while (progressed);
        opal_mutex_unlock(&winfo->mutex);
    }
    opal_mutex_unlock(&wpool->mutex);

    if (progress_dflt_worker) {
        /* make sure to progress at least some */
        opal_mutex_lock(&wpool->dflt_winfo->mutex);
        completed += ucp_worker_progress(wpool->dflt_winfo->worker);
        opal_mutex_unlock(&wpool->dflt_winfo->mutex);
    }
    return completed;
}

static int
_wpool_list_put(opal_list_t *list, opal_common_ucx_winfo_t *winfo)
{
    opal_list_append(list, &winfo->super);
    return OPAL_SUCCESS;
}

static opal_common_ucx_winfo_t*
_wpool_list_get(opal_common_ucx_wpool_t *wpool, opal_list_t *list)
{
    opal_common_ucx_winfo_t *winfo = NULL;

    if (!opal_list_is_empty(list)) {
        winfo = (opal_common_ucx_winfo_t *)opal_list_get_first(list);
        opal_list_remove_item(list, &winfo->super);
    }

    return winfo;
}

static opal_common_ucx_winfo_t *
_wpool_get_winfo(opal_common_ucx_wpool_t *wpool) 
{
    opal_common_ucx_winfo_t *winfo;
    opal_mutex_lock(&wpool->mutex);
    winfo = _wpool_list_get(wpool, &wpool->idle_workers);
    if (!winfo) {
        winfo = _winfo_create(wpool);
        if (!winfo) {
            MCA_COMMON_UCX_ERROR("Failed to allocate worker info structure");
            opal_mutex_unlock(&wpool->mutex);
            return NULL;
        }
    }

    /* Put the worker on the active list */
    _wpool_list_put(&wpool->active_workers, winfo);

    opal_mutex_unlock(&wpool->mutex);

    return winfo;
}

static void
_wpool_put_winfo(opal_common_ucx_wpool_t *wpool, opal_common_ucx_winfo_t *winfo)
{
    opal_mutex_lock(&wpool->mutex);
    opal_list_remove_item(&wpool->active_workers, &winfo->super);
    opal_list_prepend(&wpool->idle_workers, &winfo->super);
    opal_mutex_unlock(&wpool->mutex);
    
    return;
}

/* -----------------------------------------------------------------------------
 * Worker Pool Communication context management functionality
 *----------------------------------------------------------------------------*/

OPAL_DECLSPEC int
opal_common_ucx_wpctx_init(opal_common_ucx_wpool_t *wpool,
                           opal_common_ucx_ctx_t *ctx,
                           opal_common_ucx_get_proc_vpid_func_t get_proc_vpid_func)
{
    int ret = OPAL_SUCCESS;

    if (NULL == ctx) {
        ret = OPAL_ERROR;
        goto error;
    }

    OBJ_CONSTRUCT(&ctx->mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&ctx->ctx_records, opal_list_t);

    ctx->wpool = wpool;
    ctx->get_proc_vpid_func = get_proc_vpid_func;

    OBJ_CONSTRUCT(&ctx->tls_key, opal_tsd_tracked_key_t);
    opal_tsd_tracked_key_set_destructor(&ctx->tls_key, _ctx_rec_destructor);

    return ret;

error:
    OBJ_DESTRUCT(&ctx->mutex);
    OBJ_DESTRUCT(&ctx->ctx_records);
    return ret;
}

OPAL_DECLSPEC void
opal_common_ucx_wpctx_release(opal_common_ucx_ctx_t *ctx)
{
    _ctx_record_t *ctx_rec = NULL, *next;

    /* Application is expected to guarantee that no operation
     * is performed on the context that is being released */

    /* destroy key so that other threads don't invoke destructors */
    OBJ_DESTRUCT(&ctx->tls_key);

    /* loop through list of records */
    OPAL_LIST_FOREACH_SAFE(ctx_rec, next, &ctx->ctx_records, _ctx_record_t) {
       _tlocal_ctx_rec_cleanup(ctx_rec);
    }

    OBJ_DESTRUCT(&ctx->mutex);
    OBJ_DESTRUCT(&ctx->ctx_records);
}

static inline int opal_common_wpool_memmap(opal_common_ucx_ctx_t *ctx, 
                                          void **mem_base, size_t mem_size,
                                          opal_common_ucx_mem_type_t mem_type,
                                          opal_common_ucx_wpmem_t *wpmem)
{
    return _comm_ucx_wpmem_map(ctx->wpool, mem_base, mem_size, &wpmem->memh, mem_type);
}

static int opal_common_wpool_rkey_pack(opal_common_ucx_ctx_t *ctx,
                                             opal_common_ucx_wpmem_t *wpmem,
                                             void **rkey_addr, size_t *rkey_addr_len)
{
    ucs_status_t status = ucp_rkey_pack(ctx->wpool->ucp_ctx, wpmem->memh, rkey_addr, rkey_addr_len);
    if (status != UCS_OK) {
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

/* -----------------------------------------------------------------------------
 * Worker Pool Memory management functionality
 *----------------------------------------------------------------------------*/

OPAL_DECLSPEC
int opal_common_ucx_wpmem_create(opal_common_ucx_ctx_t *ctx,
                               void **mem_base, size_t mem_size,
                               opal_common_ucx_mem_type_t mem_type,
                               opal_common_ucx_exchange_func_t exchange_func,
                               void *exchange_metadata, size_t comm_size,
                               char **my_mem_addr,
                               int *my_mem_addr_size,
                               opal_common_ucx_wpmem_t **mem_ptr)
{
    opal_common_ucx_wpmem_t *mem = calloc(1, sizeof(*mem));
    void *rkey_addr = NULL;
    size_t rkey_addr_len;
    int ret = OPAL_SUCCESS;

    mem->comm_size = comm_size;
    mem->metadata = exchange_metadata;
    mem->ctx = ctx;
    mem->mem_addrs = NULL;
    mem->mem_displs = NULL;
    
    OBJ_CONSTRUCT(&mem->mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&mem->mem_records, opal_list_t);

    ret = opal_common_wpool_memmap(ctx, mem_base, mem_size, mem_type, mem);
    if (OPAL_SUCCESS != ret) {
        MCA_COMMON_UCX_VERBOSE(1, "opal_common_wpool_memmap failed: %d", ret);
        goto error_mem_map;
    }

    ret = opal_common_wpool_rkey_pack(ctx, mem, &rkey_addr, &rkey_addr_len);
    if (OPAL_SUCCESS != ret) {
        MCA_COMMON_UCX_VERBOSE(1, "ucp_rkey_pack failed: %d", ret);
        goto error_rkey_pack;
    }

    ret = exchange_func(rkey_addr, rkey_addr_len,
                        &mem->mem_addrs, &mem->mem_displs, NULL, exchange_metadata);
    if (OPAL_SUCCESS != ret) {
        goto error_rkey_pack;
    }

    OBJ_CONSTRUCT(&mem->tls_key, opal_tsd_tracked_key_t);
    opal_tsd_tracked_key_set_destructor(&mem->tls_key, _mem_rec_destructor);

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

void opal_common_ucx_wpmem_free(opal_common_ucx_wpmem_t *mem)
{
    _mem_record_t *mem_rec = NULL, *next;

    if (NULL == mem) {
        return;
    }
    
    OBJ_DESTRUCT(&mem->tls_key);

    /* Loop through list of records */
    OPAL_LIST_FOREACH_SAFE(mem_rec, next, &mem->mem_records, _mem_record_t) {
        _tlocal_mem_rec_cleanup(mem_rec);
    }
  
    OBJ_DESTRUCT(&mem->mem_records);

    free(mem->mem_addrs);
    free(mem->mem_displs);

    ucp_mem_unmap(mem->ctx->wpool->ucp_ctx, mem->memh);
    free(mem);
}

static inline _ctx_record_t *
_tlocal_get_ctx_rec(opal_tsd_tracked_key_t tls_key) {
    _ctx_record_t *ctx_rec = NULL;
    int rc = opal_tsd_tracked_key_get(&tls_key, (void**)&ctx_rec);

    if (OPAL_SUCCESS != rc) {
        return NULL;
    }

    return ctx_rec;
}

static void _ctx_rec_destructor(void *arg) {
    _tlocal_ctx_rec_cleanup ((_ctx_record_t *) arg);
    return;
}

/* Thread local storage destructor, also called from wpool_ctx release */
static void
_tlocal_ctx_rec_cleanup(_ctx_record_t *ctx_rec)
{
    if (NULL == ctx_rec) {
        return;
    }

    opal_common_ucx_winfo_t *winfo = ctx_rec->winfo;
    opal_common_ucx_wpool_t *wpool = ctx_rec->gctx->wpool;

    /* Remove worker from active and return to idle list. */
    _wpool_put_winfo(wpool, winfo);

    /* Remove the context record from the ctx list. */
    opal_mutex_lock(&ctx_rec->gctx->mutex);
    opal_list_remove_item(&ctx_rec->gctx->ctx_records, &ctx_rec->super);
    opal_mutex_unlock(&ctx_rec->gctx->mutex);

    OBJ_RELEASE(ctx_rec);

    return;
}

static _ctx_record_t *
_tlocal_add_ctx_rec(opal_common_ucx_ctx_t *ctx, size_t comm_size)
{
    int rc;
    
    _ctx_record_t *ctx_rec = OBJ_NEW(_ctx_record_t);
    if (!ctx_rec) {
        MCA_COMMON_UCX_ERROR("Failed to allocate new ctx_rec");
        goto error1;
    }

    ctx_rec->gctx = ctx;
    ctx_rec->proc_vpid = calloc(comm_size, sizeof(*ctx_rec->proc_vpid));
    ctx_rec->winfo = _wpool_get_winfo(ctx->wpool);
    if (NULL == ctx_rec->winfo) {
        MCA_COMMON_UCX_ERROR("Failed to allocate new worker");
        goto error2;
    }

    /* Add ctx_rec to list */
    opal_mutex_lock(&ctx->mutex);
    opal_list_append(&ctx->ctx_records, &ctx_rec->super);
    opal_mutex_unlock(&ctx->mutex);

    /* Add tls reference to record */
    rc = opal_tsd_tracked_key_set(&ctx->tls_key, ctx_rec);
    if (OPAL_SUCCESS != rc) {
        MCA_COMMON_UCX_ERROR("Failed to set ctx_rec tls key");
        goto error3; 
    }

    /* All good - return the record */
    return ctx_rec;

error3:
    opal_mutex_lock(&ctx->mutex);
    opal_list_remove_item(&ctx->ctx_records, &ctx_rec->super);
    opal_mutex_unlock(&ctx->mutex);
    _wpool_put_winfo(ctx->wpool, ctx_rec->winfo);
error2:
    OBJ_RELEASE(ctx_rec);
error1:
    return NULL;
}

static int _tlocal_ctx_connect(opal_common_ucx_wpmem_t *wpmem, _ctx_record_t *ctx_rec,
                               unsigned int proc_vpid, winfo_ep_iop_t **ep_iop)
{
    ucp_ep_params_t ep_params;
    opal_common_ucx_winfo_t *winfo = ctx_rec->winfo;
    opal_common_ucx_ctx_t   *gctx  = ctx_rec->gctx;
    opal_common_ucx_wpool_t *wpool = gctx->wpool;
    ucs_status_t status;

    *ep_iop = calloc(1, sizeof(*ep_iop));

    memset(&ep_params, 0, sizeof(ucp_ep_params_t));
    ep_params.field_mask = UCP_EP_PARAM_FIELD_REMOTE_ADDRESS;

    opal_mutex_lock(&winfo->mutex);
    ep_params.address = (ucp_address_t *)wpool->addrs_tbl[proc_vpid].worker_addrs;
    status = ucp_ep_create(winfo->worker, &ep_params, &(*ep_iop)->endpoint);
    if (status != UCS_OK) {
        MCA_COMMON_UCX_VERBOSE(1, "ucp_ep_create failed: %d", status);
        opal_mutex_unlock(&winfo->mutex);
        return OPAL_ERROR;
    }
    opal_hash_table_set_value_uint64(&winfo->ep_iops, proc_vpid, (void *) *ep_iop);
    opal_mutex_unlock(&winfo->mutex);
    return OPAL_SUCCESS;
}

static void
opal_common_wpool_rkey_destroy(_mem_record_t *mem_rec)
{
    size_t i;

    opal_mutex_lock(&mem_rec->wpmem->ctx->wpool->mutex);
    for(i = 0; i < mem_rec->wpmem->comm_size; i++) {
        if (mem_rec->rkeys[i]) {
            ucp_rkey_destroy(mem_rec->rkeys[i]);
        }
    }
    opal_mutex_unlock(&mem_rec->wpmem->ctx->wpool->mutex);
    return;
} 

static inline _mem_record_t *
_tlocal_get_mem_rec(opal_tsd_tracked_key_t tls_key) {
    _mem_record_t *mem_rec = NULL;
    int rc = opal_tsd_tracked_key_get(&tls_key, (void**)&mem_rec);

    if (OPAL_SUCCESS != rc) {
        return NULL;
    }

    return mem_rec;
}

static void
_mem_rec_destructor(void * arg) {
    _tlocal_mem_rec_cleanup ((_mem_record_t *) arg);
    return;
}

static void
_tlocal_mem_rec_cleanup(_mem_record_t *mem_rec)
{
    if (NULL == mem_rec) {
        return;
    }

    opal_common_wpool_rkey_destroy(mem_rec);
    free(mem_rec->rkeys);

    /* Remove item from the list */
    opal_mutex_lock(&mem_rec->wpmem->mutex);
    opal_list_remove_item(&mem_rec->wpmem->mem_records, &mem_rec->super);
    opal_mutex_unlock(&mem_rec->wpmem->mutex);

    OBJ_RELEASE(mem_rec);

    return;
}

static _mem_record_t *_tlocal_add_mem_rec(opal_common_ucx_wpmem_t *mem, _ctx_record_t *ctx_rec)
{
    int rc = OPAL_SUCCESS;

    _mem_record_t *mem_rec = _tlocal_get_mem_rec(mem->tls_key);
    if (OPAL_UNLIKELY(!mem_rec)) {
        mem_rec = OBJ_NEW(_mem_record_t);
        if (NULL == mem_rec) {
            return NULL;
        }

        mem_rec->wpmem = mem;
        mem_rec->ctx_rec = ctx_rec;
        mem_rec->winfo = ctx_rec->winfo;
        mem_rec->endpoints = calloc(mem->comm_size, sizeof(*mem_rec->endpoints));
        mem_rec->rkeys = calloc(mem->comm_size, sizeof(*mem_rec->rkeys));

        rc = opal_tsd_tracked_key_set(&mem->tls_key, mem_rec);
        if (OPAL_SUCCESS != rc) {
            return NULL;
        }

        opal_mutex_lock(&mem->mutex);
        opal_list_append(&mem->mem_records, &mem_rec->super);
        opal_mutex_unlock(&mem->mutex);
    }

    return mem_rec;
}

static int
_tlocal_mem_create_rkey(_mem_record_t *mem_rec, ucp_ep_h ep, int target)
{
    opal_common_ucx_wpmem_t *wpmem = mem_rec->wpmem;
    int displ = wpmem->mem_displs[target];
    ucs_status_t status;

    opal_mutex_lock(&mem_rec->winfo->mutex);
    status = ucp_ep_rkey_unpack(ep, &wpmem->mem_addrs[displ],
                                &mem_rec->rkeys[target]);
    opal_mutex_unlock(&mem_rec->winfo->mutex);
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
    _ctx_record_t *ctx_rec = NULL;
    _mem_record_t *mem_rec = NULL;
    opal_common_ucx_winfo_t *winfo = NULL;
    ucp_ep_h ep = NULL;
    winfo_ep_iop_t *ep_iop = NULL;
    int rc = OPAL_SUCCESS, proc_vpid;

    ctx_rec = _tlocal_get_ctx_rec(mem->ctx->tls_key);
    if (OPAL_UNLIKELY(!ctx_rec)) {
        ctx_rec = _tlocal_add_ctx_rec(mem->ctx, mem->comm_size);
        if (NULL == ctx_rec) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
    }
    winfo = ctx_rec->winfo;
    
    proc_vpid = ctx_rec->gctx->get_proc_vpid_func(mem->metadata, target);
    ctx_rec->proc_vpid[target] = proc_vpid;
    
    /* Obtain the endpoint */
    rc = opal_hash_table_get_value_uint64(&winfo->ep_iops, proc_vpid, (void **)&ep_iop);
    if (OPAL_UNLIKELY(OPAL_ERR_NOT_FOUND == rc)) {
        rc = _tlocal_ctx_connect(mem, ctx_rec, proc_vpid, &ep_iop);
        if (OPAL_SUCCESS != rc) {
            return rc;
        }
    }
   
    ep = ep_iop->endpoint;

    /* Obtain the memory region info */
    mem_rec = _tlocal_add_mem_rec(mem, ctx_rec);
    if (NULL == mem_rec) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* Obtain the rkey */
    if (OPAL_UNLIKELY(NULL == mem_rec->rkeys[target])) {
        /* Create the rkey */
        rc = _tlocal_mem_create_rkey(mem_rec, ep, target);
        if (OPAL_SUCCESS != rc) {
            return rc;
        }
    }

    mem_rec->endpoints[target] = ep;
    return OPAL_SUCCESS;
}

OPAL_DECLSPEC int
opal_common_ucx_winfo_flush(opal_common_ucx_winfo_t *winfo, ucp_ep_h ep,
                            opal_common_ucx_flush_type_t type,
                            opal_common_ucx_flush_scope_t scope,
                            ucs_status_ptr_t *req_ptr)
{
    ucs_status_ptr_t req;
    ucs_status_t status = UCS_OK;
    int rc = OPAL_SUCCESS;

#if HAVE_DECL_UCP_EP_FLUSH_NB
    if (scope == OPAL_COMMON_UCX_SCOPE_EP) {
        req = ucp_ep_flush_nb(ep, 0, opal_common_ucx_empty_complete_cb);
    } else {
        req = ucp_worker_flush_nb(winfo->worker, 0, opal_common_ucx_empty_complete_cb);
    }
    if (UCS_PTR_IS_PTR(req)) {
        ((opal_common_ucx_request_t *)req)->winfo = winfo;
    }

    if(type == OPAL_COMMON_UCX_FLUSH_B) {
        rc = opal_common_ucx_wait_request_mt(req, "ucp_ep_flush_nb");
    } else {
        *req_ptr = req;
    }
    return rc;
#endif
    
    switch (type) {
    case OPAL_COMMON_UCX_FLUSH_NB_PREFERRED:
    case OPAL_COMMON_UCX_FLUSH_B:
        if (scope == OPAL_COMMON_UCX_SCOPE_EP) {
            status = ucp_ep_flush(ep);
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
    _ctx_record_t *ctx_rec;
    opal_common_ucx_ctx_t *ctx = mem->ctx;
    winfo_ep_iop_t *ep_iop;
    void *iterator = NULL;
    size_t proc_vpid;
    int rc = OPAL_SUCCESS, ret = OPAL_SUCCESS;

    if (NULL == mem) {
        return OPAL_SUCCESS;
    }

    ctx = mem->ctx;
    opal_mutex_lock(&ctx->mutex);
    OPAL_LIST_FOREACH(ctx_rec, &ctx->ctx_records, _ctx_record_t) {
        opal_common_ucx_winfo_t *winfo = ctx_rec->winfo;
        proc_vpid = ctx_rec->proc_vpid[target];
        opal_mutex_lock(&winfo->mutex);
        ret = opal_hash_table_get_value_uint64(&winfo->ep_iops, proc_vpid, (void **) &ep_iop);
        if (OPAL_SUCCESS != ret) {
            continue;
        }
        rc = opal_common_ucx_winfo_flush(winfo, ep_iop->endpoint, OPAL_COMMON_UCX_FLUSH_B,
                                         scope, NULL);
        switch (scope) {
        case OPAL_COMMON_UCX_SCOPE_WORKER:
            winfo->global_inflight_ops = 0;
            ret = opal_hash_table_get_first_key_uint64(&winfo->ep_iops, &proc_vpid,
                                               (void **) &ep_iop, &iterator);
            if (OPAL_SUCCESS == ret) {
                do {
                    ep_iop->inflight_ops = 0;
                    ret = opal_hash_table_get_next_key_uint64(&winfo->ep_iops, &proc_vpid,
                                                             (void **) &ep_iop,
                                                              &iterator, &iterator);
                } while(OPAL_SUCCESS == ret);
            }
            break;
        case OPAL_COMMON_UCX_SCOPE_EP:
            winfo->global_inflight_ops -= ep_iop->inflight_ops;
            ep_iop->inflight_ops = 0;
            break;
        }
        opal_mutex_unlock(&winfo->mutex);

        if (rc != OPAL_SUCCESS) {
            MCA_COMMON_UCX_ERROR("opal_common_ucx_flush failed: %d", rc);
            rc = OPAL_ERROR;
        }
    }
    opal_mutex_unlock(&ctx->mutex);

    return rc;
}


OPAL_DECLSPEC int
opal_common_ucx_wpmem_fence(opal_common_ucx_wpmem_t *mem)
{
    ucp_worker_h worker = NULL;
    int rc = OPAL_SUCCESS;

    _mem_record_t *mem_rec = _tlocal_get_mem_rec(mem->tls_key);
    if (NULL == mem_rec) {
        worker = mem->ctx->wpool->dflt_worker;
    }
    else {
        worker = mem_rec->winfo->worker;
    }

    ucs_status_t status = ucp_worker_fence(worker);
    if (UCS_OK != status) {
        rc = OPAL_ERROR;
    }
    return rc;
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
