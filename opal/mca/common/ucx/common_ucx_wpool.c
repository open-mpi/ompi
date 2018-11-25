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
#ifdef FDBG
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

    DBG_OUT("_winfo_create: worker = %p\n", (void *)worker);
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
}

static void
_winfo_release(opal_common_ucx_winfo_t *winfo)
{
    DBG_OUT("_release_ctx_worker: winfo = %p, worker = %p\n",
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

    DBG_OUT("opal_common_ucx_wpool_allocate: wpool = %p\n", (void *)ptr);
    return ptr;
}

OPAL_DECLSPEC void
opal_common_ucx_wpool_free(opal_common_ucx_wpool_t *wpool)
{
    assert(wpool->refcnt == 0);

    DBG_OUT("opal_common_ucx_wpool_free: wpool = %p\n", (void *)wpool);

    free(wpool);
}

OPAL_DECLSPEC int
opal_common_ucx_wpool_init(opal_common_ucx_wpool_t *wpool,
                               int proc_world_size,
                               ucp_request_init_callback_t req_init_ptr,
                               size_t req_size, bool enable_mt)
{
    int rc = OPAL_SUCCESS;
    return rc;
}

OPAL_DECLSPEC
void opal_common_ucx_wpool_finalize(opal_common_ucx_wpool_t *wpool)
{
    _tlocal_table_t *tls_item = NULL, *tls_next;

    DBG_OUT("opal_common_ucx_wpool_finalize(start): wpool = %p\n",
            (void *)wpool);

    wpool->refcnt--;
    if (wpool->refcnt > 0) {
        DBG_OUT("opal_common_ucx_wpool_finalize: wpool = %p\n", (void *)wpool);
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
        DBG_OUT("opal_common_ucx_wpool_finalize: cleanup wpool = %p\n",
                (void *)wpool);
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
            // _winfo_reset(item->ptr); - should be already reset
            _winfo_release(item->ptr);
            OBJ_RELEASE(item);
        }
    }
    OBJ_DESTRUCT(&wpool->idle_workers);

    /* Release active workers. They are no longer active actually
     * because opal_common_ucx_wpool_finalize is being called. */
    if (!opal_list_is_empty(&wpool->active_workers)) {
        _winfo_list_item_t *item, *next;
        OPAL_LIST_FOREACH_SAFE(item, next, &wpool->active_workers, _winfo_list_item_t) {
            opal_list_remove_item(&wpool->active_workers, &item->super);
            _winfo_reset(item->ptr);
            _winfo_release(item->ptr);
            OBJ_RELEASE(item);
        }
    }
    OBJ_DESTRUCT(&wpool->active_workers);

    //OBJ_DESTRUCT(&wpool->mutex);
    ucp_cleanup(wpool->ucp_ctx);
    DBG_OUT("opal_common_ucx_wpool_finalize: wpool = %p\n", (void *)wpool);
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

    item = OBJ_NEW(_winfo_list_item_t);
    if (NULL == item) {
        MCA_COMMON_UCX_ERROR("Cannot allocate memory for winfo list item");
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    item->ptr = winfo;

    opal_mutex_lock(&wpool->mutex);
    opal_list_append(list, &item->super);
    opal_mutex_unlock(&wpool->mutex);

    DBG_OUT("_wpool_list_put: wpool = %p winfo = %p\n",
            (void *)wpool, (void *)winfo);
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

    DBG_OUT("_wpool_remove_from_idle: wpool = %p\n", (void *)wpool);
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

    DBG_OUT("_wpool_get_idle: wpool = %p winfo = %p\n",
            (void *)wpool, (void *)winfo);
    winfo->endpoints = calloc(comm_size, sizeof(ucp_ep_h));
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
    pthread_rwlockattr_t attr;
    int ret = OPAL_SUCCESS;

    ctx->ctx_id = OPAL_ATOMIC_ADD_FETCH32(&wpool->cur_ctxid, 1);
    DBG_OUT("ctx_create: ctx_id = %d\n", (int)ctx->ctx_id);

    //OBJ_CONSTRUCT(&ctx->mutex, opal_mutex_t);
    // TODO: check return codes
    pthread_rwlockattr_init(&attr);
    pthread_rwlock_init(&ctx->rwlock, &attr);
    pthread_rwlockattr_destroy(&attr);

    OBJ_CONSTRUCT(&ctx->tls_workers, opal_list_t);
    ctx->released = 0;
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
    DBG_OUT("opal_common_ucx_ctx_create: wpool = %p, (*ctx_ptr) = %p\n",
            (void *)wpool, (void *)(*ctx_ptr));
    return ret;
error:
    //OBJ_DESTRUCT(&ctx->mutex);
    OBJ_DESTRUCT(&ctx->tls_workers);
    free(ctx);
    (*ctx_ptr) = NULL;
    return ret;
}

OPAL_DECLSPEC void
opal_common_ucx_wpctx_release(opal_common_ucx_ctx_t *ctx)
{
    _ctx_record_list_item_t *item = NULL;
    int can_free = 0;

    DBG_OUT("opal_common_ucx_ctx_release: ctx = %p\n", (void *)ctx);

    /* Mark all the contexts and workers as being released
     * Threads involved in this context will reconize this
     * through the "released" flag and will perform cleanup
     * in a deferred manner.
     * If Worker pool will be destroyed earlier - wpool filalize
     * will take care of this.
     */

    //opal_mutex_lock(&ctx->mutex);
    pthread_rwlock_rdlock(&ctx->rwlock);

    /* Mark that this function has been called */
    ctx->released = 1;
    /* Go over all TLS subscribed to this context and mark
     * that this handler is no longer in use */
    OPAL_LIST_FOREACH(item, &ctx->tls_workers, _ctx_record_list_item_t) {
        item->ptr->released = 1;
    }

    if (0 == opal_list_get_size(&ctx->tls_workers)) {
        can_free = 1;
    }

    //opal_mutex_unlock(&ctx->mutex);
    pthread_rwlock_unlock(&ctx->rwlock);


    if (can_free) {
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
    //OBJ_DESTRUCT(&ctx->mutex);
    OBJ_DESTRUCT(&ctx->tls_workers);
    DBG_OUT("_common_ucx_ctx_free: ctx = %p\n", (void *)ctx);
    free(ctx);
}

/* Subscribe a new TLS to this context */
static int
_common_ucx_wpctx_append(opal_common_ucx_ctx_t *ctx, _tlocal_ctx_t *ctx_rec)
{
    _ctx_record_list_item_t *item = OBJ_NEW(_ctx_record_list_item_t);
    if (NULL == item) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    item->ptr = ctx_rec;

    //opal_mutex_lock(&ctx->mutex);
    pthread_rwlock_wrlock(&ctx->rwlock);
    opal_list_append(&ctx->tls_workers, &item->super);
    //opal_mutex_unlock(&ctx->mutex);
    pthread_rwlock_unlock(&ctx->rwlock);

    DBG_OUT("_common_ucx_ctx_append: ctx = %p, ctx_rec = %p\n",
            (void *)ctx, (void *)ctx_rec);
    return OPAL_SUCCESS;
}

/* Unsubscribe a particular TLS to this context */
static void
_common_ucx_wpctx_remove(opal_common_ucx_ctx_t *ctx, _tlocal_ctx_t *ctx_rec)
{
    int can_free = 0;
    _ctx_record_list_item_t *item = NULL, *next;

    // opal_mutex_lock(&ctx->mutex);
    pthread_rwlock_wrlock(&ctx->rwlock);

    OPAL_LIST_FOREACH_SAFE(item, next, &ctx->tls_workers,
                           _ctx_record_list_item_t) {
        if (ctx_rec == item->ptr) {
            opal_list_remove_item(&ctx->tls_workers, &item->super);
            OBJ_RELEASE(item);
            break;
        }
    }
    if (0 == opal_list_get_size(&ctx->tls_workers) && ctx->released) {
        can_free = 1;
    }
    //opal_mutex_unlock(&ctx->mutex);
    pthread_rwlock_unlock(&ctx->rwlock);

    if (can_free) {
        /* All references to this data structure are removed
         * we can safely release communication context structure */
        _common_ucx_wpctx_free(ctx);
    }
    DBG_OUT("_common_ucx_ctx_remove: ctx = %p, ctx_rec = %p\n",
            (void *)ctx, (void *)ctx_rec);
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
    int ret = OPAL_SUCCESS;

    mem->mem_id = OPAL_ATOMIC_ADD_FETCH32(&ctx->wpool->cur_memid, 1);

    DBG_OUT("mem_create: mem_id = %d\n", (int)mem->mem_id);

    OBJ_CONSTRUCT(&mem->mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&mem->registrations, opal_list_t);
    mem->released = 0;
    mem->ctx = ctx;
    mem->mem_addrs = NULL;
    mem->mem_displs = NULL;

    ret = _comm_ucx_wpmem_map(ctx->wpool, mem_base, mem_size, &mem->memh,
                            mem_type);

    /* Dont need the destructor here, will use
     * wpool-level destructor */
    pthread_key_create(&mem->mem_tls_key, NULL);

    (*mem_ptr) = mem;

    DBG_OUT("opal_common_ucx_mem_create(end): mem = %p\n", (void *)mem);
    return ret;
}

OPAL_DECLSPEC int
opal_common_ucx_wpmem_free(opal_common_ucx_wpmem_t *mem)
{
    _mem_record_list_item_t *item = NULL;
    int can_free = 0;

    opal_mutex_lock(&mem->mutex);
    /* Mark that this function has been called */
    mem->released = 1;
    /* Go over all TLS subscribed to this context and mark
     * that this handler is no longer in use */
    OPAL_LIST_FOREACH(item, &mem->registrations, _mem_record_list_item_t) {
        item->ptr->released = 1;
    }
    if(0 == opal_list_get_size(&mem->registrations)){
        can_free = 1;
    }
    opal_mutex_unlock(&mem->mutex);
    if (can_free) {
        _common_ucx_wpmem_free(mem);
    }
    return OPAL_SUCCESS;
}


static int _comm_ucx_wpmem_map(opal_common_ucx_wpool_t *wpool,
                             void **base, size_t size, ucp_mem_h *memh_ptr,
                             opal_common_ucx_mem_type_t mem_type)
{
    int ret = OPAL_SUCCESS;
    return ret;
}

static void _common_ucx_wpmem_free(opal_common_ucx_wpmem_t *mem)
{
    pthread_key_delete(mem->mem_tls_key);
    free(mem->mem_addrs);
    free(mem->mem_displs);
    ucp_mem_unmap(mem->ctx->wpool->ucp_ctx, mem->memh);
    OBJ_DESTRUCT(&mem->mutex);
    OBJ_DESTRUCT(&mem->registrations);
    DBG_OUT("_common_ucx_mem_free: mem = %p\n", (void *)mem);
    free(mem);
}

static int
_common_ucx_wpmem_append(opal_common_ucx_wpmem_t *mem,
                       _tlocal_mem_t *mem_rec)
{
    _mem_record_list_item_t *item = OBJ_NEW(_mem_record_list_item_t);
    if (NULL == item) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    item->ptr = mem_rec;
    opal_mutex_lock(&mem->mutex);
    opal_list_append(&mem->registrations, &item->super);
    opal_mutex_unlock(&mem->mutex);
    DBG_OUT("_common_ucx_mem_append: mem = %p, mem_rec = %p\n", (void *)mem, (void *)mem_rec);
    return OPAL_SUCCESS;
}

static void
_common_ucx_mem_remove(opal_common_ucx_wpmem_t *mem, _tlocal_mem_t *mem_rec)
{
    int can_free = 0;
    _mem_record_list_item_t *item = NULL, *next;

    opal_mutex_lock(&mem->mutex);
    OPAL_LIST_FOREACH_SAFE(item, next, &mem->registrations,
                           _mem_record_list_item_t) {
        if (mem_rec == item->ptr) {
            opal_list_remove_item(&mem->registrations, &item->super);
            OBJ_RELEASE(item);
            break;
        }
    }
    if (0 == opal_list_get_size(&mem->registrations)) {
        can_free = 1;
    }
    opal_mutex_unlock(&mem->mutex);

    if (can_free) {
        /* All references to this data structure are removed
         * we can safely release communication context structure */
        _common_ucx_wpmem_free(mem);
    }
    DBG_OUT("_common_ucx_mem_remove(end): mem = %p mem_rec = %p\n",
            (void *)mem, (void *)mem_rec);
    return;
}

/* TLS management functions */

// TODO: don't want to inline this function
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
        DBG_OUT("_tlocal_tls_ctxtbl_extend failed\n");
        // TODO: handle error
    }
    if(_tlocal_tls_memtbl_extend(tls, 4)) {
        DBG_OUT("_tlocal_tls_memtbl_extend failed\n");
        // TODO: handle error
    }

    pthread_setspecific(wpool->tls_key, tls);
    DBG_OUT("_common_ucx_tls_init(end): wpool = %p\n", (void *)wpool);
    return tls;
}

static inline _tlocal_table_t *
_tlocal_get_tls(opal_common_ucx_wpool_t *wpool){
    _tlocal_table_t *tls = pthread_getspecific(wpool->tls_key);
    if( OPAL_UNLIKELY(NULL == tls) ) {
        tls = _common_ucx_tls_init(wpool);
    }
    DBG_OUT("_tlocal_get_tls(end): wpool = %p tls = %p\n",
            (void *)wpool, (void *)tls);
    return tls;
}

/*
static void _tlocal_cleanup(void *arg)
{
    _tlocal_table_t *item = NULL, *next;
    _tlocal_table_t *tls = (_tlocal_table_t *)arg;
    opal_common_ucx_wpool_t *wpool = NULL;

    DBG_OUT("_cleanup_tlocal: start\n");

    if (NULL == tls) {
        return;
    }

    wpool = tls->wpool;
    *//* 1. Remove us from tls_list *//*
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
*/

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
    DBG_OUT("_common_ucx_tls_cleanup(end): tls = %p\n", (void *)tls);

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
    DBG_OUT("_tlocal_tls_ctxtbl_extend(end): tbl = %p\n", (void *)tbl);
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
    DBG_OUT("_tlocal_tls_memtbl_extend(end): tbl = %p\n", (void *)tbl);
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
    DBG_OUT("_tlocal_ctx_search: tls = %p ctx_id = %d\n", (void *)tls, ctx_id);
    return NULL;
}

static int
_tlocal_ctx_record_cleanup(_tlocal_ctx_t *ctx_rec)
{
    opal_common_ucx_winfo_t *winfo = ctx_rec->winfo;
    if (0 == ctx_rec->ctx_id) {
        return OPAL_SUCCESS;
    }
    /* Remove myself from the communication context structure
     * This may result in context release as we are using
     * delayed cleanup */
    _common_ucx_wpctx_remove(ctx_rec->gctx, ctx_rec);

    /* Mark this winfo as free and it will be garbage-collected by
     * progress or flush function */
    opal_mutex_lock(&winfo->mutex);
    winfo->released = 1;
    opal_mutex_unlock(&winfo->mutex);

    /* Erase the record so it can be reused */
    memset(ctx_rec, 0, sizeof(*ctx_rec));
    DBG_OUT("_tlocal_cleanup_ctx_record(end): ctx_rec = %p\n", (void *)ctx_rec);
    return OPAL_SUCCESS;
}

// TODO: Don't want to inline this (slow path)
static _tlocal_ctx_t *
_tlocal_add_ctx(_tlocal_table_t *tls, opal_common_ucx_ctx_t *ctx)
{
    size_t i;
    int rc;

    /* Try to find available record in the TLS table */
    for (i=0; i<tls->ctx_tbl_size; i++) {
        if (0 == tls->ctx_tbl[i]->ctx_id) {
            /* Found clean record */
            break;
        }
        if (tls->ctx_tbl[i]->released ) {
            /* Found dirty record, need to clean first */
            _tlocal_ctx_record_cleanup(tls->ctx_tbl[i]);
            break;
        }
    }

    /* if needed - extend the table */
    if( i >= tls->ctx_tbl_size ){
        i = tls->ctx_tbl_size;
        rc = _tlocal_tls_ctxtbl_extend(tls, 4);
        if (rc) {
            //TODO: error out
            return NULL;
        }
    }

    tls->ctx_tbl[i]->ctx_id = ctx->ctx_id;
    tls->ctx_tbl[i]->gctx = ctx;
    tls->ctx_tbl[i]->winfo = _wpool_get_idle(tls->wpool, ctx->comm_size);
    if (NULL == tls->ctx_tbl[i]->winfo) {
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
    _wpool_add_active(tls->wpool, tls->ctx_tbl[i]->winfo);

    /* add this worker into the context list */
    rc = _common_ucx_wpctx_append(ctx, tls->ctx_tbl[i]);
    if (rc) {
        //TODO: error out
        return NULL;
    }

    DBG_OUT("_tlocal_add_ctx: tls = %p, ctx_rec = %p, winfo = %p\n",
            (void *)tls, (void *)&tls->ctx_tbl[i],
            (void *)tls->ctx_tbl[i]->winfo);

    /* All good - return the record */
    return tls->ctx_tbl[i];
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
    DBG_OUT("_tlocal_ctx_connect(after ucp_ep_create): worker = %p ep = %p\n",
            (void *)winfo->worker, (void *)winfo->endpoints[target]);
    opal_mutex_unlock(&winfo->mutex);
    return OPAL_SUCCESS;
}

/* TLS memory management */

static inline _tlocal_mem_t *
_tlocal_search_mem(_tlocal_table_t *tls, int mem_id)
{
    size_t i;
    DBG_OUT("_tlocal_search_mem(begin): tls = %p mem_id = %d\n",
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
    DBG_OUT("_tlocal_mem_record_cleanup: record=%p, is_freed = %d\n",
            (void *)mem_rec, mem_rec->released);
    if (mem_rec->released) {
        return;
    }
    /* Remove myself from the memory context structure
     * This may result in context release as we are using
     * delayed cleanup */
    _common_ucx_mem_remove(mem_rec->gmem, mem_rec);
    DBG_OUT("_tlocal_mem_record_cleanup(_common_ucx_mem_remove): gmem = %p mem_rec = %p\n",
            (void *)mem_rec->gmem, (void *)mem_rec);

    for(i = 0; i < mem_rec->gmem->ctx->comm_size; i++) {
        if (mem_rec->mem->rkeys[i]) {
            ucp_rkey_destroy(mem_rec->mem->rkeys[i]);
            DBG_OUT("_tlocal_mem_record_cleanup(after ucp_rkey_destroy): rkey_entry = %p\n",
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


// TODO: Don't want to inline this (slow path)
static _tlocal_mem_t *_tlocal_add_mem(_tlocal_table_t *tls,
                                       opal_common_ucx_wpmem_t *mem)
{
    size_t i;
    _tlocal_ctx_t *ctx_rec = NULL;
    int rc = OPAL_SUCCESS;

    /* Try to find available spot in the table */
    for (i=0; i<tls->mem_tbl_size; i++) {
        if (0 == tls->mem_tbl[i]->mem_id) {
            /* Found a clear record */
        }
        if (tls->mem_tbl[i]->released) {
            /* Found a dirty record. Need to clean it first */
            _tlocal_mem_record_cleanup(tls->mem_tbl[i]);
            DBG_OUT("_tlocal_add_mem(after _tlocal_mem_record_cleanup): tls = %p mem_tbl_entry = %p\n",
                    (void *)tls, (void *)tls->mem_tbl[i]);
            break;
        }
    }

    if( i >= tls->mem_tbl_size ){
        i = tls->mem_tbl_size;
        rc = _tlocal_tls_memtbl_extend(tls, 4);
        if (rc != OPAL_SUCCESS) {
            //TODO: error out
            return NULL;
        }
        DBG_OUT("_tlocal_add_mem(after _tlocal_tls_memtbl_extend): tls = %p\n",
                (void *)tls);
    }
    tls->mem_tbl[i]->mem_id = mem->mem_id;
    tls->mem_tbl[i]->gmem = mem;
    tls->mem_tbl[i]->released = 0;
    tls->mem_tbl[i]->mem = calloc(1, sizeof(*tls->mem_tbl[i]->mem));
    ctx_rec = _tlocal_ctx_search(tls, mem->ctx->ctx_id);
    if (NULL == ctx_rec) {
        // TODO: act accordingly - cleanup
        return NULL;
    }
    DBG_OUT("_tlocal_add_mem(after _tlocal_ctx_search): tls = %p, ctx_id = %d\n",
            (void *)tls, (int)mem->ctx->ctx_id);

    tls->mem_tbl[i]->mem->worker = ctx_rec->winfo;
    tls->mem_tbl[i]->mem->rkeys = calloc(mem->ctx->comm_size,
                                         sizeof(*tls->mem_tbl[i]->mem->rkeys));

    tls->mem_tbl[i]->mem_tls_ptr =
            calloc(1, sizeof(*tls->mem_tbl[i]->mem_tls_ptr));
    tls->mem_tbl[i]->mem_tls_ptr->winfo = ctx_rec->winfo;
    tls->mem_tbl[i]->mem_tls_ptr->rkeys = tls->mem_tbl[i]->mem->rkeys;
    pthread_setspecific(mem->mem_tls_key, tls->mem_tbl[i]->mem_tls_ptr);

    /* Make sure that we completed all the data structures before
     * placing the item to the list
     * NOTE: essentially we don't need this as list append is an
     * operation protected by mutex
     */
    opal_atomic_wmb();

    rc = _common_ucx_wpmem_append(mem, tls->mem_tbl[i]);
    if (rc) {
        // TODO: error handling
        return NULL;
    }
    DBG_OUT("_tlocal_add_mem(after _common_ucx_mem_append): mem = %p, mem_tbl_entry = %p\n",
            (void *)mem, (void *)tls->mem_tbl[i]);

    return tls->mem_tbl[i];
}

static int _tlocal_mem_create_rkey(_tlocal_mem_t *mem_rec, ucp_ep_h ep, int target)
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
    DBG_OUT("_tlocal_mem_create_rkey(after ucp_ep_rkey_unpack): mem_rec = %p ep = %p target = %d\n",
            (void *)mem_rec, (void *)ep, target);
    return OPAL_SUCCESS;
}

/* TODO: no inline */
OPAL_DECLSPEC int opal_common_ucx_tlocal_fetch_spath(opal_common_ucx_wpmem_t *mem, int target)
{
    _tlocal_table_t *tls = NULL;
    _tlocal_ctx_t *ctx_rec = NULL;
    opal_common_ucx_winfo_t *winfo = NULL;
    _tlocal_mem_t *mem_rec = NULL;
    _mem_info_t *mem_info = NULL;
    ucp_ep_h ep;
    int rc = OPAL_SUCCESS;

    DBG_OUT("_tlocal_fetch: starttls \n");

    tls = _tlocal_get_tls(mem->ctx->wpool);

    DBG_OUT("_tlocal_fetch: tls = %p\n",(void*)tls);

    /* Obtain the worker structure */
    ctx_rec = _tlocal_ctx_search(tls, mem->ctx->ctx_id);

    DBG_OUT("_tlocal_fetch(after _tlocal_ctx_search): ctx_id = %d, ctx_rec=%p\n",
            (int)mem->ctx->ctx_id, (void *)ctx_rec);
    if (OPAL_UNLIKELY(NULL == ctx_rec)) {
        ctx_rec = _tlocal_add_ctx(tls, mem->ctx);
        if (NULL == ctx_rec) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
        DBG_OUT("_tlocal_fetch(after _tlocal_add_ctx): tls = %p ctx = %p\n", (void *)tls, (void *)mem->ctx);
    }
    winfo = ctx_rec->winfo;
    DBG_OUT("_tlocal_fetch: winfo = %p ctx=%p\n", (void *)winfo, (void *)mem->ctx);

    /* Obtain the endpoint */
    if (OPAL_UNLIKELY(NULL == winfo->endpoints[target])) {
        rc = _tlocal_ctx_connect(ctx_rec, target);
        if (rc != OPAL_SUCCESS) {
            return rc;
        }
        DBG_OUT("_tlocal_fetch(after _tlocal_ctx_connect): ctx_rec = %p target = %d\n", (void *)ctx_rec, target);
    }
    ep = winfo->endpoints[target];
    DBG_OUT("_tlocal_fetch: ep = %p\n", (void *)ep);

    /* Obtain the memory region info */
    mem_rec = _tlocal_search_mem(tls, mem->mem_id);
    DBG_OUT("_tlocal_fetch: tls = %p mem_rec = %p mem_id = %d\n", (void *)tls, (void *)mem_rec, (int)mem->mem_id);
    if (OPAL_UNLIKELY(mem_rec == NULL)) {
        mem_rec = _tlocal_add_mem(tls, mem);
        DBG_OUT("_tlocal_fetch(after _tlocal_add_mem): tls = %p mem = %p\n", (void *)tls, (void *)mem);
        if (NULL == mem_rec) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
    }
    mem_info = mem_rec->mem;
    DBG_OUT("_tlocal_fetch: mem_info = %p\n", (void *)mem_info);

    /* Obtain the rkey */
    if (OPAL_UNLIKELY(NULL == mem_info->rkeys[target])) {
        /* Create the rkey */
        rc = _tlocal_mem_create_rkey(mem_rec, ep, target);
        if (rc) {
            return rc;
        }
        DBG_OUT("_tlocal_fetch: creating rkey ...\n");
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

    DBG_OUT("opal_common_ucx_mem_flush: mem = %p, target = %d\n", (void *)mem, target);

    // TODO: make this as a read lock
    //opal_mutex_lock(&ctx->mutex);
    pthread_rwlock_rdlock(&ctx->rwlock);

    OPAL_LIST_FOREACH(item, &ctx->tls_workers, _ctx_record_list_item_t) {
        switch (scope) {
        case OPAL_COMMON_UCX_SCOPE_WORKER:
            opal_mutex_lock(&item->ptr->winfo->mutex);
            rc = opal_common_ucx_worker_flush(item->ptr->winfo->worker);
            if (rc != OPAL_SUCCESS) {
                MCA_COMMON_UCX_VERBOSE(1, "opal_common_ucx_worker_flush failed: %d", rc);
                rc = OPAL_ERROR;
            }
            DBG_OUT("opal_common_ucx_mem_flush(after opal_common_ucx_worker_flush): worker = %p\n",
                    (void *)item->ptr->winfo->worker);
            opal_mutex_unlock(&item->ptr->winfo->mutex);
            break;
        case OPAL_COMMON_UCX_SCOPE_EP:
            if (NULL != item->ptr->winfo->endpoints[target] ) {
                opal_mutex_lock(&item->ptr->winfo->mutex);
                rc = opal_common_ucx_ep_flush(item->ptr->winfo->endpoints[target],
                                              item->ptr->winfo->worker);
                if (rc != OPAL_SUCCESS) {
                    MCA_COMMON_UCX_VERBOSE(1, "opal_common_ucx_ep_flush failed: %d", rc);
                    rc = OPAL_ERROR;
                }
                DBG_OUT("opal_common_ucx_mem_flush(after opal_common_ucx_worker_flush): ep = %p worker = %p\n",
                        (void *)item->ptr->winfo->endpoints[target],
                        (void *)item->ptr->winfo->worker);
                opal_mutex_unlock(&item->ptr->winfo->mutex);
            }
        }
    }
    //opal_mutex_unlock(&ctx->mutex);
    pthread_rwlock_unlock(&ctx->rwlock);

    return rc;
}

OPAL_DECLSPEC int
opal_common_ucx_wpmem_fence(opal_common_ucx_wpmem_t *mem) {
    /* TODO */
    return OPAL_SUCCESS;
}

