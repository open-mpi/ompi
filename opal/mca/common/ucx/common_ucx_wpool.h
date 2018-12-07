#ifndef COMMON_UCX_WPOOL_H
#define COMMON_UCX_WPOOL_H


#include "opal_config.h"

#include "common_ucx_int.h"
#include <stdint.h>

#include <ucp/api/ucp.h>
#include <pthread.h>

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/runtime/opal_progress.h"
#include "opal/include/opal/constants.h"
#include "opal/class/opal_list.h"

BEGIN_C_DECLS

typedef struct {
    /* Ref counting & locking*/
    int refcnt;
    opal_mutex_t mutex;

    /* UCX data */
    ucp_context_h ucp_ctx;
    ucp_worker_h recv_worker;
    ucp_address_t *recv_waddr;
    size_t recv_waddr_len;

    /* Thread-local key to allow each thread to have
     * local information assisiated with this wpool */
    pthread_key_t tls_key;

    /* Bookkeeping information */
    opal_list_t idle_workers;
    opal_list_t active_workers;

    opal_atomic_int32_t cur_ctxid, cur_memid;
    opal_list_t tls_list;
} opal_common_ucx_wpool_t;

typedef struct {
    int ctx_id;
    opal_mutex_t mutex;
    opal_atomic_int32_t refcntr;

    /* the reference to a Worker pool this context belongs to*/
    opal_common_ucx_wpool_t *wpool;
    /* A list of references to TLS context records
     * we need to keep track of them to have an ability to
     * let thread know that this context is no longer valid */
    opal_list_t tls_workers;
    volatile int released;

    /* UCX addressing information */
    char *recv_worker_addrs;
    int *recv_worker_displs;
    size_t comm_size;
} opal_common_ucx_ctx_t;

typedef struct {
    int mem_id;
    /* reference context to which memory region belongs */
    opal_common_ucx_ctx_t *ctx;

    /* object lifetime control */
    volatile int released;
    opal_atomic_int32_t refcntr;

    /* UCX memory handler */
    ucp_mem_h memh;
    char *mem_addrs;
    int *mem_displs;

    /* TLS item that allows each thread to
     * store endpoints and rkey arrays
     * for faster access */
    pthread_key_t mem_tls_key;
} opal_common_ucx_wpmem_t;

typedef struct  {
    opal_mutex_t mutex;
    volatile int released;
    ucp_worker_h worker;
    ucp_ep_h *endpoints;
    size_t comm_size;
} opal_common_ucx_winfo_t;

typedef struct {
    opal_common_ucx_winfo_t *winfo;
    ucp_rkey_h *rkeys;
} opal_common_ucx_tlocal_fast_ptrs_t;

typedef enum {
    OPAL_COMMON_UCX_PUT,
    OPAL_COMMON_UCX_GET
} opal_common_ucx_op_t;

typedef enum {
    OPAL_COMMON_UCX_SCOPE_EP,
    OPAL_COMMON_UCX_SCOPE_WORKER
} opal_common_ucx_flush_scope_t;

typedef enum {
    OPAL_COMMON_UCX_MEM_ALLOCATE_MAP,
    OPAL_COMMON_UCX_MEM_MAP
} opal_common_ucx_mem_type_t;

typedef int (*opal_common_ucx_exchange_func_t)(void *my_info, size_t my_info_len,
                                               char **recv_info, int **disps,
                                               void *metadata);

/* For developer use only */
#define OPAL_COMMON_UCX_WPOOL_DBG
#ifdef OPAL_COMMON_UCX_WPOOL_DBG
extern __thread FILE *tls_pf;
extern __thread int initialized;

#include  <unistd.h>
#include <sys/syscall.h>
#include <time.h>
#include <sys/time.h>

static int _dbg_winfo = 0;
static int _dbg_wpool = 0;
static int _dbg_ctx = 0;
static int _dbg_mem = 0;
static int _dbg_tls = 0;

static inline void opal_common_ucx_wpool_dbg_init(void)
{
    if( !initialized ) {
        int tid = syscall(__NR_gettid);
        char hname[128];
        gethostname(hname, 127);
        char fname[128];

        sprintf(fname, "%s.%d.log", hname, tid);
        tls_pf = fopen(fname, "w");
        initialized = 1;
        // Create issusion that they are used to avoid compiler warnings
        (void)_dbg_ctx;
        (void)_dbg_mem;
        (void)_dbg_tls;
        (void)_dbg_winfo;
        (void)_dbg_wpool;
    }
}


#define WPOOL_DBG_OUT(level, ...)                    \
{                                              \
    struct timeval start_;                     \
    time_t nowtime_;                           \
    struct tm *nowtm_;                         \
    char tmbuf_[64];                           \
    gettimeofday(&start_, NULL);               \
    nowtime_ = start_.tv_sec;                  \
    nowtm_ = localtime(&nowtime_);             \
    strftime(tmbuf_, sizeof(tmbuf_),           \
               "%H:%M:%S", nowtm_);            \
    opal_common_ucx_wpool_dbg_init();          \
    if (level) {                               \
        fprintf(tls_pf, "[%s.%06ld] %s:",      \
                      tmbuf_, start_.tv_usec,  \
                      __func__);          \
    }                                          \
    fprintf(tls_pf, __VA_ARGS__);              \
}

#else
#define DBG_OUT(...)
#endif


/* Manage Worker Pool (wpool) */
OPAL_DECLSPEC opal_common_ucx_wpool_t * opal_common_ucx_wpool_allocate(void);
OPAL_DECLSPEC void opal_common_ucx_wpool_free(opal_common_ucx_wpool_t *wpool);
OPAL_DECLSPEC int opal_common_ucx_wpool_init(opal_common_ucx_wpool_t *wpool,
                                             int proc_world_size,
                                             ucp_request_init_callback_t req_init_ptr,
                                             size_t req_size, bool enable_mt);
OPAL_DECLSPEC void opal_common_ucx_wpool_finalize(opal_common_ucx_wpool_t *wpool);
OPAL_DECLSPEC void opal_common_ucx_wpool_progress(opal_common_ucx_wpool_t *wpool);

/* Manage Communication context */
OPAL_DECLSPEC int opal_common_ucx_wpctx_create(opal_common_ucx_wpool_t *wpool, int comm_size,
                                             opal_common_ucx_exchange_func_t exchange_func,
                                             void *exchange_metadata,
                                             opal_common_ucx_ctx_t **ctx_ptr);
OPAL_DECLSPEC void opal_common_ucx_wpctx_release(opal_common_ucx_ctx_t *ctx);

/* Managing thread local storage */
OPAL_DECLSPEC int opal_common_ucx_tlocal_fetch_spath(opal_common_ucx_wpmem_t *mem, int target);
static inline int
opal_common_ucx_tlocal_fetch(opal_common_ucx_wpmem_t *mem, int target,
                                ucp_ep_h *_ep, ucp_rkey_h *_rkey,
                                opal_common_ucx_winfo_t **_winfo)
{
    opal_common_ucx_tlocal_fast_ptrs_t *fp = NULL;
    int expr;
    int rc = OPAL_SUCCESS;

    /* First check the fast-path */
    fp = pthread_getspecific(mem->mem_tls_key);
    expr = fp && (NULL != fp->winfo) && (fp->winfo->endpoints[target]) &&
            (NULL != fp->rkeys[target]);
    if (OPAL_UNLIKELY(!expr)) {
        rc = opal_common_ucx_tlocal_fetch_spath(mem, target);
        if (OPAL_SUCCESS != rc) {
            return rc;
        }
        fp = pthread_getspecific(mem->mem_tls_key);
    }
    MCA_COMMON_UCX_ASSERT(fp && (NULL != fp->winfo) &&
                          (fp->winfo->endpoints[target])
                          && (NULL != fp->rkeys[target]));

    *_rkey = fp->rkeys[target];
    *_winfo = fp->winfo;
    *_ep = fp->winfo->endpoints[target];
    return OPAL_SUCCESS;
}

/* Manage & operations on the Memory registrations */
OPAL_DECLSPEC int opal_common_ucx_wpmem_create(opal_common_ucx_ctx_t *ctx,
                               void **mem_base, size_t mem_size,
                               opal_common_ucx_mem_type_t mem_type,
                               opal_common_ucx_exchange_func_t exchange_func,
                               void *exchange_metadata,
                               opal_common_ucx_wpmem_t **mem_ptr);
OPAL_DECLSPEC int opal_common_ucx_wpmem_free(opal_common_ucx_wpmem_t *mem);

OPAL_DECLSPEC int opal_common_ucx_wpmem_flush(opal_common_ucx_wpmem_t *mem,
                                            opal_common_ucx_flush_scope_t scope,
                                            int target);
OPAL_DECLSPEC int opal_common_ucx_wpmem_fence(opal_common_ucx_wpmem_t *mem);


static inline int
opal_common_ucx_wpmem_putget(opal_common_ucx_wpmem_t *mem, opal_common_ucx_op_t op,
                           int target, void *buffer, size_t len,
                           uint64_t rem_addr)
{
    ucp_ep_h ep;
    ucp_rkey_h rkey;
    ucs_status_t status;
    opal_common_ucx_winfo_t *winfo;
    int rc = OPAL_SUCCESS;
    char *called_func = "";

    rc = opal_common_ucx_tlocal_fetch(mem, target, &ep, &rkey, &winfo);
    if(OPAL_UNLIKELY(OPAL_SUCCESS != rc)){
        MCA_COMMON_UCX_VERBOSE(1, "tlocal_fetch failed: %d", rc);
        return rc;
    }
    WPOOL_DBG_OUT(_dbg_mem, "mem = %p, ep = %p, rkey = %p, winfo = %p\n",
                  (void *)mem, (void *)ep, (void *)rkey, (void *)winfo);

    /* Perform the operation */
    opal_mutex_lock(&winfo->mutex);
    switch(op){
    case OPAL_COMMON_UCX_PUT:
        status = ucp_put_nbi(ep, buffer,len, rem_addr, rkey);
        called_func = "ucp_put_nbi";
        break;
    case OPAL_COMMON_UCX_GET:
        status = ucp_get_nbi(ep, buffer,len, rem_addr, rkey);
        called_func = "ucp_get_nbi";
        break;
    }
    opal_mutex_unlock(&winfo->mutex);

    if (OPAL_UNLIKELY(status != UCS_OK && status != UCS_INPROGRESS)) {
        MCA_COMMON_UCX_ERROR("%s failed: %d", called_func, status);
        rc = OPAL_ERROR;
    } else {
        WPOOL_DBG_OUT(_dbg_mem,"ep = %p, rkey = %p\n",
                      (void *)ep, (void *)rkey);
    }
    return rc;
}


static inline int
opal_common_ucx_wpmem_cmpswp(opal_common_ucx_wpmem_t *mem, uint64_t compare,
                           uint64_t value, int target, void *buffer, size_t len,
                           uint64_t rem_addr)
{
    ucp_ep_h ep;
    ucp_rkey_h rkey;
    opal_common_ucx_winfo_t *winfo = NULL;
    ucs_status_t status;
    int rc = OPAL_SUCCESS;

    rc = opal_common_ucx_tlocal_fetch(mem, target, &ep, &rkey, &winfo);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        MCA_COMMON_UCX_ERROR("opal_common_ucx_tlocal_fetch failed: %d", rc);
        return rc;
    }
    WPOOL_DBG_OUT("mem = %p, ep = %p, rkey = %p, winfo = %p\n",
                  (void *)mem, (void *)ep, (void *)rkey, (void *)winfo);

    /* Perform the operation */
    opal_mutex_lock(&winfo->mutex);
    status = opal_common_ucx_atomic_cswap(ep, compare, value,
                                          buffer, len,
                                          rem_addr, rkey,
                                          winfo->worker);
    if (OPAL_UNLIKELY(status != UCS_OK)) {
        MCA_COMMON_UCX_ERROR("opal_common_ucx_atomic_cswap failed: %d", status);
        rc = OPAL_ERROR;
    } else {
        WPOOL_DBG_OUT(_dbg_mem, "ep = %p, rkey = %p\n",
                      (void *)ep, (void *)rkey);
    }
    opal_mutex_unlock(&winfo->mutex);

    return rc;
}

static inline int
opal_common_ucx_wpmem_post(opal_common_ucx_wpmem_t *mem, ucp_atomic_post_op_t opcode,
                         uint64_t value, int target, size_t len, uint64_t rem_addr)
{
    ucp_ep_h ep;
    ucp_rkey_h rkey;
    opal_common_ucx_winfo_t *winfo = NULL;
    ucs_status_t status;
    int rc = OPAL_SUCCESS;


    rc =opal_common_ucx_tlocal_fetch(mem, target, &ep, &rkey, &winfo);
    if(OPAL_UNLIKELY(OPAL_SUCCESS != rc)){
        MCA_COMMON_UCX_ERROR("tlocal_fetch failed: %d", rc);
        return rc;
    }
    WPOOL_DBG_OUT(_dbg_mem, "mem = %p, ep = %p, rkey = %p, winfo = %p\n",
                  (void *)mem, (void *)ep, (void *)rkey, (void *)winfo);

    /* Perform the operation */
    opal_mutex_lock(&winfo->mutex);
    status = ucp_atomic_post(ep, opcode, value,
                             len, rem_addr, rkey);
    if (OPAL_UNLIKELY(status != UCS_OK)) {
        MCA_COMMON_UCX_ERROR("ucp_atomic_post failed: %d", status);
        rc = OPAL_ERROR;
    } else {
        WPOOL_DBG_OUT(_dbg_mem, "ep = %p, rkey = %p\n",
                      (void *)ep, (void *)rkey);
    }
    opal_mutex_unlock(&winfo->mutex);
    return rc;
}

static inline int
opal_common_ucx_wpmem_fetch(opal_common_ucx_wpmem_t *mem,
                            ucp_atomic_fetch_op_t opcode, uint64_t value,
                            int target, void *buffer, size_t len,
                            uint64_t rem_addr)
{
    ucp_ep_h ep = NULL;
    ucp_rkey_h rkey = NULL;
    opal_common_ucx_winfo_t *winfo = NULL;
    ucs_status_t status;
    int rc = OPAL_SUCCESS;

    rc = opal_common_ucx_tlocal_fetch(mem, target, &ep, &rkey, &winfo);
    if(OPAL_UNLIKELY(OPAL_SUCCESS != rc)){
        MCA_COMMON_UCX_ERROR("tlocal_fetch failed: %d", rc);
        return rc;
    }
    WPOOL_DBG_OUT(_dbg_mem, "mem = %p, ep = %p, rkey = %p, winfo = %p\n",
                  (void *)mem, (void *)ep, (void *)rkey, (void *)winfo);

    /* Perform the operation */
    opal_mutex_lock(&winfo->mutex);
    status = opal_common_ucx_atomic_fetch(ep, opcode, value,
                                          buffer, len,
                                          rem_addr, rkey,
                                          winfo->worker);
    if (OPAL_UNLIKELY(status != UCS_OK)) {
        MCA_COMMON_UCX_ERROR("ucp_atomic_cswap64 failed: %d", status);
        rc = OPAL_ERROR;
    } else {
        WPOOL_DBG_OUT(_dbg_mem, "ep = %p, rkey = %p\n",
                      (void *)ep, (void *)rkey);
    }
    opal_mutex_unlock(&winfo->mutex);

    return rc;
}

static inline int
opal_common_ucx_wpmem_fetch_nb(opal_common_ucx_wpmem_t *mem,
                                 ucp_atomic_fetch_op_t opcode,
                                 uint64_t value,
                                 int target, void *buffer, size_t len,
                                 uint64_t rem_addr, ucs_status_ptr_t *ptr)
{
    ucp_ep_h ep = NULL;
    ucp_rkey_h rkey = NULL;
    opal_common_ucx_winfo_t *winfo = NULL;
    int rc = OPAL_SUCCESS;
    rc = opal_common_ucx_tlocal_fetch(mem, target, &ep, &rkey, &winfo);
    if(OPAL_UNLIKELY(OPAL_SUCCESS != rc)){
        MCA_COMMON_UCX_ERROR("tlocal_fetch failed: %d", rc);
        return rc;
    }
    /* Perform the operation */
    opal_mutex_lock(&winfo->mutex);
    (*ptr) = opal_common_ucx_atomic_fetch_nb(ep, opcode, value,
                                             buffer, len,
                                             rem_addr, rkey,
                                             winfo->worker);
    opal_mutex_unlock(&winfo->mutex);
    return rc;
}

END_C_DECLS

#endif // COMMON_UCX_WPOOL_H
