#ifndef COMMON_UCX_WPOOL_INT_H
#define COMMON_UCX_WPOOL_INT_H

#include "opal_config.h"
#include "common_ucx_int.h"
#include "common_ucx_wpool.h"

typedef struct {
    int ctx_id;
    opal_common_ucx_ctx_t *gctx;
    opal_common_ucx_winfo_t *winfo;
} _tlocal_ctx_t;

typedef struct {
    opal_common_ucx_winfo_t *worker;
    ucp_rkey_h *rkeys;
} _mem_info_t;

typedef struct {
    int mem_id;
    opal_common_ucx_wpmem_t *gmem;
    _mem_info_t *mem;
    opal_common_ucx_tlocal_fast_ptrs_t *mem_tls_ptr;
} _tlocal_mem_t;

typedef struct {
    opal_list_item_t super;
    opal_common_ucx_winfo_t *ptr;
} _winfo_list_item_t;
OBJ_CLASS_DECLARATION(_winfo_list_item_t);


typedef struct {
    opal_list_item_t super;
    opal_common_ucx_winfo_t *ptr;
} _ctx_record_list_item_t;
OBJ_CLASS_DECLARATION(_ctx_record_list_item_t);

typedef struct {
    opal_list_item_t super;
    _tlocal_mem_t *ptr;
} _mem_record_list_item_t;
OBJ_CLASS_DECLARATION(_mem_record_list_item_t);

/* thread-local table */
typedef struct {
    opal_list_item_t super;
    opal_common_ucx_wpool_t *wpool;
    _tlocal_ctx_t **ctx_tbl;
    size_t ctx_tbl_size;
    _tlocal_mem_t **mem_tbl;
    size_t mem_tbl_size;
} _tlocal_table_t;

OBJ_CLASS_DECLARATION(_tlocal_table_t);



static int _tlocal_tls_ctxtbl_extend(_tlocal_table_t *tbl, size_t append);
static int _tlocal_tls_memtbl_extend(_tlocal_table_t *tbl, size_t append);
static _tlocal_table_t* _common_ucx_tls_init(opal_common_ucx_wpool_t *wpool);
static void _common_ucx_tls_cleanup(_tlocal_table_t *tls);
static inline _tlocal_ctx_t *_tlocal_ctx_search(_tlocal_table_t *tls, int ctx_id);
static int _tlocal_ctx_record_cleanup(_tlocal_ctx_t *ctx_rec);
static _tlocal_ctx_t *_tlocal_add_ctx(_tlocal_table_t *tls, opal_common_ucx_ctx_t *ctx);
static int _tlocal_ctx_connect(_tlocal_ctx_t *ctx, int target);
static inline _tlocal_mem_t *_tlocal_search_mem(_tlocal_table_t *tls, int mem_id);
static _tlocal_mem_t *_tlocal_add_mem(_tlocal_table_t *tls, opal_common_ucx_wpmem_t *mem);
static int _tlocal_mem_create_rkey(_tlocal_mem_t *mem_rec, ucp_ep_h ep, int target);
// TOD: Return the error from it
static void _tlocal_mem_record_cleanup(_tlocal_mem_t *mem_rec);


static void _tlocal_cleanup(void *arg);

/* Sorted declarations */


/* Internal Worker Information (winfo) management */
static opal_common_ucx_winfo_t *_winfo_create(opal_common_ucx_wpool_t *wpool);
static void _winfo_release(opal_common_ucx_winfo_t *winfo);
static void _winfo_reset(opal_common_ucx_winfo_t *winfo);

/* Internal Worker Pool (wpool) management */
static int _wpool_list_put(opal_common_ucx_wpool_t *wpool, opal_list_t *list,
                           opal_common_ucx_winfo_t *winfo);
static int _wpool_list_put(opal_common_ucx_wpool_t *wpool, opal_list_t *list,
                           opal_common_ucx_winfo_t *winfo);
static opal_common_ucx_winfo_t *_wpool_list_get(opal_common_ucx_wpool_t *wpool,
                                       opal_list_t *list);
static opal_common_ucx_winfo_t *_wpool_get_idle(opal_common_ucx_wpool_t *wpool,
                                       size_t comm_size);
static int _wpool_add_active(opal_common_ucx_wpool_t *wpool,
                             opal_common_ucx_winfo_t *winfo);

/* Internal Worker Pool Context management */
static void _common_ucx_wpctx_free(opal_common_ucx_ctx_t *ctx);
static int _common_ucx_wpctx_append(opal_common_ucx_ctx_t *ctx,
                                    opal_common_ucx_winfo_t *winfo);
static void _common_ucx_wpctx_remove(opal_common_ucx_ctx_t *ctx,
                                     opal_common_ucx_winfo_t *winfo);

/* Internal Worker Pool Memeory management */
static int _comm_ucx_wpmem_map(opal_common_ucx_wpool_t *wpool,
                               void **base, size_t size, ucp_mem_h *memh_ptr,
                               opal_common_ucx_mem_type_t mem_type);
static void _common_ucx_wpmem_free(opal_common_ucx_wpmem_t *mem);
static int _common_ucx_wpmem_signup(opal_common_ucx_wpmem_t *mem);
static void _common_ucx_mem_signout(opal_common_ucx_wpmem_t *mem);


#endif // COMMON_UCX_WPOOL_INT_H
