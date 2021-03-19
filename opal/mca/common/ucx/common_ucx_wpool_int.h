#ifndef COMMON_UCX_WPOOL_INT_H
#define COMMON_UCX_WPOOL_INT_H

#include "opal_config.h"
#include "common_ucx.h"
#include "common_ucx_wpool.h"

static int _tlocal_ctx_connect(_ctx_record_t *ctx_rec, int target);
static int _tlocal_mem_create_rkey(_mem_record_t *mem_rec, ucp_ep_h ep, int target);

/* Sorted declarations */

/* Internal Worker Information (winfo) management */
static opal_common_ucx_winfo_t *_winfo_create(opal_common_ucx_wpool_t *wpool);
static void _winfo_destructor(opal_common_ucx_winfo_t *winfo);

/* Internal Worker Pool Memory management */
static int _comm_ucx_wpmem_map(opal_common_ucx_wpool_t *wpool, void **base, size_t size,
                               ucp_mem_h *memh_ptr, opal_common_ucx_mem_type_t mem_type);

#endif // COMMON_UCX_WPOOL_INT_H
