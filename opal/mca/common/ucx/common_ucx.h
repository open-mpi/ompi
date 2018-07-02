/*
 * Copyright (c) 2018      Mellanox Technologies.  All rights reserved.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef _COMMON_UCX_H_
#define _COMMON_UCX_H_

#include "opal_config.h"

#include <stdint.h>

#include <ucp/api/ucp.h>

#include "opal/mca/mca.h"
#include "opal/runtime/opal_progress.h"
#include "opal/class/opal_list.h"

BEGIN_C_DECLS

extern int opal_common_ucx_progress_iterations;

OPAL_DECLSPEC void opal_common_ucx_mca_register(void);
OPAL_DECLSPEC void opal_common_ucx_empty_complete_cb(void *request, ucs_status_t status);
OPAL_DECLSPEC void opal_common_ucx_mca_pmix_fence(ucp_worker_h worker);

static inline
ucs_status_t opal_common_ucx_wait_request(ucs_status_ptr_t request, ucp_worker_h worker)
{
    ucs_status_t status;
    int i;
#if !HAVE_DECL_UCP_REQUEST_CHECK_STATUS
    ucp_tag_recv_info_t info;
#endif

    /* check for request completed or failed */
    if (OPAL_LIKELY(UCS_OK == request)) {
        return UCS_OK;
    } else if (OPAL_UNLIKELY(UCS_PTR_IS_ERR(request))) {
        return UCS_PTR_STATUS(request);
    }

    while (1) {
        /* call UCX progress */
        for (i = 0; i < opal_common_ucx_progress_iterations; i++) {
            if (UCS_INPROGRESS != (status =
#if HAVE_DECL_UCP_REQUEST_CHECK_STATUS
                ucp_request_check_status(request)
#else
                ucp_request_test(request, &info)
#endif
                )) {
                ucp_request_free(request);
                return status;
            }
            ucp_worker_progress(worker);
        }
        /* call OPAL progress on every opal_common_ucx_progress_iterations
         * calls to UCX progress */
        opal_progress();
    }
}

static inline
ucs_status_t opal_common_ucx_ep_flush(ucp_ep_h ep, ucp_worker_h worker)
{
#if HAVE_DECL_UCP_EP_FLUSH_NB
    ucs_status_ptr_t status;

    status = ucp_ep_flush_nb(ep, 0, opal_common_ucx_empty_complete_cb);
    return opal_common_ucx_wait_request(status, worker);
#else
    return ucp_ep_flush(ep);
#endif
}

static inline
ucs_status_t opal_common_ucx_worker_flush(ucp_worker_h worker)
{
#if HAVE_DECL_UCP_WORKER_FLUSH_NB
    ucs_status_ptr_t status;

    status = ucp_worker_flush_nb(worker, 0, opal_common_ucx_empty_complete_cb);
    return opal_common_ucx_wait_request(status, worker);
#else
    return ucp_worker_flush(worker);
#endif
}

static inline
ucs_status_t opal_common_ucx_atomic_fetch(ucp_ep_h ep, ucp_atomic_fetch_op_t opcode,
                                          uint64_t value, void *result, size_t op_size,
                                          uint64_t remote_addr, ucp_rkey_h rkey,
                                          ucp_worker_h worker)
{
    ucs_status_ptr_t request;

    request = ucp_atomic_fetch_nb(ep, opcode, value, result, op_size,
                                  remote_addr, rkey, opal_common_ucx_empty_complete_cb);
    return opal_common_ucx_wait_request(request, worker);
}

static inline
ucs_status_t opal_common_ucx_atomic_cswap(ucp_ep_h ep, uint64_t compare,
                                          uint64_t value, void *result, size_t op_size,
                                          uint64_t remote_addr, ucp_rkey_h rkey,
                                          ucp_worker_h worker)
{
    uint64_t tmp = value;
    ucs_status_t status;

    status = opal_common_ucx_atomic_fetch(ep, UCP_ATOMIC_FETCH_OP_CSWAP, compare, &tmp,
                                          op_size, remote_addr, rkey, worker);
    if (OPAL_LIKELY(UCS_OK == status)) {
        /* in case if op_size is constant (like sizeof(type)) then this condition
         * is evaluated in compile time */
        if (op_size == sizeof(uint64_t)) {
            *(uint64_t*)result = tmp;
        } else {
            assert(op_size == sizeof(uint32_t));
            *(uint32_t*)result = tmp;
        }
    }
    return status;
}

END_C_DECLS

#endif
