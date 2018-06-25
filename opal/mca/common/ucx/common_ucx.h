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

static inline
ucs_status_t opal_common_ucx_wait_request(ucs_status_ptr_t request, ucp_worker_h worker)
{
    ucs_status_t status;
    int i;

    /* check for request completed or failed */
    if (OPAL_LIKELY(UCS_OK == request)) {
        return UCS_OK;
    } else if (OPAL_UNLIKELY(UCS_PTR_IS_ERR(request))) {
        return UCS_PTR_STATUS(request);
    }

    while (1) {
        /* call UCX progress */
        for (i = 0; i < opal_common_ucx_progress_iterations; i++) {
            if (UCS_INPROGRESS != (status = ucp_request_check_status(request))) {
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
    ucs_status_ptr_t status;

    status = ucp_ep_flush_nb(ep, 0, opal_common_ucx_empty_complete_cb);
    return opal_common_ucx_wait_request(status, worker);
}

static inline
ucs_status_t opal_common_ucx_worker_flush(ucp_worker_h worker)
{
    ucs_status_ptr_t status;

    status = ucp_worker_flush_nb(worker, 0, opal_common_ucx_empty_complete_cb);
    return opal_common_ucx_wait_request(status, worker);
}
END_C_DECLS

#endif
