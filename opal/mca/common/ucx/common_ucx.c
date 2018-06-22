/*
 * Copyright (C) Mellanox Technologies Ltd. 2018. ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "common_ucx.h"

/***********************************************************************/

static void opal_common_ucp_send_cb(void *request, ucs_status_t status)
{
}

ucs_status_t opal_common_ucx_ep_flush(ucp_ep_h ep, ucp_worker_h worker)
{
    ucs_status_ptr_t status;

    status = ucp_ep_flush_nb(ep, 0, opal_common_ucp_send_cb);
    return opal_common_ucx_wait_request(status, worker);
}

ucs_status_t opal_common_ucx_worker_flush(ucp_worker_h worker)
{
    ucs_status_ptr_t status;

    status = ucp_worker_flush_nb(worker, 0, opal_common_ucp_send_cb);
    return opal_common_ucx_wait_request(status, worker);
}
