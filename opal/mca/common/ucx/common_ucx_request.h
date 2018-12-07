#ifndef COMMON_UCX_REQUEST_H
#define COMMON_UCX_REQUEST_H

#include "opal_config.h"
#include <ucp/api/ucp.h>

typedef void (*opal_common_ucx_user_req_handler_t)(void *request);

typedef struct {
    void *ext_req;
    opal_common_ucx_user_req_handler_t ext_cb;
} opal_common_ucx_request_t;

OPAL_DECLSPEC void opal_common_ucx_req_init(void *request);
OPAL_DECLSPEC void opal_common_ucx_req_completion(void *request, ucs_status_t status);

#endif // COMMON_UCX_REQUEST_H
