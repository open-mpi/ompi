#include "common_ucx_request.h"

OPAL_DECLSPEC void
opal_common_ucx_req_init(void *request) {
    opal_common_ucx_request_t *req = (opal_common_ucx_request_t *)request;
    req->ext_req = NULL;
    req->ext_cb = NULL;
}

OPAL_DECLSPEC void
opal_common_ucx_req_completion(void *request, ucs_status_t status) {
    opal_common_ucx_request_t *req = (opal_common_ucx_request_t *)request;
    if (req->ext_cb != NULL) {
        (*req->ext_cb)(req->ext_req);
    }
    ucp_request_release(req);
}
