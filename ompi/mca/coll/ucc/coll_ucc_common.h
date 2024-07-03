/**
  Copyright (c) 2021      Mellanox Technologies. All rights reserved.
  $COPYRIGHT$
  Additional copyrights may follow
  $HEADER$
 */

#ifndef MCA_COLL_UCC_COMMON_H
#define MCA_COLL_UCC_COMMON_H

#include "ompi/constants.h"
#include "coll_ucc.h"
#include "coll_ucc_dtypes.h"

#define COLL_UCC_CHECK(_call) do {              \
        if (UCC_OK != (_call)) {                \
            goto fallback;                      \
        }                                       \
    } while(0)

#define COLL_UCC_POST_AND_CHECK(_req) do {           \
        if (UCC_OK != ucc_collective_post(_req)) {   \
            ucc_collective_finalize(_req);           \
            goto fallback;                           \
        }                                            \
    } while(0)

#define COLL_UCC_GET_REQ(_coll_req) do {                                \
        opal_free_list_item_t *item;                                    \
        item = opal_free_list_wait (&mca_coll_ucc_component.requests);  \
        if (OPAL_UNLIKELY(NULL == item)) {                              \
            UCC_ERROR("failed to get mca_coll_ucc_req from free_list"); \
            goto fallback;                                              \
        }                                                               \
        _coll_req = (mca_coll_ucc_req_t*)item;                          \
        OMPI_REQUEST_INIT(&_coll_req->super, false);                    \
        _coll_req->super.req_complete_cb      = NULL;                   \
        _coll_req->super.req_complete_cb_data = NULL;                   \
        _coll_req->super.req_status.MPI_ERROR = MPI_SUCCESS;            \
        _coll_req->super.req_state            = OMPI_REQUEST_ACTIVE;    \
        _coll_req->super.req_free             = mca_coll_ucc_req_free;  \
        _coll_req->super.req_type             = OMPI_REQUEST_COLL;      \
    } while(0)

#define COLL_UCC_REQ_INIT(_coll_req, _req, _coll, _module) do{          \
        if (_coll_req) {                                                \
            _coll.mask   |= UCC_COLL_ARGS_FIELD_CB;                     \
            _coll.cb.cb   = mca_coll_ucc_completion;                    \
            _coll.cb.data = (void*)_coll_req;                           \
        } else {                                                        \
            _coll.mask  |= UCC_COLL_ARGS_FIELD_FLAGS;                   \
            _coll.flags |= UCC_COLL_ARGS_HINT_OPTIMIZE_LATENCY;         \
        }                                                               \
        COLL_UCC_CHECK(ucc_collective_init(&_coll, _req,                \
                                           _module->ucc_team));         \
        if (_coll_req) {                                                \
            _coll_req->ucc_req = *(_req);                               \
        }                                                               \
    } while(0)

static inline ucc_status_t coll_ucc_req_wait(ucc_coll_req_h req)
{
    ucc_status_t status;
    while (UCC_OK != (status = ucc_collective_test(req))) {
        if (status < 0) {
            UCC_ERROR("ucc_collective_test failed: %s",
                      ucc_status_string(status));
            ucc_collective_finalize(req);
            return status;
        }
        ucc_context_progress(mca_coll_ucc_component.ucc_context);
        opal_progress();
    }
    return ucc_collective_finalize(req);
}

int mca_coll_ucc_req_free(struct ompi_request_t **ompi_req);
void mca_coll_ucc_completion(void *data, ucc_status_t status);

#endif
