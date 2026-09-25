/**
  Copyright (c) 2021      Mellanox Technologies. All rights reserved.
  Copyright (c) 2025      Fujitsu Limited. All rights reserved.
  Copyright (c) 2026      NVIDIA Corporation. All rights reserved.
  $COPYRIGHT$
  Additional copyrights may follow
  $HEADER$
  SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#ifndef MCA_COLL_UCC_COMMON_H
#define MCA_COLL_UCC_COMMON_H

#include "ompi/constants.h"
#include "coll_ucc.h"
#include "coll_ucc_dtypes.h"

/*
 * A collective UCC declined to start may safely run on the previous module:
 * every rank evaluates the same arguments against the same team, so either all
 * of them fall back or none of them do.
 *
 * The rooted collectives weaken that: the root also tests datatypes the other
 * ranks do not have, so it can decline alone and hang the job the same way.
 * Nothing here can detect it -- deciding together would require the peers'
 * datatypes, which are not available -- so it remains a known limitation.
 *
 * A collective that failed once it was posted may not. Only the ranks that
 * observed the failure would change module, while the rest stay in UCC waiting
 * for peers that have left, so the job hangs instead of failing. Report the
 * error to the caller and let the communicator's error handler decide.
 */
#define COLL_UCC_CHECK(_call) do {              \
        if (UCC_OK != (_call)) {                \
            goto fallback;                      \
        }                                       \
    } while(0)

#define COLL_UCC_POST_AND_CHECK(_req) do {                          \
        ucc_status_t _post_status = ucc_collective_post(_req);      \
        if (UCC_OK != _post_status) {                               \
            UCC_ERROR("ucc_collective_post failed: %s",             \
                      ucc_status_string(_post_status));             \
            ucc_collective_finalize(_req);                          \
            goto failed;                                            \
        }                                                           \
    } while(0)

#define COLL_UCC_CHECK_POSTED(_call) do {       \
        if (UCC_OK != (_call)) {                \
            goto failed;                        \
        }                                       \
    } while(0)

#define COLL_UCC_GET_REQ(_coll_req, _comm) do {                         \
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
        _coll_req->super.req_mpi_object.comm  = _comm;                  \
    } while(0)

#define COLL_UCC_GET_REQ_PERSISTENT(_coll_req, _comm)                   \
    do {                                                                \
        opal_free_list_item_t *item;                                    \
        item = opal_free_list_wait(&mca_coll_ucc_component.requests);   \
        if (OPAL_UNLIKELY(NULL == item)) {                              \
            UCC_ERROR("failed to get mca_coll_ucc_req from free_list"); \
            goto fallback;                                              \
        }                                                               \
        _coll_req = (mca_coll_ucc_req_t *) item;                        \
        OMPI_REQUEST_INIT(&_coll_req->super, true);                     \
        _coll_req->super.req_complete_cb = NULL;                        \
        _coll_req->super.req_complete_cb_data = NULL;                   \
        _coll_req->super.req_status.MPI_ERROR = MPI_SUCCESS;            \
        _coll_req->super.req_free = mca_coll_ucc_req_free;              \
        _coll_req->super.req_start = mca_coll_ucc_req_start;            \
        _coll_req->super.req_type = OMPI_REQUEST_COLL;                  \
        _coll_req->super.req_mpi_object.comm = _comm;                   \
        _coll_req->ucc_req = NULL;                                      \
    } while (0)

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
int mca_coll_ucc_req_start(size_t count, struct ompi_request_t **requests);

#endif
