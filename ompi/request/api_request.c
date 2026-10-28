/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#include "ompi_config.h"

#include "ompi/constants.h"
#include "ompi/request/request.h"

static void ompi_api_request_construct(ompi_api_request_t *request)
{
    request->lang_complete = NULL;
    request->lang_cleanup = NULL;
}

OBJ_CLASS_INSTANCE(ompi_api_request_t, ompi_request_t,
                   ompi_api_request_construct, NULL);

int ompi_api_request_set_lang_hooks(ompi_request_t *req,
                                    ompi_api_request_hook_fn_t complete,
                                    ompi_api_request_hook_fn_t cleanup)
{
    ompi_api_request_t *request = (ompi_api_request_t *) req;

    if (!ompi_api_request_is(req)) {
        return MPI_ERR_REQUEST;
    }

    request->lang_cleanup = cleanup;

    /* The request may already be running, so publish the completion hook
     * last: the moment it is visible the request may be observed complete,
     * and the hook needs the payload the caller parked for it. */
    opal_atomic_wmb ();
    request->lang_complete = complete;

    return OMPI_SUCCESS;
}

void ompi_api_request_complete(ompi_api_request_t *request)
{
    if (NULL == request->lang_complete) {
        return;
    }

    /* Once per activation.  MPI_Testall over an array that names the same
     * request twice, or a test that returns true followed by a wait, must
     * not convert twice.  A non-persistent request is done for good, so
     * leaving it inactive here is enough; a persistent one is reactivated by
     * MPI_Start, which is what makes this per activation rather than per
     * request. */
    if (OMPI_REQUEST_ACTIVE != request->super.req_state) {
        return;
    }
    if (!request->super.req_persistent) {
        request->super.req_state = OMPI_REQUEST_INACTIVE;
    }

    request->lang_complete(request);
}

void ompi_api_request_dispose(ompi_api_request_t *request)
{
    if (NULL != request->lang_cleanup) {
        request->lang_cleanup(request);
        request->lang_cleanup = NULL;
    }
    request->lang_complete = NULL;
}
