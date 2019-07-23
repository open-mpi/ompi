/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2017. ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/request/request.h"
#include "ompi/mca/osc/osc.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/osc/base/osc_base_obj_convert.h"

#include "osc_ucx.h"
#include "osc_ucx_request.h"

static int request_cancel(struct ompi_request_t *request, int complete)
{
    return MPI_ERR_REQUEST;
}

static int request_free(struct ompi_request_t **ompi_req)
{
    ompi_osc_ucx_request_t *request = (ompi_osc_ucx_request_t*) *ompi_req;

    if (true != (bool)(request->super.req_complete)) {
        return MPI_ERR_REQUEST;
    }

    OMPI_OSC_UCX_REQUEST_RETURN(request);

    *ompi_req = MPI_REQUEST_NULL;

    return OMPI_SUCCESS;
}

static void request_construct(ompi_osc_ucx_request_t *request)
{
    request->super.req_type = OMPI_REQUEST_WIN;
    request->super.req_status._cancelled = 0;
    request->super.req_free = request_free;
    request->super.req_cancel = request_cancel;
}

void internal_req_init(void *request) {
    ompi_osc_ucx_internal_request_t *req = (ompi_osc_ucx_internal_request_t *)request;
    req->external_req = NULL;
}

void req_completion(void *request, ucs_status_t status) {
    ompi_osc_ucx_internal_request_t *req = (ompi_osc_ucx_internal_request_t *)request;

    if(req->external_req != NULL) {
        ompi_request_complete(&(req->external_req->super), true);
        req->external_req = NULL;
        ucp_request_release(req);
        mca_osc_ucx_component.num_incomplete_req_ops--;
        assert(mca_osc_ucx_component.num_incomplete_req_ops >= 0);
    }
}

OBJ_CLASS_INSTANCE(ompi_osc_ucx_request_t, ompi_request_t,
                   request_construct, NULL);
