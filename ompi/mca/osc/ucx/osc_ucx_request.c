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
    ompi_osc_ucx_generic_request_t *request = (ompi_osc_ucx_generic_request_t*) *ompi_req;

    if (true != (bool)(request->super.super.req_complete)) {
        return MPI_ERR_REQUEST;
    }

    OMPI_OSC_UCX_REQUEST_RETURN(request);

    *ompi_req = MPI_REQUEST_NULL;

    return OMPI_SUCCESS;
}

static void request_construct(ompi_osc_ucx_generic_request_t *request)
{
    request->super.super.req_type = OMPI_REQUEST_WIN;
    request->super.super.req_status._cancelled = 0;
    request->super.super.req_free = request_free;
    request->super.super.req_cancel = request_cancel;
}

OBJ_CLASS_INSTANCE(ompi_osc_ucx_generic_request_t, ompi_request_t,
                   request_construct, NULL);
OBJ_CLASS_INSTANCE(ompi_osc_ucx_accumulate_request_t, ompi_request_t,
                   request_construct, NULL);
