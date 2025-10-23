/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2025 Bull SAS.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <stddef.h>
#include "ompi/mca/osc/ubcl/osc_ubcl.h"
#include "ompi/mca/osc/ubcl/osc_ubcl_request.h"
#include "ompi/mca/osc/ubcl/osc_ubcl_utils.h"
#include "ompi/mca/common/ubcl/common_ubcl.h"

#define container_of(ptr, type, member) ((type *) ((char *) (ptr) -offsetof(type, member)))

/* Should be filtered out by MPI_Start based on request->type, but maybe not
 * by MPI_Startall */
static int osc_ubcl_request_start(size_t count, struct ompi_request_t **requests)
{
    (void) count;
    (void) requests;
    return MPI_ERR_REQUEST;
}

static int osc_ubcl_request_free(struct ompi_request_t **request)
{
    OPAL_OUTPUT_VERBOSE((50, mca_osc_ubcl_component.output,
                         "OSC/UBCL REQUEST_FINALIZE BEGIN osc_req=%p\n",
                         request));
    mca_osc_ubcl_request_t *req;
    opal_free_list_item_t * item;

    req = container_of((*request), mca_osc_ubcl_request_t, ompi_req);
    item = (opal_free_list_item_t *) req;

    if (!REQUEST_COMPLETE(&(req)->ompi_req)) {
        abort();
    }

    *request = MPI_REQUEST_NULL;
    opal_free_list_return(&mca_osc_ubcl_component.req_free_list, item);

    return OMPI_SUCCESS;
}

/* Cannot cancel osc requests */
static int osc_ubcl_request_cancel(struct ompi_request_t *request, int complete)
{
    (void) request;
    (void) complete;
    return MPI_ERR_REQUEST;
}

/* Called on free_list init during OBJ_CONSTRUCT */
static void osc_ubcl_request_construct(mca_osc_ubcl_request_t *request)
{
    request->ompi_req.req_type = OMPI_REQUEST_WIN;
    request->ompi_req.req_status._cancelled = 0;
    request->ompi_req.req_free = osc_ubcl_request_free;
    request->ompi_req.req_cancel = osc_ubcl_request_cancel;
    request->ompi_req.req_start = osc_ubcl_request_start;
}

/* callback privided to ubcl */
void ubcl_request_complete_cb(ubcl_status_t status, void *cb_data)
{
    ompi_request_t *request;
    mca_osc_ubcl_request_t *osc_req;
    mca_osc_ubcl_module_t *module;
    size_t segment_count;
    size_t segment_acked;

    osc_req = (mca_osc_ubcl_request_t *) cb_data;
    request = &osc_req->ompi_req;
    module = (mca_osc_ubcl_module_t *) osc_req->win->w_osc_module;

    OPAL_OUTPUT_VERBOSE((50, mca_osc_ubcl_component.output,
                         "OSC/UBCL DATA TRANSFER COMPLETE mpi_req=%p\n", request));

    mca_common_ubcl_status_to_ompi(&request->req_status, status, module->comm, -1);
    if (MPI_STATUS_IGNORE != &request->req_status) {
        request->req_status.MPI_ERROR = ubcl_error_to_ompi(status.status);
    }
    if (UBCL_SUCCESS != status.status) {
        mca_osc_ubcl_error(OMPI_ERROR, "UBCL error at request completion");
    }

    segment_count = osc_req->segment_count;
    segment_acked = opal_atomic_add_fetch_64((int64_t *) &osc_req->segment_ack, 1);

    if (segment_count == segment_acked) {
        MCA_OSC_UBCL_REQUEST_FINI(osc_req);
        ompi_request_complete(request, true);
        /* Free is called once the completed request is waited/tested
         * However this request comes from a non request-based call, then MPI_Wait will never be
         * called so osc_ubcl_request_free must be manually called here */
        if (!osc_req->is_request_based) {
            osc_ubcl_request_free(&request);
        }
    }
}

OBJ_CLASS_INSTANCE(mca_osc_ubcl_request_t,
                   opal_free_list_item_t,
                   osc_ubcl_request_construct,
                   NULL);
