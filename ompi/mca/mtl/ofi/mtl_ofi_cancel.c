/*
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "mtl_ofi.h"
#include "mtl_ofi_types.h"
#include "mtl_ofi_request.h"


int
ompi_mtl_ofi_cancel(struct mca_mtl_base_module_t *mtl,
                    mca_mtl_request_t *mtl_request,
                    int flag)
{
    int ret;
    ompi_mtl_ofi_request_t *ofi_req = (ompi_mtl_ofi_request_t*) mtl_request;

    switch (ofi_req->type) {
        case OMPI_MTL_OFI_SEND:
            /**
             * Cannot cancel sends yet
             */
            break;

        case OMPI_MTL_OFI_RECV:
            /**
             * Cancel a receive request only if it hasn't been matched yet.
             * The event queue needs to be drained to make sure there isn't
             * any pending receive completion event.
             */
            ompi_mtl_ofi_progress();

            if (!ofi_req->req_started) {
                ret = fi_cancel((fid_t)ompi_mtl_ofi.ep, &ofi_req->ctx);
                if (0 == ret) {
                    /**
                     * The request was successfully cancelled.
                     */
                    ofi_req->super.ompi_req->req_status._cancelled = true;
                    ofi_req->super.completion_callback(&ofi_req->super);
                }
            }
            break;

        default:
            return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
