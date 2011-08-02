/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "mtl_mxm.h"
#include "mtl_mxm_request.h"

int ompi_mtl_mxm_cancel(struct mca_mtl_base_module_t* mtl,
                        struct mca_mtl_request_t *mtl_request, int flag)
{

    mxm_error_t err;
    mca_mtl_mxm_request_t *mtl_mxm_request = (mca_mtl_mxm_request_t*) mtl_request;

    err = mxm_req_cancel(mtl_mxm_request->mxm_base_request);
    if (MXM_OK == err) {
        err = mxm_req_test(mtl_mxm_request->mxm_base_request);
        if (MXM_OK == err) {
            mtl_request->ompi_req->req_status._cancelled = true;
            mtl_mxm_request->super.completion_callback(&mtl_mxm_request->super);
            return OMPI_SUCCESS;
        } else {
            return OMPI_ERROR;
        }
    } else if (MXM_ERR_NO_MESSAGE == err) {
        return OMPI_SUCCESS;
    }

    return OMPI_ERROR;
}
