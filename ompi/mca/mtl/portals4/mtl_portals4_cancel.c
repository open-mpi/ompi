/*
 * Copyright (c) 2012      Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <portals4.h>

#include "mtl_portals4.h"
#include "mtl_portals4_request.h"


int
ompi_mtl_portals4_cancel(struct mca_mtl_base_module_t* mtl,
                         mca_mtl_request_t *mtl_request,
                         int flag)
{
    ompi_mtl_portals4_base_request_t *base_request = 
        (ompi_mtl_portals4_base_request_t*) mtl_request;
    int ret;

    switch (base_request->type) {
    case portals4_req_isend:
        /* can't cancel sends yet */
        break;

    case portals4_req_recv:
        {
            ompi_mtl_portals4_recv_request_t *recvreq = 
                (ompi_mtl_portals4_recv_request_t*) base_request;

            /* Cancel receive requests if not yet matched (otherwise,
               they are guaranteed to complete and don't need to be
               cancelled).  If the me_h is already INVALID, that means
               that not only has matching occurred, but the
               communication end event has been seen.  If MEUnlink
               fails, that means that either something bad has
               happened or the ME is in use (meaning no cancel).  Need
               to drain queue to make sure there isn't a pending
               receive completion event... */
            ompi_mtl_portals4_progress();

            if (PTL_INVALID_HANDLE != recvreq->me_h) {
                ret = PtlMEUnlink(recvreq->me_h);
                if (OPAL_UNLIKELY(PTL_OK == ret)) {
                    recvreq->super.super.ompi_req->req_status._cancelled = true;
                    recvreq->super.super.completion_callback(&recvreq->super.super);
                }
            }
        }
        break;

    default:
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


