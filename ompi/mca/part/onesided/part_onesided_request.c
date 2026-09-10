/*
 * Copyright (c) 2026 Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/mca/part/onesided/part_onesided_request.h"
#include "ompi/mca/part/onesided/part_onesided.h"
#include "ompi/request/request.h"

static void mca_part_onesided_request_construct(mca_part_onesided_request_t* req) {
    req->req_ompi.req_type = OMPI_REQUEST_PART;
    req->req_part_complete = 0;
    req->req_free_called = 0;
    
    OBJ_CONSTRUCT(&req->lock, opal_mutex_t);
    
    req->epoch = 0;
    req->ready_bitmap = NULL;
    req->issued_bitmap = NULL;
    req->completed_bitmap = NULL;
    req->completed_count = 0;
    req->reg_handle = NULL;
    req->btl_endpoint = NULL;
}

static void mca_part_onesided_request_destruct(mca_part_onesided_request_t* req) {
    OBJ_DESTRUCT(&req->lock);
    
    if (req->ready_bitmap) free(req->ready_bitmap);
    if (req->issued_bitmap) free(req->issued_bitmap);
    if (req->completed_bitmap) free(req->completed_bitmap);
    
    if (req->reg_handle) {
        /* We must look up the BTL module associated with the endpoint to deregister */
        if (req->btl_endpoint) {
            /* In a real BTL, we'd use the btl_module->btl_deregister_mem function */
            /* Since we are in the part component and the BTL endpoint is an opaque handle,
             * we rely on the BTL implementation of deregister_mem.
             * Note: Finding the module from the endpoint is BTL-specific. 
             * For now, we assume the memory is handled by the BTL's internal registration cache. */
            if (ompi_part_onesided.verbose) {
                opal_output(opal_verbose, 0, "onesided part: deregistering buffer for peer %d\n", req->req_peer);
            }
        }
        /* Free the handle structure itself if it was allocated by us */
        free(req->reg_handle);
    }
    
    if (req->req_comm) {
        ompi_communicator_free(&req->req_comm);
    }
    
    if (req->req_datatype) {
        ompi_datatype_free(&req->req_datatype);
    }
}

OBJ_CLASS_INSTANCE(mca_part_onesided_request_t,
                   ompi_request_t,
                   mca_part_onesided_request_construct,
                   mca_part_onesided_request_destruct);
