/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2014 University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "io_ompio_request.h"

static void mca_io_ompio_request_construct(mca_ompio_request_t* req);
static void mca_io_ompio_request_destruct(mca_ompio_request_t *req);

bool mca_io_ompio_progress_is_registered=false;

static int mca_io_ompio_request_free ( struct ompi_request_t **req)
{
    mca_ompio_request_t *ompio_req = ( mca_ompio_request_t *)*req;
    if ( NULL != ompio_req->req_free_fn ) {
	ompio_req->req_free_fn (ompio_req );
    }
    opal_list_remove_item (&mca_io_ompio_pending_requests, &ompio_req->req_item);

    OBJ_RELEASE (*req);
    return OMPI_SUCCESS;
}

static int mca_io_ompio_request_cancel ( struct ompi_request_t *req, int flag)
{
    return OMPI_SUCCESS;
}

OBJ_CLASS_INSTANCE(mca_ompio_request_t, ompi_request_t,
                   mca_io_ompio_request_construct,
                   mca_io_ompio_request_destruct);

void mca_io_ompio_request_construct(mca_ompio_request_t* req)
{
    OMPI_REQUEST_INIT (&(req->req_ompi), false );
    req->req_ompi.req_free   = mca_io_ompio_request_free;
    req->req_ompi.req_cancel = mca_io_ompio_request_cancel;
    req->req_ompi.req_type   = OMPI_REQUEST_IO;
    req->req_data            = NULL;
    req->req_progress_fn     = NULL;
    req->req_free_fn         = NULL;

    OBJ_CONSTRUCT(&req->req_item, opal_list_item_t);
    opal_list_append (&mca_io_ompio_pending_requests, &req->req_item);
    return;
}
void mca_io_ompio_request_destruct(mca_ompio_request_t* req)
{
    OMPI_REQUEST_FINI ( &(req->req_ompi));
    OBJ_DESTRUCT (&req->req_item);
    if ( NULL != req->req_data ) {
	free (req->req_data);
    }

    return;
}

int mca_io_ompio_component_progress ( void )
{
    mca_ompio_request_t *req=NULL;
    opal_list_item_t *litem=NULL;
    int completed=0;

    OPAL_LIST_FOREACH(litem, &mca_io_ompio_pending_requests, opal_list_item_t) {
        req = GET_OMPIO_REQ_FROM_ITEM(litem);
	if ( true == req->req_ompi.req_complete ) {
	    continue;
	}
        if ( NULL != req->req_progress_fn ) {
            if ( req->req_progress_fn(req) ) {
                completed++;
                ompi_request_complete (&req->req_ompi, 1);
                /* The fbtl progress function is expected to set the 
		** status elements 
		*/
            }
        }
        
    }

    return completed;
}
