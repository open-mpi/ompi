/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "include/types.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "ptl_ib.h"
#include "ptl_ib_sendreq.h"

static void mca_ptl_ib_send_request_construct(mca_ptl_ib_send_request_t*);
static void mca_ptl_ib_send_request_destruct(mca_ptl_ib_send_request_t*);

OBJ_CLASS_INSTANCE(mca_ptl_ib_send_request_t, 
        mca_pml_base_send_request_t,
        mca_ptl_ib_send_request_construct, 
        mca_ptl_ib_send_request_destruct);

void mca_ptl_ib_send_request_construct(mca_ptl_ib_send_request_t* request)
{
    A_PRINT("Request Construct");
    request->req_frag = NULL;

    /*
    OBJ_CONSTRUCT(&request->req_frag, mca_ptl_ib_send_frag_t);
    */
}


void mca_ptl_ib_send_request_destruct(mca_ptl_ib_send_request_t* request)
{
    D_PRINT("\n");

    /*
    OBJ_DESTRUCT(&request->req_frag);
    */
}
