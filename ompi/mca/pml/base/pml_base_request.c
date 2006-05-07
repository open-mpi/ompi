/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/pml_base_request.h"

static void mca_pml_base_request_construct(mca_pml_base_request_t* req)
{
    req->req_ompi.req_type = OMPI_REQUEST_PML;
}

static void mca_pml_base_request_destruct(mca_pml_base_request_t* req)
{
}

opal_class_t mca_pml_base_request_t_class = { 
    "mca_pml_base_request_t", 
    OBJ_CLASS(ompi_request_t),
    (opal_construct_t) mca_pml_base_request_construct, 
    (opal_destruct_t) mca_pml_base_request_destruct 
};

typedef struct {
    opal_list_item_t* last_memory;
    opal_list_item_t* last_request;
} ompi_debug_request_pos_t;

int mca_pml_base_get_next_request( ompi_free_list_t* list,
                                   struct ompi_free_list_pos_t* position,
                                   ompi_request_state_t req_state,
                                   mca_pml_base_request_t** base_req )
{
    opal_list_item_t* item;

 dig_for_the_requests:
    (void)ompi_free_list_parse( list, position, &item );
    if( item == NULL ) {
        *base_req = NULL;
        return 0;
    }
    *base_req = (mca_pml_base_request_t*)item;
    /* Now that we have a pointer to a request, let's find if the request
     * match the user requirements.
     */
    if( (*base_req)->req_ompi.req_state == OMPI_REQUEST_INVALID )
        goto dig_for_the_requests;

    /* If the req_state is set to INVALID, then the user does not care about
     * the status of the request (except it should be valid !!!)
     */
    if( req_state == OMPI_REQUEST_INVALID )
        return 0;

    if( (*base_req)->req_ompi.req_state != req_state )
        goto dig_for_the_requests;

    return 0;
}
