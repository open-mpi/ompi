/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include <unistd.h>
#include <sys/types.h>
#include <sys/errno.h>
#include "include/types.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "ptl_sm.h"
#include "ptl_sm_sendreq.h"
#include "ptl_sm_address.h"


static void mca_ptl_sm_send_request_construct(mca_ptl_sm_send_request_t*);
static void mca_ptl_sm_send_request_destruct(mca_ptl_sm_send_request_t*);
                                                                                                                

OBJ_CLASS_INSTANCE(
    mca_ptl_sm_send_request_t,
    mca_pml_base_send_request_t,
    mca_ptl_sm_send_request_construct,
    mca_ptl_sm_send_request_destruct
);


/* constructor for the shared memory send descriptor */
void mca_ptl_sm_send_request_construct(mca_ptl_sm_send_request_t* request)
{
    OBJ_CONSTRUCT(&request->req_frag, mca_ptl_sm_frag_t);
}


/* desnstructor for the shared memory send descriptor */
void mca_ptl_sm_send_request_destruct(mca_ptl_sm_send_request_t* request)
{
    OBJ_DESTRUCT(&request->req_frag);
}

/* initializtion function to be called when a new shared
 * memory send request is initialized.  This will attempt
 * to allocate fragment descriptor and payload memory
 */
int mca_ptl_sm_send_request_init(struct mca_ptl_base_module_t* ptl,
        struct mca_pml_base_send_request_t* request)
{

    mca_ptl_sm_send_request_t *sm_request;
    mca_ptl_sm_t *ptl_sm;

    int return_value=OMPI_SUCCESS;

    /* cast to shared memory send descriptor */
    sm_request=(mca_ptl_sm_send_request_t *)request;

    /* cast to shared memory ptl */
    ptl_sm=(mca_ptl_sm_t *)ptl;

    /* get first fragment descritor from free list - the pointer
     * returned is valid only in this process, since different
     * processes may have different base addresses 
     */
    sm_request->req_frag=(mca_ptl_sm_frag_t *)ompi_list_get_first(
            (void *)&(mca_ptl_sm_component.sm_first_frags));
    if(NULL == sm_request->req_frag){
        return_value=OMPI_ERR_OUT_OF_RESOURCE;
    }

    return return_value;
}

