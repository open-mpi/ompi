/*
 * $HEADER$
 */
#include <unistd.h>
#include <sys/types.h>
#include <sys/errno.h>
#include "include/types.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "ptl_sm.h"
#include "ptl_sm_sendreq.h"


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
int mca_ptl_sm_send_request_init(mca_ptl_base_module_request_init_fn_t* request)
{

    int return_value=OMPI_SUCCESS;

    return return_value;
}

