/*
 * $HEADER$
 */
#include <unistd.h>
#include <sys/types.h>
#include <sys/errno.h>
#include "types.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "ptl_elan_req.h"
#include "ptl_elan.h"

void
mca_ptl_elan_send_request_construct (mca_ptl_elan_send_request_t * request)
{
    OBJ_CONSTRUCT (&request->super, mca_pml_base_send_request_t);
    request->req_frag = NULL;
}


void
mca_ptl_elan_send_request_destruct (mca_ptl_elan_send_request_t * request)
{
    OBJ_DESTRUCT (&request->super);
    request->req_frag = NULL;
}

ompi_class_t mca_ptl_elan_send_request_t_class = {
    "mca_ptl_elan_send_request_t",
    OBJ_CLASS (mca_pml_base_send_request_t),
    (ompi_construct_t) mca_ptl_elan_send_request_construct,
    (ompi_destruct_t) mca_ptl_elan_send_request_destruct
};
