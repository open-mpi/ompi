/*
 * $HEADER$
 */
#include "ompi_config.h"
#include <unistd.h>
#include <sys/types.h>
#include <sys/errno.h>
#include "include/types.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "ptl_tcp.h"
#include "ptl_tcp_sendreq.h"


static void mca_ptl_tcp_send_request_construct(mca_ptl_tcp_send_request_t*);
static void mca_ptl_tcp_send_request_destruct(mca_ptl_tcp_send_request_t*);
                                                                                                                

ompi_class_t  mca_ptl_tcp_send_request_t_class = {
    "mca_ptl_tcp_send_request_t",
    OBJ_CLASS(mca_pml_base_send_request_t),
    (ompi_construct_t)mca_ptl_tcp_send_request_construct,
    (ompi_destruct_t)mca_ptl_tcp_send_request_destruct
};


void mca_ptl_tcp_send_request_construct(mca_ptl_tcp_send_request_t* request)
{
    OBJ_CONSTRUCT(&request->req_frag, mca_ptl_tcp_send_frag_t);
}


void mca_ptl_tcp_send_request_destruct(mca_ptl_tcp_send_request_t* request)
{
    OBJ_DESTRUCT(&request->req_frag);
}

