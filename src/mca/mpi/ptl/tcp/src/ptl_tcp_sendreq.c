/*
 * $HEADER$
 */
#include <unistd.h>
#include <sys/types.h>
#include <sys/errno.h>
#include "lam/types.h"
#include "mca/mpi/ptl/base/ptl_base_sendreq.h"
#include "ptl_tcp.h"
#include "ptl_tcp_sendreq.h"


static void mca_ptl_tcp_send_request_init(mca_ptl_tcp_send_request_t*);
static void mca_ptl_tcp_send_request_destroy(mca_ptl_tcp_send_request_t*);
                                                                                                                

lam_class_info_t  mca_ptl_tcp_send_request_cls = {
    "mca_ptl_tcp_send_request_t",
    &mca_ptl_base_send_request_cls,
    (class_init_t)mca_ptl_tcp_send_request_init,
    (class_destroy_t)mca_ptl_tcp_send_request_destroy
};


void mca_ptl_tcp_send_request_init(mca_ptl_tcp_send_request_t* request)
{
    SUPER_INIT(request, &mca_ptl_base_send_request_cls);
    STATIC_INIT(request->req_frag, &mca_ptl_tcp_send_frag_cls);
}


void mca_ptl_tcp_send_request_destroy(mca_ptl_tcp_send_request_t* request)
{
    STATIC_DESTROY(request->req_frag);
    SUPER_DESTROY(request, &mca_ptl_base_send_request_cls);
}

