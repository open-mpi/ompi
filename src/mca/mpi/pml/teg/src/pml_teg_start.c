#include "pml_teg.h"
#include "pml_teg_recvreq.h"
#include "pml_teg_sendreq.h"


int mca_pml_teg_start(lam_request_t* request)
{
    switch(request->req_type) {
        case MCA_PML_REQUEST_SEND:
            return mca_pml_teg_send_request_start((mca_ptl_base_send_request_t*)request);
        case MCA_PML_REQUEST_RECV:
            return mca_pml_teg_recv_request_start((mca_ptl_base_recv_request_t*)request);
        default:
            return LAM_ERROR;
    }
}

