#include "pml_teg.h"
#include "mca/ptl/base/ptl_base_comm.h"
#include "mca/pml/base/pml_base_request.h"
#include "mca/ptl/base/ptl_base_sendreq.h"



int mca_pml_teg_free(lam_request_t** request)
{
    mca_pml_base_request_t* pml_request = *(mca_pml_base_request_t**)request;
    pml_request->req_free_called = true;
    if(pml_request->req_pml_done == true) 
    {
        switch(pml_request->req_type) {
        case MCA_PML_REQUEST_SEND:
            {
            mca_ptl_base_send_request_t* sendreq = (mca_ptl_base_send_request_t*)pml_request;
            mca_ptl_t* ptl = sendreq->req_owner;
            ptl->ptl_request_return(ptl, sendreq);
            break;
            }
        case MCA_PML_REQUEST_RECV:
            {
            lam_free_list_return(&mca_pml_teg.teg_recv_requests, (lam_list_item_t*)pml_request);
            break;
            }
        default:
            break;
        }
    }
    *request = NULL;
    return LAM_SUCCESS;
}

