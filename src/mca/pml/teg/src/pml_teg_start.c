#include "pml_teg.h"
#include "pml_teg_recvreq.h"
#include "pml_teg_sendreq.h"


int mca_pml_teg_start(size_t count, ompi_request_t** requests)
{
    int rc;
    size_t i;
    for(i=0; i<count; i++) {
        mca_pml_base_request_t *pml_request = (mca_pml_base_request_t*)requests[i];
        if(NULL == pml_request)
            continue;

        switch(pml_request->req_type) {
            case MCA_PML_REQUEST_SEND:
                if((rc = mca_pml_teg_send_request_start((mca_ptl_base_send_request_t*)pml_request)) 
                    != OMPI_SUCCESS)
                    return rc;
                break;
            case MCA_PML_REQUEST_RECV:
                if((rc = mca_pml_teg_recv_request_start((mca_ptl_base_recv_request_t*)pml_request)) 
                    != OMPI_SUCCESS)
                    return rc;
                break;
            default:
                return OMPI_ERROR;
        }
    }
    return OMPI_SUCCESS;
}

