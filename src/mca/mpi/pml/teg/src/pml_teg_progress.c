#include "pml_teg.h"
#include "pml_teg_sendreq.h"


int mca_pml_teg_progress(void)
{
    THREAD_LOCK(&mca_pml_teg.teg_lock);
    mca_ptl_base_send_request_t* req;
    for(req =  (mca_ptl_base_send_request_t*)lam_list_get_first(&mca_pml_teg.teg_incomplete_sends);
        req != (mca_ptl_base_send_request_t*)lam_list_get_end(&mca_pml_teg.teg_incomplete_sends);
        req =  (mca_ptl_base_send_request_t*)lam_list_get_next(req)) {
                                                                                                         
        bool complete;
        int rc = mca_pml_teg_send_request_schedule(req, &complete);
        if(rc != LAM_SUCCESS) {
             THREAD_UNLOCK(&mca_pml_teg.teg_lock);
             return rc;
        }
        if(complete) {
            req = (mca_ptl_base_send_request_t*)lam_list_remove(
                &mca_pml_teg.teg_incomplete_sends, (lam_list_item_t*)req);
        }
    }
    THREAD_UNLOCK(&mca_pml_teg.teg_lock);
    return LAM_SUCCESS;
}

