#include "pml_teg.h"
#include "pml_teg_sendreq.h"


int mca_pml_teg_progress(void)
{
    mca_ptl_base_tstamp_t tstamp;
    size_t i;

    /*
     * Progress each of the PTL modules
     */
    for(i=0; i<mca_pml_teg.teg_num_ptl_modules; i++)
        mca_pml_teg.teg_ptl_modules[i]->ptlm_progress(tstamp);

    /*
     * Complete any pending send requests. 
     */
    THREAD_LOCK(&mca_pml_teg.teg_lock);
    mca_ptl_base_send_request_t* req;
    for(req =  (mca_ptl_base_send_request_t*)lam_list_get_first(&mca_pml_teg.teg_incomplete_sends);
        req != (mca_ptl_base_send_request_t*)lam_list_get_end(&mca_pml_teg.teg_incomplete_sends);
        req =  (mca_ptl_base_send_request_t*)lam_list_get_next(req)) {
                                                                                                         
        bool complete;
        int rc = mca_pml_teg_send_request_schedule(req, &complete);
        if(rc != LAM_SUCCESS) {
             continue;
        }
        if(complete) {
            req = (mca_ptl_base_send_request_t*)lam_list_remove_item(
                &mca_pml_teg.teg_incomplete_sends, (lam_list_item_t*)req);
        }
    }
    THREAD_UNLOCK(&mca_pml_teg.teg_lock);
    return LAM_SUCCESS;
}

