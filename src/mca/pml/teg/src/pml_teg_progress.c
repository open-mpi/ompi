#include "pml_teg.h"
#include "pml_teg_sendreq.h"


int mca_pml_teg_progress(void)
{
    mca_ptl_tstamp_t tstamp = 0;
    size_t i;

    /*
     * Progress each of the PTL modules
     */
    for(i=0; i<mca_pml_teg.teg_num_ptl_modules; i++) {
        mca_ptl_base_module_progress_fn_t progress = mca_pml_teg.teg_ptl_modules[i]->ptlm_progress;
        if(NULL != progress)
            progress(tstamp);
    }
    return OMPI_SUCCESS;
}

