#include "pml_teg.h"
#include "mca/ptl/base/ptl_base_comm.h"
#include "mca/pml/base/pml_base_request.h"
#include "mca/ptl/base/ptl_base_sendreq.h"


int mca_pml_teg_free(lam_request_t** request)
{
    MCA_PML_TEG_FREE(request);
    return LAM_SUCCESS;
}

