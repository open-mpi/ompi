#include "pml_teg.h"
#include "mca/pml/base/pml_base_request.h"


int mca_pml_teg_free(ompi_request_t** request)
{
    MCA_PML_TEG_FREE(request);
    return OMPI_SUCCESS;
}

