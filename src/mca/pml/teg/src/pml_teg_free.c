/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "pml_teg.h"
#include "pml_teg_sendreq.h"
#include "pml_teg_recvreq.h"
#include "mca/pml/base/pml_base_request.h"


int mca_pml_teg_free(ompi_request_t** request)
{
    MCA_PML_TEG_FREE(request);
    return OMPI_SUCCESS;
}

