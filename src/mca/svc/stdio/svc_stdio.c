#include "ompi_config.h"
#include "include/constants.h"
#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include "svc_stdio.h"


mca_svc_base_module_t mca_svc_stdio_module = {
    mca_svc_stdio_module_init,
    mca_svc_stdio_module_fini
};


/**
 *  Process an OOB request.
 */

static void mca_svc_stdio_recv(
    int status,
    ompi_process_name_t* peer,
    ompi_buffer_t buffer,
    int tag,
    void* cbdata)
{
}


/**
 * Register a callback to receive OOB requests.
 */

int mca_svc_stdio_module_init(mca_svc_base_module_t* module)
{
    return mca_oob_recv_packed_nb(
        MCA_OOB_NAME_ANY,  
        MCA_OOB_TAG_STDIO, 
        MCA_OOB_ALLOC, 
        mca_svc_stdio_recv,
        NULL);
}


/**
 *  Cleanup
 */

int mca_svc_stdio_module_fini(mca_svc_base_module_t* module)
{
    return OMPI_SUCCESS;
}

