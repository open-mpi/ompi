#include "ompi_config.h"
#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include "svc_exec.h"


mca_svc_base_module_t mca_svc_exec_module = {
    mca_svc_exec_module_init,
    mca_svc_exec_module_fini
};


/**
 *  Process an OOB request.
 */

static void mca_svc_exec_recv(
    int status,
    ompi_process_name_t* peer,
    ompi_buffer_t buffer,
    int tag,
    void* cbdata)
{
    /* unpack message */
    
}


/**
 * Register a callback to receive OOB requests.
 */

int mca_svc_exec_module_init(mca_svc_base_module_t* module)
{
    return mca_oob_recv_packed_nb(
        MCA_OOB_NAME_ANY,  
        MCA_OOB_TAG_EXEC, 
        MCA_OOB_ALLOC, 
        mca_svc_exec_recv,
        NULL);
}


/**
 *  Cleanup
 */

int mca_svc_exec_module_fini(mca_svc_base_module_t* module)
{
    return OMPI_SUCCESS;
}

