/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "class/ompi_object.h"
#include "file/file.h"
#include "mca/io/base/io_base_request.h"


/*
 * Private functions
 */
static void io_base_request_constructor(mca_io_base_request_t *req);


OBJ_CLASS_INSTANCE(mca_io_base_request_t,
                   ompi_request_t,
                   io_base_request_constructor,
                   NULL);


static void io_base_request_constructor(mca_io_base_request_t *req)
{
    req->req_ompi.req_type = OMPI_REQUEST_IO;
}


/*
 * Return a module-specific IO MPI_Request
 */
int mca_io_base_request_alloc(ompi_file_t *file, 
                              mca_io_base_request_t **req)
{
    int err;
    size_t extra;
    mca_io_base_module_request_init_fn_t func;

    /* JMS For the moment, no freelisting */

    switch (file->f_io_version) {
    case MCA_IO_BASE_V_1_0_0:
        extra = file->f_io_selected_module.v1_0_0.io_module_cache_bytes;
        func = file->f_io_selected_module.v1_0_0.io_module_request_init;
        break;

    default:
        extra = 0;
        func = NULL;
        break;
    }

    /* Malloc out enough space */

    *req = malloc(sizeof(mca_io_base_request_t) + extra);
    if (NULL == *req) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Construct the object and call the module's request init
       function, if it exists */

    OBJ_CONSTRUCT(req, mca_io_base_request_t);
    if (NULL != func) {
        if (OMPI_SUCCESS != (err = func(&file->f_io_selected_module, *req))) {
            OBJ_RELEASE(*req);
            return err;
        }
    }

    return OMPI_SUCCESS;
}


/*
 * Free a module-specific IO MPI_Request
 */
void mca_io_base_request_free(ompi_file_t *file,
                              mca_io_base_request_t **req)
{
    mca_io_base_module_request_finalize_fn_t func;

    /* JMS For the moment, no freelisting */

    switch (file->f_io_version) {
    case MCA_IO_BASE_V_1_0_0:
        func = file->f_io_selected_module.v1_0_0.io_module_request_finalize;
        if (NULL != func) {
            func(&file->f_io_selected_module, *req);
        }
        break;

    default:
        break;
    }

    OBJ_RELEASE(*req);
    *req = (mca_io_base_request_t*) MPI_REQUEST_NULL;
}

