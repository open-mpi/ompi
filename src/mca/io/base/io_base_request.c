/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "class/ompi_object.h"
#include "file/file.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/io/base/base.h"
#include "mca/io/base/io_base_request.h"


/*
 * Public variables
 */
bool mca_io_base_requests_valid = false;
ompi_free_list_t mca_io_base_requests;


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
    req->super.req_type = OMPI_REQUEST_IO;
}


/*
 * Setup the freelist of IO requests.  This does not need to be
 * protected with a lock because it's called during MPI_INIT.
 */
int mca_io_base_request_create_freelist(void)
{
    ompi_list_item_t *p;
    const mca_base_component_t *component;
    const mca_io_base_component_1_0_0_t *v100;
    size_t size = 0;
    int i, init, incr;

    /* Find the maximum additional number of bytes required by all io
       components for requests and make that the request size */

    for (p = ompi_list_get_first(&mca_io_base_components_available); 
         p != ompi_list_get_end(&mca_io_base_components_available); 
         p = ompi_list_get_next(p)) {
        component = ((mca_base_component_priority_list_item_t *) 
                     p)->super.cli_component;

        /* Only know how to handle v1.0.0 components for now */

        if (component->mca_type_major_version == 1 &&
            component->mca_type_minor_version == 0 &&
            component->mca_type_release_version == 0) {
            v100 = (mca_io_base_component_1_0_0_t *) component;
            if (v100->io_request_bytes > size) {
                size = v100->io_request_bytes;
            }
        }
    }

    /* Construct and initialized the freelist of IO requests. */

    OBJ_CONSTRUCT(&mca_io_base_requests, ompi_free_list_t);
    mca_io_base_requests_valid = true;
    i = mca_base_param_find("io", "base", "freelist_initial_size");
    mca_base_param_lookup_int(i, &init);
    i = mca_base_param_find("io", "base", "freelist_increment");
    mca_base_param_lookup_int(i, &incr);

    ompi_free_list_init(&mca_io_base_requests,
                        sizeof(mca_io_base_request_t) + size,
                        OBJ_CLASS(mca_io_base_request_t),
                        init, -1, incr,
                        NULL);

    /* All done */

    return OMPI_SUCCESS;
}


/*
 * Return a module-specific IO MPI_Request
 */
int mca_io_base_request_alloc(ompi_file_t *file, 
                              mca_io_base_request_t **req)
{
    int err;
    mca_io_base_module_request_once_init_fn_t func;
    ompi_list_item_t *item;

    /* See if we've got a request on the module's freelist (which is
       cached on the file, since there's only one module per
       MPI_File).  Use a quick-but-not-entirely-accurate (but good
       enough) check as a slight optimization to potentially having to
       avoid locking and unlocking. */

    if (ompi_list_get_size(&file->f_io_requests) > 0) {
        OMPI_THREAD_LOCK(&file->f_io_requests_lock);
        *req = (mca_io_base_request_t*) 
            ompi_list_remove_first(&file->f_io_requests);
        OMPI_THREAD_UNLOCK(&file->f_io_requests_lock);
    } else {
        *req = NULL;
    }
        
    /* Nope, we didn't have one on the file freelist, so let's get one
       off the global freelist */

    if (NULL == *req) {
        OMPI_FREE_LIST_GET(&mca_io_base_requests, item, err);
        *req = (mca_io_base_request_t*) item;

        /* Call the per-use init function, if it exists */

        switch (file->f_io_version) {
        case MCA_IO_BASE_V_1_0_0:

            /* These can be set once for this request since this
               request will always be used with the same module (and
               therefore, the same MPI_File).  Note that
               (*req)->req_ompi.rq_type is already set by the
               constructor. */

            (*req)->req_file = file;
            (*req)->req_ver = file->f_io_version;
            (*req)->super.req_fini =
                file->f_io_selected_module.v1_0_0.io_module_request_fini;
            (*req)->super.req_free = 
                file->f_io_selected_module.v1_0_0.io_module_request_free;
            (*req)->super.req_cancel =
                file->f_io_selected_module.v1_0_0.io_module_request_cancel;

            /* Call the module's once-per process init, if it
               exists */

            func = 
                file->f_io_selected_module.v1_0_0.io_module_request_once_init;
            if (NULL != func) {
                if (OMPI_SUCCESS != 
                    (err = func(&file->f_io_selected_module, *req))) {
                    OMPI_FREE_LIST_RETURN(&mca_io_base_requests, item);
                    return err;
                }
            }

            break;
            
        default:
            OMPI_FREE_LIST_RETURN(&mca_io_base_requests, item);
            return OMPI_ERR_NOT_IMPLEMENTED;
            break;
        }
    }

    /* Initialize the request */

    OMPI_REQUEST_INIT(&((*req)->super));

    /* All done */

    return OMPI_SUCCESS;
}


/*
 * Free a module-specific IO MPI_Request
 */
void mca_io_base_request_free(ompi_file_t *file,
                              mca_io_base_request_t *req)
{
    /* Put the request back on the per-module freelist, since it's
       been initialized for that module */

    OMPI_THREAD_LOCK(&file->f_io_requests_lock);
    ompi_list_prepend(&file->f_io_requests, (ompi_list_item_t*) req);
    OMPI_THREAD_UNLOCK(&file->f_io_requests_lock);
}


/*
 * Return all the requests in the per-file freelist to the global list
 */
void mca_io_base_request_return(ompi_file_t *file)
{
    ompi_list_item_t *p, *next;

    OMPI_THREAD_LOCK(&file->f_io_requests_lock);
    for (p = ompi_list_get_first(&file->f_io_requests);
         p != ompi_list_get_end(&file->f_io_requests);
         p = next) {
        next = ompi_list_get_next(p);
        OMPI_FREE_LIST_RETURN(&mca_io_base_requests, p);
    }
    OMPI_THREAD_UNLOCK(&file->f_io_requests_lock);
}
