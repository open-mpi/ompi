/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "class/ompi_object.h"
#include "request/request.h"
#include "include/constants.h"

ompi_pointer_array_t  ompi_request_f_to_c_table;
volatile int          ompi_request_waiting = 0;
int                   ompi_request_poll_iterations = 20000;
ompi_mutex_t          ompi_request_lock;
ompi_condition_t      ompi_request_cond;
ompi_request_t        ompi_request_null;


static void ompi_request_construct(ompi_request_t* req)
{
    OMPI_REQUEST_INIT(req);
    req->req_query = NULL;
    req->req_fini = NULL;
    req->req_free = NULL;
    req->req_cancel = NULL;
}

static void ompi_request_destruct(ompi_request_t* req)
{
    OMPI_REQUEST_FINI(req);
}

static int ompi_request_null_free(ompi_request_t** request)
{
    return OMPI_SUCCESS;
}

static int ompi_request_null_cancel(ompi_request_t* request, int flag)
{
    return OMPI_SUCCESS;
}


OBJ_CLASS_INSTANCE(
    ompi_request_t,
    ompi_object_t,
    ompi_request_construct,
    ompi_request_destruct);


int ompi_request_init(void)
{
    OBJ_CONSTRUCT(&ompi_request_f_to_c_table, ompi_pointer_array_t);
    OBJ_CONSTRUCT(&ompi_request_lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&ompi_request_cond, ompi_condition_t);
    OBJ_CONSTRUCT(&ompi_request_null, ompi_request_t);

    ompi_request_null.req_status.MPI_SOURCE = MPI_PROC_NULL;
    ompi_request_null.req_status.MPI_TAG = MPI_ANY_TAG;
    ompi_request_null.req_status.MPI_ERROR = MPI_SUCCESS;
    ompi_request_null.req_status._count = 0;

    ompi_request_null.req_complete = true;
    ompi_request_null.req_type = OMPI_REQUEST_NULL;
    ompi_request_null.req_query = NULL;
    ompi_request_null.req_fini = ompi_request_null_free;
    ompi_request_null.req_free = ompi_request_null_free;
    ompi_request_null.req_cancel = ompi_request_null_cancel;

    if (0 != ompi_pointer_array_add(&ompi_request_f_to_c_table, 
                                    MPI_REQUEST_NULL)) {
        return OMPI_ERR_REQUEST;
    }
    return OMPI_SUCCESS;
}


int ompi_request_finalize(void)
{
    OBJ_DESTRUCT(&ompi_request_null);
    OBJ_DESTRUCT(&ompi_request_cond);
    OBJ_DESTRUCT(&ompi_request_lock);
    OBJ_DESTRUCT(&ompi_request_f_to_c_table);
    return OMPI_SUCCESS;
}


int ompi_request_complete(ompi_request_t* request)
{
    OMPI_THREAD_LOCK(&ompi_request_lock);
    request->req_complete = true;
    if (request->req_query != NULL) {
        int rc = request->req_query(request, &request->req_status);
        if(rc != OMPI_SUCCESS) {
            OMPI_THREAD_UNLOCK(&ompi_request_lock);
            return rc;
        }
    }
    if(ompi_request_waiting)
        ompi_condition_signal(&ompi_request_cond);
    OMPI_THREAD_UNLOCK(&ompi_request_lock);
    return OMPI_SUCCESS;
}

