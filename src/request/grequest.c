/*
 * $HEADER$
 */

#include "ompi_config.h"
#include "request/grequest.h"


static int ompi_grequest_free(ompi_request_t** req)
{
    int rc = OMPI_SUCCESS;
    ompi_grequest_t* greq = *(ompi_grequest_t**)req;
    if(greq->greq_free != NULL)
        rc = greq->greq_free(greq->greq_state);
    if(rc == OMPI_SUCCESS) {
        OBJ_RELEASE(greq);
        *req = MPI_REQUEST_NULL;
    }
    return rc;
}

static int ompi_grequest_cancel(ompi_request_t* req, int flag)
{
    int rc = OMPI_SUCCESS;
    ompi_grequest_t* greq = (ompi_grequest_t*)req;
    if(greq->greq_cancel != NULL)
        rc = greq->greq_cancel(greq->greq_state, flag);
    return rc;
}

static void ompi_grequest_construct(ompi_grequest_t* greq)
{
    OMPI_REQUEST_INIT(&greq->greq_base);
    greq->greq_base.req_fini = ompi_grequest_free;
    greq->greq_base.req_free = ompi_grequest_free;
    greq->greq_base.req_cancel = ompi_grequest_cancel;
    greq->greq_base.req_type = OMPI_REQUEST_GEN;
}
                                                                                                                         
                                                                                                                         
static void ompi_grequest_destruct(ompi_grequest_t* greq)
{
    OMPI_REQUEST_FINI(&greq->greq_base);
}


OBJ_CLASS_INSTANCE(
    ompi_grequest_t,
    ompi_request_t,
    ompi_grequest_construct,
    ompi_grequest_destruct);


int ompi_grequest_start(
    MPI_Grequest_query_function *gquery_fn,
    MPI_Grequest_free_function *gfree_fn,
    MPI_Grequest_cancel_function *gcancel_fn,
    void* gstate,
    ompi_request_t** request)
{
    ompi_grequest_t *greq = OBJ_NEW(ompi_grequest_t);
    if(greq == NULL) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    greq->greq_state = gstate;
    greq->greq_query = gquery_fn;
    greq->greq_free = gfree_fn;
    greq->greq_cancel = gcancel_fn; 
    *request = &greq->greq_base;
    return OMPI_SUCCESS;
}

int ompi_grequest_complete(ompi_grequest_t* grequest)
{
    int rc = OMPI_SUCCESS;
    OMPI_THREAD_LOCK(&ompi_request_lock);
    grequest->greq_base.req_complete = true;
    if(grequest->greq_query != NULL)
        rc = grequest->greq_query(grequest->greq_state, &grequest->greq_base.req_status);
    if(ompi_request_waiting)
        ompi_condition_signal(&ompi_request_cond);
    OMPI_THREAD_UNLOCK(&ompi_request_lock);
    return rc;
}

