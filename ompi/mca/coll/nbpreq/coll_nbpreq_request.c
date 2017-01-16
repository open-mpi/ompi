/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2017      FUJITSU LIMITED.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_nbpreq.h"
#include "coll_nbpreq_request.h"
#include "ompi/mca/topo/topo.h"

static int request_complete(struct ompi_request_t* request);

// define `mca_coll_nbpreq_*_init` functions

#define DEFINE_INIT_FUNC(name_us_, name_lc_, name_uc_)                      \
    int mca_coll_nbpreq_ ## name_us_ ## _init(name_uc_ ## _INIT_ARGS)       \
    {                                                                       \
        int rc;                                                             \
        mca_coll_nbpreq_request_t *req;                                     \
        opal_free_list_t *free_requests =                                   \
            &mca_coll_nbpreq_component.free_requests;                       \
                                                                            \
        req = (mca_coll_nbpreq_request_t *)                                 \
              opal_free_list_wait(free_requests);                           \
        if (NULL == req) {                                                  \
            return OMPI_ERR_OUT_OF_RESOURCE;                                \
        }                                                                   \
                                                                            \
        OMPI_REQUEST_INIT(&req->super, true);                               \
        req->coll_type = name_uc_;                                          \
        req->comm = comm;                                                   \
        req->nb_request = NULL;                                             \
        save_func_args_ ## name_lc_(req, name_uc_ ##_BASE_ARG_NAMES);       \
                                                                            \
        *request = &req->super;                                             \
                                                                            \
        return OMPI_SUCCESS;                                                \
    }

DEFINE_INIT_FUNC(allgather,            allgather,           ALLGATHER)
DEFINE_INIT_FUNC(allgatherv,           allgatherv,          ALLGATHERV)
DEFINE_INIT_FUNC(allreduce,            allreduce,           ALLREDUCE)
DEFINE_INIT_FUNC(alltoall,             alltoall,            ALLTOALL)
DEFINE_INIT_FUNC(alltoallv,            alltoallv,           ALLTOALLV)
DEFINE_INIT_FUNC(alltoallw,            alltoallw,           ALLTOALLW)
DEFINE_INIT_FUNC(barrier,              barrier,             BARRIER)
DEFINE_INIT_FUNC(bcast,                bcast,               BCAST)
DEFINE_INIT_FUNC(exscan,               exscan,              EXSCAN)
DEFINE_INIT_FUNC(gather,               gather,              GATHER)
DEFINE_INIT_FUNC(gatherv,              gatherv,             GATHERV)
DEFINE_INIT_FUNC(reduce,               reduce,              REDUCE)
DEFINE_INIT_FUNC(reduce_scatter,       reducescatter,       REDUCESCATTER)
DEFINE_INIT_FUNC(reduce_scatter_block, reducescatterblock,  REDUCESCATTERBLOCK)
DEFINE_INIT_FUNC(scan,                 scan,                SCAN)
DEFINE_INIT_FUNC(scatter,              scatter,             SCATTER)
DEFINE_INIT_FUNC(scatterv,             scatterv,            SCATTERV)
DEFINE_INIT_FUNC(neighbor_allgather,   neighbor_allgather,  NEIGHBOR_ALLGATHER)
DEFINE_INIT_FUNC(neighbor_allgatherv,  neighbor_allgatherv, NEIGHBOR_ALLGATHERV)
DEFINE_INIT_FUNC(neighbor_alltoall,    neighbor_alltoall,   NEIGHBOR_ALLTOALL)
DEFINE_INIT_FUNC(neighbor_alltoallv,   neighbor_alltoallv,  NEIGHBOR_ALLTOALLV)
DEFINE_INIT_FUNC(neighbor_alltoallw,   neighbor_alltoallw,  NEIGHBOR_ALLTOALLW)

/**
 * Function called when `MPI_START` or `MPI_STARTALL` is called.
 *
 * @param[in]  count    Number of the persistent requests.
 * @param[in]  requests Array of persistent requests.
 * @return              OMPI_SUCCESS or failure status.
 */
static int request_start(size_t count, struct ompi_request_t **requests)
{
    int rc, i;
    mca_coll_nbpreq_request_t *req; // persistent communication request
    ompi_request_t *nb_req;         // nonblocking communication request

    for (i = 0; i < count; i++) {
        req = (mca_coll_nbpreq_request_t *)requests[i];

        // call the corresponding nonblocking communication function
        rc = call_nb_func(req);
        if (OMPI_SUCCESS != rc) {
            return rc;
        }

        // replace the `complete` callback function and its data of the
        // nonblocking communication request so that our callback function
        // is called when the nonblocking communication completes
        nb_req = req->nb_request;
        req->nb_req_complete_cb      = nb_req->req_complete_cb;
        req->nb_req_complete_cb_data = nb_req->req_complete_cb_data;
        nb_req->req_complete_cb      = request_complete;
        nb_req->req_complete_cb_data = req;

        req->super.req_state = OMPI_REQUEST_ACTIVE;
        req->super.req_complete = REQUEST_PENDING;
    }

    return OMPI_SUCCESS;
}

/**
 * Function called when the corresponding nonblocking communication completes.
 *
 * The `req_complete_cb` of the nonblocking communication request is set to
 * this function in the `request_start` function.
 *
 * @param[in]  request Request of the nonblocking communication.
 * @return             OMPI_SUCCESS or failure status.
 */
static int request_complete(struct ompi_request_t *request)
{
    int rc, i;
    ompi_request_t *nb_req = request;
    mca_coll_nbpreq_request_t *req = nb_req->req_complete_cb_data;

    // restore the callback function and its data of the nonblocking
    // communication request, and call the callback function */
    nb_req->req_complete_cb      = req->nb_req_complete_cb;
    nb_req->req_complete_cb_data = req->nb_req_complete_cb_data;
    if (NULL != nb_req->req_complete_cb) {
        rc = nb_req->req_complete_cb(nb_req);
        if (OMPI_SUCCESS != rc) {
            return rc;
        }
    }
    ompi_request_free(&nb_req);

    // mark this persistent communication request as complete
    // so that `MPI_WAIT` etc. will return */
    rc = ompi_request_complete(&req->super, true);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    return OMPI_SUCCESS;
}

static int request_free(struct ompi_request_t **request_ptr)
{
    mca_coll_nbpreq_request_t *req = (mca_coll_nbpreq_request_t *)*request_ptr;

    OMPI_REQUEST_FINI(&req->super);
    opal_free_list_return(&mca_coll_nbpreq_component.free_requests,
                          &req->super.super);

    return OMPI_SUCCESS;
}

static int request_cancel(struct ompi_request_t *request, int flag)
{
    // The MPI standard forbids calling `MPI_CANCEL` for a persistent
    // collective communication request.
    return MPI_ERR_REQUEST;
}

static void request_construct(mca_coll_nbpreq_request_t *request)
{
    request->super.req_type = OMPI_REQUEST_COLL;
    request->super.req_status._cancelled = 0;
    request->super.req_start  = request_start;
    request->super.req_free   = request_free;
    request->super.req_cancel = request_cancel;
}

OBJ_CLASS_INSTANCE(mca_coll_nbpreq_request_t,
                   ompi_request_t,
                   request_construct,
                   NULL);
