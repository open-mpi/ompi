/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"
#include "opal/prefetch.h"
#include "oshmem/constants.h"
#include "oshmem/mca/spml/spml.h"
#include "ompi/mca/btl/btl.h"
#include "orte/mca/errmgr/errmgr.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/mca/bml/base/base.h"
#include "oshmem/mca/spml/yoda/spml_yoda.h"
#include "oshmem/mca/spml/yoda/spml_yoda_putreq.h"
#include "oshmem/mca/spml/yoda/spml_yoda_getreq.h"
#include "oshmem/mca/spml/yoda/spml_yoda_rdmafrag.h"

/*
 * The free call mark the final stage in a request life-cycle. Starting from this
 * point the request is completed at both SPML and user level, and can be used
 * for others one sided communications. Therefore, in the case of the YODA SPML it should
 * be added to the free request list.
 */
static int mca_spml_yoda_get_request_free(struct oshmem_request_t** request)
{
    mca_spml_yoda_get_request_t* getreq =
            *(mca_spml_yoda_get_request_t**) request;

    assert( false == getreq->req_get.req_base.req_free_called);

    OPAL_THREAD_LOCK(&oshmem_request_lock);
    getreq->req_get.req_base.req_free_called = true;

    OMPI_FREE_LIST_RETURN_MT( &mca_spml_base_get_requests,
                          (ompi_free_list_item_t*)getreq);

    OPAL_THREAD_UNLOCK(&oshmem_request_lock);

    *request = SHMEM_REQUEST_NULL; /*MPI_REQUEST_NULL;*/
    return OSHMEM_SUCCESS;
}

static int mca_spml_yoda_get_request_cancel(struct oshmem_request_t* request,
                                            int complete)
{
    /* we dont cancel get requests by now */
    return OSHMEM_SUCCESS;
}

static void mca_spml_yoda_get_request_construct(mca_spml_yoda_get_request_t* req)
{
    req->req_get.req_base.req_type = MCA_SPML_REQUEST_GET;
    req->req_get.req_base.req_oshmem.req_free = mca_spml_yoda_get_request_free;
    req->req_get.req_base.req_oshmem.req_cancel =
            mca_spml_yoda_get_request_cancel;
}

static void mca_spml_yoda_get_request_destruct(mca_spml_yoda_get_request_t* req)
{
}

OBJ_CLASS_INSTANCE( mca_spml_yoda_get_request_t,
                   mca_spml_base_get_request_t,
                   mca_spml_yoda_get_request_construct,
                   mca_spml_yoda_get_request_destruct);

void mca_spml_yoda_get_completion(mca_btl_base_module_t* btl,
                                  struct mca_btl_base_endpoint_t* ep,
                                  struct mca_btl_base_descriptor_t* des,
                                  int status)
{
    mca_spml_yoda_rdma_frag_t* frag =
            (mca_spml_yoda_rdma_frag_t*) des->des_cbdata;
    mca_spml_yoda_get_request_t* getreq =
            (mca_spml_yoda_get_request_t*) frag->rdma_req;
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*) des->des_context;

    /* check completion status */
    if (OPAL_UNLIKELY(OSHMEM_SUCCESS != status)) {
        /* shmem has no way to propagate errors. cry&die */
        SPML_ERROR("FATAL get completion error");
        abort();
    }
    /* decide if we need to copy buffer */
    if (getreq->p_dst) {
        memcpy(getreq->p_dst,
               des->des_dst->seg_addr.pval,
           frag->size);
    }

    if (getreq->parent) {
        OPAL_THREAD_ADD32(&getreq->parent->active_count, -1);
    }
    getreq->req_get.req_base.req_spml_complete = true;
    oshmem_request_complete(&getreq->req_get.req_base.req_oshmem, 1);
    oshmem_request_free((oshmem_request_t**) &getreq);

    mca_bml_base_free(bml_btl, des);

    OPAL_THREAD_ADD32(&mca_spml_yoda.n_active_gets, -1);
}

void mca_spml_yoda_get_response_completion(mca_btl_base_module_t* btl,
                                  struct mca_btl_base_endpoint_t* ep,
                                  struct mca_btl_base_descriptor_t* des,
                                  int status)
{
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*) des->des_context;

    /* check completion status */
    if (OPAL_UNLIKELY(OSHMEM_SUCCESS != status)) {
        /* shmem has no way to propagate errors. cry&die */
        SPML_ERROR("FATAL get completion error");
        abort();
    }

    mca_bml_base_free(bml_btl, des);

    OPAL_THREAD_ADD32(&mca_spml_yoda.n_active_gets, -1);
}
