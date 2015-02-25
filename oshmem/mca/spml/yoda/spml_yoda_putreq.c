/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
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
#include "opal/mca/btl/btl.h"
#include "orte/mca/errmgr/errmgr.h"
#include "opal/mca/mpool/mpool.h"
#include "ompi/mca/bml/base/base.h"
#include "oshmem/mca/spml/yoda/spml_yoda.h"
#include "oshmem/mca/spml/yoda/spml_yoda_putreq.h"
#include "oshmem/mca/spml/yoda/spml_yoda_rdmafrag.h"
#include "oshmem/runtime/runtime.h"
/*
 * The free call mark the final stage in a request life-cycle. Starting from this
 * point the request is completed at both SPML and user level, and can be used
 * for others p2p communications. Therefore, in the case of the YODA SPML it should
 * be added to the free request list.
 */
static int mca_spml_yoda_put_request_free(struct oshmem_request_t** request)
{
    mca_spml_yoda_put_request_t* putreq =
            *(mca_spml_yoda_put_request_t**) request;

    assert( false == putreq->req_put.req_base.req_free_called);

    OPAL_THREAD_LOCK(&oshmem_request_lock);
    putreq->req_put.req_base.req_free_called = true;
    opal_free_list_return (&mca_spml_base_put_requests,
                           (opal_free_list_item_t*)putreq);
    OPAL_THREAD_UNLOCK(&oshmem_request_lock);

    *request = SHMEM_REQUEST_NULL;
    return OSHMEM_SUCCESS;
}

static int mca_spml_yoda_put_request_cancel(struct oshmem_request_t* request,
                                            int complete)
{
    /* we dont cancel put requests by now */
    return OSHMEM_SUCCESS;
}

static void mca_spml_yoda_put_request_construct(mca_spml_yoda_put_request_t* req)
{
    req->req_put.req_base.req_type = MCA_SPML_REQUEST_PUT;
    req->req_put.req_base.req_oshmem.req_free = mca_spml_yoda_put_request_free;
    req->req_put.req_base.req_oshmem.req_cancel =
            mca_spml_yoda_put_request_cancel;
}

static void mca_spml_yoda_put_request_destruct(mca_spml_yoda_put_request_t* req)
{
}

OBJ_CLASS_INSTANCE( mca_spml_yoda_put_request_t,
                   mca_spml_base_put_request_t,
                   mca_spml_yoda_put_request_construct,
                   mca_spml_yoda_put_request_destruct);

void mca_spml_yoda_put_completion(mca_btl_base_module_t* btl,
                                  struct mca_btl_base_endpoint_t* ep,
                                  struct mca_btl_base_descriptor_t* des,
                                  int status)
{
    mca_spml_yoda_rdma_frag_t* frag =
            (mca_spml_yoda_rdma_frag_t*) des->des_cbdata;
    mca_spml_yoda_put_request_t* putreq =
            (mca_spml_yoda_put_request_t*) frag->rdma_req;
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*) des->des_context;

    OPAL_THREAD_ADD32(&mca_spml_yoda.n_active_puts, -1);
    /* check completion status */
    if (OPAL_UNLIKELY(OSHMEM_SUCCESS != status)) {
        /* no way to propagete errors. die */
        SPML_ERROR("FATAL put completion error");
        oshmem_shmem_abort(-1);
    }

    putreq->req_put.req_base.req_spml_complete = true;
    oshmem_request_complete(&putreq->req_put.req_base.req_oshmem, 1);
    oshmem_request_free((oshmem_request_t**) &putreq);
    mca_bml_base_free(bml_btl, des);
}

void mca_spml_yoda_put_completion_rdma (struct mca_btl_base_module_t* module,
					struct mca_btl_base_endpoint_t* endpoint,
					void *local_address,
					struct mca_btl_base_registration_handle_t *local_handle,
					void *context, void *cbdata, int status)
{
    mca_btl_base_descriptor_t *des = (mca_btl_base_descriptor_t *) cbdata;
    mca_bml_base_btl_t *bml_btl = (mca_bml_base_btl_t *) context;
    des->des_context = context;

    if (bml_btl->btl->btl_register_mem) {
	bml_btl->btl->btl_deregister_mem (bml_btl->btl, local_handle);
    }

    des->des_cbfunc (module, endpoint, des, status);
}
