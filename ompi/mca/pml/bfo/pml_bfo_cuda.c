/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2010-2012 Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2012-2015 NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"
#include "opal/prefetch.h"
#include "opal/mca/btl/btl.h"
#include "opal/mca/mpool/mpool.h"
#include "ompi/constants.h"
#include "ompi/mca/pml/pml.h"
#include "pml_bfo.h"
#include "pml_bfo_hdr.h"
#include "pml_bfo_rdmafrag.h"
#include "pml_bfo_recvreq.h"
#include "pml_bfo_sendreq.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/memchecker.h"

size_t mca_pml_bfo_rdma_cuda_btls(
    mca_bml_base_endpoint_t* bml_endpoint,
    unsigned char* base,
    size_t size,
    mca_pml_bfo_com_btl_t* rdma_btls);

int mca_pml_bfo_cuda_need_buffers(void * rreq,
                                  mca_btl_base_module_t* btl);

/**
 * Handle the CUDA buffer.
 */
int mca_pml_bfo_send_request_start_cuda(mca_pml_bfo_send_request_t* sendreq,
                                        mca_bml_base_btl_t* bml_btl,
                                        size_t size) {
    int rc;
    sendreq->req_send.req_base.req_convertor.flags &= ~CONVERTOR_CUDA;
    if (opal_convertor_need_buffers(&sendreq->req_send.req_base.req_convertor) == false) {
        unsigned char *base;
        opal_convertor_get_current_pointer( &sendreq->req_send.req_base.req_convertor, (void**)&base );
        /* Set flag back */
        sendreq->req_send.req_base.req_convertor.flags |= CONVERTOR_CUDA;
        if( 0 != (sendreq->req_rdma_cnt = (uint32_t)mca_pml_bfo_rdma_cuda_btls(
                                                                           sendreq->req_endpoint,
                                                                           base,
                                                                           sendreq->req_send.req_bytes_packed,
                                                                           sendreq->req_rdma))) {
            rc = mca_pml_bfo_send_request_start_rdma(sendreq, bml_btl,
                                                     sendreq->req_send.req_bytes_packed);
            if( OPAL_UNLIKELY(OMPI_SUCCESS != rc) ) {
                mca_pml_bfo_free_rdma_resources(sendreq);
            }
        } else {
            if (bml_btl->btl_flags & MCA_BTL_FLAGS_CUDA_PUT) {
                rc = mca_pml_bfo_send_request_start_rndv(sendreq, bml_btl, size,
                                                         MCA_PML_BFO_HDR_FLAGS_CONTIG);
            } else {
                rc = mca_pml_bfo_send_request_start_rndv(sendreq, bml_btl, size, 0);
            }
        }
    } else {
        /* Do not send anything with first rendezvous message as copying GPU
         * memory into RNDV message is expensive. */
        sendreq->req_send.req_base.req_convertor.flags |= CONVERTOR_CUDA;
        rc = mca_pml_bfo_send_request_start_rndv(sendreq, bml_btl, 0, 0);
    }
    return rc;
}



size_t mca_pml_bfo_rdma_cuda_btls(
    mca_bml_base_endpoint_t* bml_endpoint,
    unsigned char* base,
    size_t size,
    mca_pml_bfo_com_btl_t* rdma_btls)
{
    int num_btls = mca_bml_base_btl_array_get_size(&bml_endpoint->btl_send);
    double weight_total = 0;
    int num_btls_used = 0, n;

    /* shortcut when there are no rdma capable btls */
    if(num_btls == 0) {
        return 0;
    }

    /* check to see if memory is registered */
    for(n = 0; n < num_btls && num_btls_used < mca_pml_bfo.max_rdma_per_request;
            n++) {
        mca_bml_base_btl_t* bml_btl =
            mca_bml_base_btl_array_get_index(&bml_endpoint->btl_send, n);

        if (bml_btl->btl_flags & MCA_BTL_FLAGS_CUDA_GET) {
            mca_mpool_base_registration_t* reg = NULL;
            mca_mpool_base_module_t *btl_mpool = bml_btl->btl->btl_mpool;

            if( NULL != btl_mpool ) {
                /* register the memory */
                btl_mpool->mpool_register(btl_mpool, base, size, 0, &reg);
            }

            if(NULL == reg)
                continue;

            rdma_btls[num_btls_used].bml_btl = bml_btl;
            rdma_btls[num_btls_used].btl_reg = reg;
            weight_total += bml_btl->btl_weight;
            num_btls_used++;
        }
    }

    /* if we don't use leave_pinned and all BTLs that already have this memory
     * registered amount to less then half of available bandwidth - fall back to
     * pipeline protocol */
    if(0 == num_btls_used || (!mca_pml_bfo.leave_pinned && weight_total < 0.5))
        return 0;

    mca_pml_bfo_calc_weighted_length(rdma_btls, num_btls_used, size,
                                     weight_total);

    return num_btls_used;
}

int mca_pml_bfo_cuda_need_buffers(void * rreq,
                                  mca_btl_base_module_t* btl)
{
    mca_pml_bfo_recv_request_t* recvreq = (mca_pml_bfo_recv_request_t*)rreq;
    if ((recvreq->req_recv.req_base.req_convertor.flags & CONVERTOR_CUDA) &&
        (btl->btl_flags & MCA_BTL_FLAGS_CUDA_GET)) {
        recvreq->req_recv.req_base.req_convertor.flags &= ~CONVERTOR_CUDA;
        if(opal_convertor_need_buffers(&recvreq->req_recv.req_base.req_convertor) == true) {
            recvreq->req_recv.req_base.req_convertor.flags |= CONVERTOR_CUDA;
            return true;
        } else {
            recvreq->req_recv.req_base.req_convertor.flags |= CONVERTOR_CUDA;
            return false;
        }
    }
    return true;
}

