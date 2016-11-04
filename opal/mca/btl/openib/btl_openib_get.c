/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2009 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2006-2016 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * Copyright (c) 2008-2012 Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2009      IBM Corporation.  All rights reserved.
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved
 * Copyright (c) 2013      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_openib.h"
#include "btl_openib_frag.h"
#include "btl_openib_endpoint.h"
#include "btl_openib_proc.h"
#include "btl_openib_xrc.h"

/*
 * RDMA READ remote buffer to local buffer address.
 */

int mca_btl_openib_get (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *ep, void *local_address,
                        uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                        mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                        int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    mca_btl_openib_get_frag_t* frag = NULL;
    int qp = order;
    int rc;

    if (OPAL_UNLIKELY(size > btl->btl_get_limit)) {
        return OPAL_ERR_BAD_PARAM;
    }

    frag = to_get_frag(alloc_recv_user_frag());
    if (OPAL_UNLIKELY(NULL == frag)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    if (MCA_BTL_NO_ORDER == qp) {
        qp = mca_btl_openib_component.rdma_qp;
    }

    /* set base descriptor flags */
    to_base_frag(frag)->base.order = qp;
    /* free this descriptor when the operation is complete */
    to_base_frag(frag)->base.des_flags = MCA_BTL_DES_FLAGS_BTL_OWNERSHIP;

    /* set up scatter-gather entry */
    to_com_frag(frag)->sg_entry.length = size;
    to_com_frag(frag)->sg_entry.lkey = local_handle->lkey;
    to_com_frag(frag)->sg_entry.addr = (uint64_t)(uintptr_t) local_address;
    to_com_frag(frag)->endpoint = ep;

    /* set up rdma callback */
    frag->cb.func = cbfunc;
    frag->cb.context = cbcontext;
    frag->cb.data = cbdata;
    frag->cb.local_handle = local_handle;

    /* set up descriptor */
    frag->sr_desc.wr.rdma.remote_addr = remote_address;
    /* the opcode may have been changed by an atomic operation */
    frag->sr_desc.opcode = IBV_WR_RDMA_READ;

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    if((ep->endpoint_proc->proc_opal->proc_arch & OPAL_ARCH_ISBIGENDIAN)
            != (opal_proc_local_get()->proc_arch & OPAL_ARCH_ISBIGENDIAN)) {
        frag->sr_desc.wr.rdma.rkey = opal_swap_bytes4 (remote_handle->rkey);
    } else
#endif
    {
        frag->sr_desc.wr.rdma.rkey = remote_handle->rkey;
    }

    if (ep->endpoint_state != MCA_BTL_IB_CONNECTED) {
        OPAL_THREAD_LOCK(&ep->endpoint_lock);
        rc = check_endpoint_state(ep, &to_base_frag(frag)->base, &ep->pending_get_frags);
        OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
        if (OPAL_ERR_RESOURCE_BUSY == rc) {
            return OPAL_SUCCESS;
        }

        if (OPAL_SUCCESS != rc) {
            MCA_BTL_IB_FRAG_RETURN (frag);
            return rc;
        }
    }

    rc = mca_btl_openib_get_internal (btl, ep, frag);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        if (OPAL_LIKELY(OPAL_ERR_OUT_OF_RESOURCE == rc)) {
            rc = OPAL_SUCCESS;

            OPAL_THREAD_LOCK(&ep->endpoint_lock);
            opal_list_append(&ep->pending_get_frags, (opal_list_item_t*)frag);
            OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
        } else {
            MCA_BTL_IB_FRAG_RETURN (frag);
        }
    }

    return rc;
}

int mca_btl_openib_get_internal (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *ep,
                                 mca_btl_openib_get_frag_t *frag)
{
    int qp = to_base_frag(frag)->base.order;
    struct ibv_send_wr *bad_wr;

#if HAVE_XRC
    if (MCA_BTL_XRC_ENABLED && BTL_OPENIB_QP_TYPE_XRC(qp)) {
        /* NTH: the remote SRQ number is only available once the endpoint is connected. By
         * setting the value here instead of mca_btl_openib_get we guarantee the rem_srqs
         * array is initialized. */
#if OPAL_HAVE_CONNECTX_XRC_DOMAINS
        frag->sr_desc.qp_type.xrc.remote_srqn = ep->rem_info.rem_srqs[qp].rem_srq_num;
#else
        frag->sr_desc.xrc_remote_srq_num = ep->rem_info.rem_srqs[qp].rem_srq_num;
#endif
    }
#endif

    /* check for a send wqe */
    if (qp_get_wqe(ep, qp) < 0) {
        qp_put_wqe(ep, qp);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* check for a get token */
    if (OPAL_THREAD_ADD32(&ep->get_tokens,-1) < 0) {
        qp_put_wqe(ep, qp);
        OPAL_THREAD_ADD32(&ep->get_tokens,1);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    qp_inflight_wqe_to_frag(ep, qp, to_com_frag(frag));
    qp_reset_signal_count(ep, qp);

    if (ibv_post_send(ep->qps[qp].qp->lcl_qp, &frag->sr_desc, &bad_wr)) {
        qp_put_wqe(ep, qp);
        OPAL_THREAD_ADD32(&ep->get_tokens,1);
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}
