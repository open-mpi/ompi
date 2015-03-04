/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_openib.h"
#include "btl_openib_endpoint.h"
#include "btl_openib_proc.h"
#include "btl_openib_xrc.h"

#if HAVE_DECL_IBV_ATOMIC_HCA

static int mca_btl_openib_atomic_internal (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
					   void *local_address, uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
					   mca_btl_base_registration_handle_t *remote_handle, enum ibv_wr_opcode opcode,
					   int64_t operand, int operand2, int flags, int order, mca_btl_base_rdma_completion_fn_t cbfunc,
					   void *cbcontext, void *cbdata)
{
    mca_btl_openib_get_frag_t* frag = NULL;
    int qp = order;
    int rc;

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
    to_com_frag(frag)->sg_entry.length = 8;
    to_com_frag(frag)->sg_entry.lkey = local_handle->lkey;
    to_com_frag(frag)->sg_entry.addr = (uint64_t)(uintptr_t) local_address;
    to_com_frag(frag)->endpoint = endpoint;

    /* set up rdma callback */
    frag->cb.func = cbfunc;
    frag->cb.context = cbcontext;
    frag->cb.data = cbdata;
    frag->cb.local_handle = local_handle;

    /* set up descriptor */
    frag->sr_desc.wr.atomic.remote_addr = remote_address;
    frag->sr_desc.opcode = opcode;
    frag->sr_desc.wr.atomic.compare_add = operand;
    frag->sr_desc.wr.atomic.swap = operand2;

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    if((endpoint->endpoint_proc->proc_opal->proc_arch & OPAL_ARCH_ISBIGENDIAN)
            != (opal_proc_local_get()->proc_arch & OPAL_ARCH_ISBIGENDIAN)) {
        frag->sr_desc.wr.atomic.rkey = opal_swap_bytes4 (remote_handle->rkey);
    } else
#endif
    {
        frag->sr_desc.wr.atomic.rkey = remote_handle->rkey;
    }

#if HAVE_XRC
    if (MCA_BTL_XRC_ENABLED && BTL_OPENIB_QP_TYPE_XRC(qp)) {
        frag->sr_desc.xrc_remote_srq_num=endpoint->rem_info.rem_srqs[qp].rem_srq_num;
    }
#endif

    if (endpoint->endpoint_state != MCA_BTL_IB_CONNECTED) {
        OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
        rc = check_endpoint_state(endpoint, &to_base_frag(frag)->base, &endpoint->pending_get_frags);
        OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
        if (OPAL_ERR_RESOURCE_BUSY == rc) {
            return OPAL_SUCCESS;
        }

        if (OPAL_SUCCESS != rc) {
            MCA_BTL_IB_FRAG_RETURN (frag);
            return rc;
        }
    }

    rc = mca_btl_openib_get_internal (btl, endpoint, frag);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        if (OPAL_LIKELY(OPAL_ERR_OUT_OF_RESOURCE == rc)) {
            rc = OPAL_SUCCESS;

            OPAL_THREAD_SCOPED_LOCK(&endpoint->endpoint_lock,
				    opal_list_append(&endpoint->pending_get_frags, (opal_list_item_t*)frag));
        } else {
            MCA_BTL_IB_FRAG_RETURN (frag);
        }
    }

    return rc;
}

int mca_btl_openib_atomic_fop (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                               void *local_address, uint64_t remote_address,
                               struct mca_btl_base_registration_handle_t *local_handle,
                               struct mca_btl_base_registration_handle_t *remote_handle, mca_btl_base_atomic_op_t op,
                               uint64_t operand, int flags, int order, mca_btl_base_rdma_completion_fn_t cbfunc,
                               void *cbcontext, void *cbdata)
{

    if (OPAL_UNLIKELY(MCA_BTL_ATOMIC_ADD != op)) {
	return OPAL_ERR_NOT_SUPPORTED;
    }

    return mca_btl_openib_atomic_internal (btl, endpoint, local_address, remote_address, local_handle,
					   remote_handle, IBV_WR_ATOMIC_FETCH_AND_ADD, operand, 0,
					   flags, order, cbfunc, cbcontext, cbdata);
}

int mca_btl_openib_atomic_cswap (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                                 void *local_address, uint64_t remote_address,
                                 struct mca_btl_base_registration_handle_t *local_handle,
                                 struct mca_btl_base_registration_handle_t *remote_handle, uint64_t compare,
                                 uint64_t value, int flags, int order, mca_btl_base_rdma_completion_fn_t cbfunc,
                                 void *cbcontext, void *cbdata)
{
    return mca_btl_openib_atomic_internal (btl, endpoint, local_address, remote_address, local_handle,
					   remote_handle, IBV_WR_ATOMIC_CMP_AND_SWP, compare, value,
					   flags, order, cbfunc, cbcontext, cbdata);
}

#endif
