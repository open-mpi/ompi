/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_ugni_smsg.h"
#include "btl_ugni_rdma.h"

static void mca_btl_ugni_smsg_mbox_construct (mca_btl_ugni_smsg_mbox_t *mbox) {
    struct mca_btl_ugni_reg_t *ugni_reg =
        (struct mca_btl_ugni_reg_t *) mbox->super.registration;
    struct mca_mpool_base_registration_t *base_reg =
        (struct mca_mpool_base_registration_t *) ugni_reg;

    /* initialize mailbox attributes */
    mbox->attr.smsg_attr.msg_type       = GNI_SMSG_TYPE_MBOX_AUTO_RETRANSMIT;
    mbox->attr.smsg_attr.msg_maxsize    = mca_btl_ugni_component.ugni_smsg_limit;
    mbox->attr.smsg_attr.mbox_maxcredit = mca_btl_ugni_component.smsg_max_credits;
    mbox->attr.smsg_attr.mbox_offset    = (uintptr_t) mbox->super.ptr - (uintptr_t) base_reg->base;
    mbox->attr.smsg_attr.msg_buffer     = base_reg->base;
    mbox->attr.smsg_attr.buff_size      = mca_btl_ugni_component.smsg_mbox_size;
    mbox->attr.smsg_attr.mem_hndl       = ugni_reg->memory_hdl;

    mbox->attr.proc_id = mca_btl_ugni_proc_name_to_id (*OMPI_PROC_MY_NAME);
}

OBJ_CLASS_INSTANCE(mca_btl_ugni_smsg_mbox_t, ompi_free_list_item_t,
                   mca_btl_ugni_smsg_mbox_construct, NULL);


int mca_btl_ugni_smsg_init (mca_btl_ugni_module_t *ugni_module)
{
    gni_return_t rc;

    rc = GNI_SmsgSetMaxRetrans (ugni_module->device->dev_handle,
                                mca_btl_ugni_component.smsg_max_retries);
    if (GNI_RC_SUCCESS != rc) {
        BTL_ERROR(("error setting maximum SMSG retries"));
        return ompi_common_rc_ugni_to_ompi (rc);
    }

    return OMPI_SUCCESS;
}

/* progress */
int mca_btl_ugni_smsg_process (mca_btl_base_endpoint_t *ep)
{
    mca_btl_active_message_callback_t *reg;
    mca_btl_ugni_base_frag_t frag;
    mca_btl_base_segment_t seg;
    bool disconnect = false;
    uintptr_t data_ptr;
    gni_return_t rc;
    uint32_t len;
    int count = 0;

    if (!opal_atomic_cmpset_32 (&ep->smsg_progressing, 0, 1)) {
        /* already progressing (we can't support reentry here) */
        return 0;
    }

    /* per uGNI documentation we loop until the mailbox is empty */
    do {
        uint8_t tag = GNI_SMSG_ANY_TAG;

        rc = GNI_SmsgGetNextWTag (ep->smsg_ep_handle, (void **) &data_ptr, &tag);
        if (GNI_RC_NOT_DONE == rc) {
            BTL_VERBOSE(("no smsg message waiting. rc = %d", rc));

            ep->smsg_progressing = 0;

            return count;
        }

        if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
            fprintf (stderr, "Unhandled Smsg error: %d\n", rc);
            assert (0);
            return OMPI_ERROR;
        }

        if (OPAL_UNLIKELY(0 == data_ptr)) {
            BTL_ERROR(("null data ptr!"));
            assert (0);
            return OMPI_ERROR;
        }

        count++;

        BTL_VERBOSE(("got smsg fragment. tag = %d\n", tag));

        switch (tag) {
        case MCA_BTL_UGNI_TAG_SEND:
            frag.hdr.send = ((mca_btl_ugni_send_frag_hdr_t *) data_ptr)[0];

            tag = frag.hdr.send.lag >> 24;
            len = frag.hdr.send.lag & 0x00ffffff;

            BTL_VERBOSE(("received smsg fragment. hdr = {len = %u, tag = %d}", len, tag));

            reg = mca_btl_base_active_message_trigger + tag;
            frag.base.des_dst     = &seg;
            frag.base.des_dst_cnt = 1;

            seg.seg_addr.pval = (void *)((uintptr_t)data_ptr + sizeof (mca_btl_ugni_send_frag_hdr_t));
            seg.seg_len       = len;

            assert (NULL != reg->cbfunc);

            reg->cbfunc(&ep->btl->super, tag, &(frag.base), reg->cbdata);

            break;
        case MCA_BTL_UGNI_TAG_GET_INIT:
            frag.hdr.eager_ex = ((mca_btl_ugni_eager_ex_frag_hdr_t *) data_ptr)[0];

            mca_btl_ugni_start_eager_get (ep, frag.hdr.eager_ex, NULL);
            break;
        case MCA_BTL_UGNI_TAG_RDMA_COMPLETE:
            frag.hdr.rdma = ((mca_btl_ugni_rdma_frag_hdr_t *) data_ptr)[0];

            if (((mca_btl_ugni_base_frag_t *)frag.hdr.rdma.ctx)->flags & MCA_BTL_UGNI_FRAG_SMSG_COMPLETE) {
                mca_btl_ugni_frag_complete (frag.hdr.rdma.ctx, OMPI_SUCCESS);
            } else {
                /* let the local smsg completion finish this frag */
                ((mca_btl_ugni_base_frag_t *)frag.hdr.rdma.ctx)->flags &= ~MCA_BTL_UGNI_FRAG_IGNORE;
            }
            break;
        case MCA_BTL_UGNI_TAG_DISCONNECT:
            /* remote endpoint has disconnected */
            disconnect = true;
            break;
        default:
            BTL_ERROR(("unknown tag %d\n", tag));
            break;
        }

        rc = GNI_SmsgRelease (ep->smsg_ep_handle);
        if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
            BTL_ERROR(("Smsg release failed! rc = %d", rc));
            return OMPI_ERROR;
        }
    } while (!disconnect);

    ep->smsg_progressing = false;

    /* disconnect if we get here */
    opal_mutex_lock (&ep->lock);

    mca_btl_ugni_ep_disconnect (ep, false);

    opal_mutex_unlock (&ep->lock);

    return count;
}

static inline int
mca_btl_ugni_handle_remote_smsg_overrun (mca_btl_ugni_module_t *btl)
{
    gni_cq_entry_t event_data;
    size_t endpoint_count;
    unsigned int ep_index;
    int count, rc;

    BTL_VERBOSE(("btl/ugni_component detected SMSG CQ overrun. "
                 "processing message backlog..."));

    /* we don't know which endpoint lost an smsg completion. clear the
       smsg remote cq and check all mailboxes */

    /* clear out remote cq */
    do {
        rc = GNI_CqGetEvent (btl->smsg_remote_cq, &event_data);
    } while (GNI_RC_NOT_DONE != rc);

    endpoint_count = opal_pointer_array_get_size (&btl->endpoints);

    for (ep_index = 0, count = 0 ; ep_index < endpoint_count ; ++ep_index) {
        mca_btl_base_endpoint_t *ep;

        ep = (mca_btl_base_endpoint_t *) opal_pointer_array_get_item (&btl->endpoints,
                                                                      ep_index);

        if (NULL == ep || MCA_BTL_UGNI_EP_STATE_CONNECTED != ep->state) {
            continue;
        }

        /* clear out smsg mailbox */
        rc = mca_btl_ugni_smsg_process (ep);
        if (OPAL_LIKELY(rc >= 0)) {
            count += rc;
        }
    }

    return count;
}

int mca_btl_ugni_progress_remote_smsg (mca_btl_ugni_module_t *btl)
{
    mca_btl_base_endpoint_t *ep;
    gni_cq_entry_t event_data;
    gni_return_t grc;
    uint64_t inst_id;

    grc = GNI_CqGetEvent (btl->smsg_remote_cq, &event_data);
    if (GNI_RC_NOT_DONE == grc) {
        return 0;
    }

    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != grc || !GNI_CQ_STATUS_OK(event_data) ||
                      GNI_CQ_OVERRUN(event_data))) {
        if (GNI_RC_ERROR_RESOURCE == grc ||
            (GNI_RC_SUCCESS == grc && GNI_CQ_OVERRUN(event_data))) {
            /* recover from smsg cq overrun */
            return mca_btl_ugni_handle_remote_smsg_overrun (btl);
        }

        BTL_ERROR(("unhandled error in GNI_CqGetEvent"));

        /* unhandled error: crash */
        assert (0);
        return ompi_common_rc_ugni_to_ompi (grc);
    }

    BTL_VERBOSE(("REMOTE CQ: Got event 0x%" PRIx64 ". msg id = %" PRIu64
                 ". ok = %d, type = %" PRIu64 "\n", (uint64_t) event_data,
                 GNI_CQ_GET_MSG_ID(event_data), GNI_CQ_STATUS_OK(event_data),
                 GNI_CQ_GET_TYPE(event_data)));

    inst_id = GNI_CQ_GET_INST_ID(event_data);

    ep = (mca_btl_base_endpoint_t *) opal_pointer_array_get_item (&btl->endpoints, inst_id);

    if (OPAL_UNLIKELY(MCA_BTL_UGNI_EP_STATE_CONNECTED != ep->state)) {
        /* due to the nature of datagrams we may get a smsg completion before
           we get mailbox info from the peer */
        BTL_VERBOSE(("event occurred on an unconnected endpoint! ep state = %d", ep->state));
        return 0;
    }

    return mca_btl_ugni_smsg_process (ep);
}

