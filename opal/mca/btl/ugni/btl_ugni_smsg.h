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

#if !defined(MCA_BTL_UGNI_SMSG_H)
#define MCA_BTL_UGNI_SMSG_H

#include "btl_ugni.h"
#include "btl_ugni_endpoint.h"
#include "btl_ugni_frag.h"
#include "btl_ugni_rdma.h"

typedef enum {
    MCA_BTL_UGNI_TAG_SEND,
    MCA_BTL_UGNI_TAG_DISCONNECT,
    MCA_BTL_UGNI_TAG_GET_INIT,
    MCA_BTL_UGNI_TAG_RDMA_COMPLETE
} mca_btl_ugni_smsg_tag_t;

typedef struct mca_btl_ugni_smsg_mbox_t {
    ompi_free_list_item_t super;
    mca_btl_ugni_endpoint_attr_t attr;
} mca_btl_ugni_smsg_mbox_t;

OBJ_CLASS_DECLARATION(mca_btl_ugni_smsg_mbox_t);

int mca_btl_ugni_smsg_init (mca_btl_ugni_module_t *ugni_module);
int mca_btl_ugni_smsg_process (mca_btl_base_endpoint_t *ep);
int mca_btl_ugni_progress_remote_smsg (mca_btl_ugni_module_t *btl);

static inline int mca_btl_ugni_progress_local_smsg (mca_btl_ugni_module_t *ugni_module)
{
    mca_btl_ugni_base_frag_t *frag;
    gni_cq_entry_t event_data;
    gni_return_t grc;

    /* nothing to do */
    if (0 == ugni_module->active_send_count) {
        return OPAL_SUCCESS;
    }

    OPAL_THREAD_LOCK(&ugni_module->device->dev_lock);
    grc = GNI_CqGetEvent (ugni_module->smsg_local_cq, &event_data);
    OPAL_THREAD_UNLOCK(&ugni_module->device->dev_lock);
    if (GNI_RC_NOT_DONE == grc) {
        return OPAL_SUCCESS;
    }

    if (OPAL_UNLIKELY((GNI_RC_SUCCESS != grc && !event_data) || GNI_CQ_OVERRUN(event_data))) {
        /* TODO -- need to handle overrun -- how do we do this without an event?
           will the event eventually come back? Ask Cray */
        BTL_ERROR(("post error! cq overrun = %d", (int)GNI_CQ_OVERRUN(event_data)));
        assert (0);
        return opal_common_rc_ugni_to_opal (grc);
    }

    assert (GNI_CQ_GET_TYPE(event_data) == GNI_CQ_EVENT_TYPE_SMSG);

    frag = (mca_btl_ugni_base_frag_t *) opal_pointer_array_get_item (&ugni_module->pending_smsg_frags_bb,
                                                                     GNI_CQ_GET_MSG_ID(event_data));
    if (OPAL_UNLIKELY(NULL == frag)) {
        assert (0);
        return OPAL_ERROR;
    }

    (void)opal_atomic_add_32(&ugni_module->active_send_count, -1);

    frag->flags |= MCA_BTL_UGNI_FRAG_SMSG_COMPLETE;

    if (!(frag->flags & MCA_BTL_UGNI_FRAG_IGNORE)) {
        mca_btl_ugni_frag_complete (frag, OPAL_SUCCESS);
    }

    return 1;
}

static void mca_btl_ugni_cqwrite_complete (struct mca_btl_ugni_base_frag_t *frag, int rc)
{
    frag->flags |= MCA_BTL_UGNI_FRAG_COMPLETE;

    BTL_VERBOSE(("cqwrite  frag complete"));
    mca_btl_ugni_frag_return (frag);
}

static inline int opal_mca_btl_ugni_smsg_send (mca_btl_ugni_base_frag_t *frag,
                                               void *hdr, size_t hdr_len,
                                               void *payload, size_t payload_len,
                                               mca_btl_ugni_smsg_tag_t tag)
{
    int rc;
    gni_return_t grc;
    mca_btl_ugni_base_frag_t *cq_write_frag = NULL;

    OPAL_THREAD_LOCK(&frag->endpoint->common->dev->dev_lock);
    grc = GNI_SmsgSendWTag (frag->endpoint->smsg_ep_handle, hdr, hdr_len,
                            payload, payload_len, frag->msg_id, tag);
    OPAL_THREAD_UNLOCK(&frag->endpoint->common->dev->dev_lock);

    if (OPAL_LIKELY(GNI_RC_SUCCESS == grc)) {
        /* increment the active send counter */
        (void)opal_atomic_add_32(&frag->endpoint->btl->active_send_count, 1);

        if (howards_progress_var == 1 && (getenv("GENERATE_MDH_IRQS") != NULL)) {
            if (frag->base.des_flags & MCA_BTL_DES_FLAGS_SIGNAL) {
                rc = mca_btl_ugni_frag_alloc(frag->endpoint,
                                             &frag->endpoint->btl->rdma_frags,
                                             &cq_write_frag);
                if (rc == OPAL_SUCCESS) {
                    cq_write_frag->registration = NULL;
                    cq_write_frag->endpoint = frag->endpoint;
                    cq_write_frag->post_desc.base.type = GNI_POST_CQWRITE;
                    cq_write_frag->post_desc.base.cqwrite_value = 0xdead;   /* up to 48 bytes here, not used for now */
                    cq_write_frag->post_desc.base.cq_mode = GNI_CQMODE_GLOBAL_EVENT;
                    cq_write_frag->post_desc.base.dlvr_mode = GNI_DLVMODE_IN_ORDER;
                    cq_write_frag->post_desc.base.src_cq_hndl = frag->endpoint->btl->rdma_local_cq;
                    cq_write_frag->post_desc.base.remote_mem_hndl = frag->endpoint->rmt_irq_mem_hndl;
                    cq_write_frag->post_desc.tries = 0;
                    cq_write_frag->cbfunc = mca_btl_ugni_cqwrite_complete;
#if 0
                fprintf(stderr,"doing a GNI_PostCqWrite to 0x%lx 0x%lx \n",cq_write_frag->post_desc.base.remote_mem_hndl.qword1,
                                                                           cq_write_frag->post_desc.base.remote_mem_hndl.qword2);
#endif
                    OPAL_THREAD_LOCK(&frag->endpoint->common->dev->dev_lock);
                    grc = GNI_PostCqWrite(frag->endpoint->rdma_ep_handle, &cq_write_frag->post_desc.base);
                    OPAL_THREAD_UNLOCK(&frag->endpoint->common->dev->dev_lock);
                    if (grc == GNI_RC_ERROR_RESOURCE) {   /* errors for PostCqWrite treated as non-fatal */
                        fprintf(stderr,"GNI_PostCqWrite returned gni error %s\n",gni_err_str[grc]);
                        mca_btl_ugni_frag_return (cq_write_frag);
                    }
                }
            }
        }

        (void) mca_btl_ugni_progress_local_smsg ((mca_btl_ugni_module_t *) frag->endpoint->btl);
        return OPAL_SUCCESS;
    }

    if (OPAL_LIKELY(GNI_RC_NOT_DONE == grc)) {
        BTL_VERBOSE(("out of credits"));

        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    BTL_ERROR(("GNI_SmsgSendWTag failed with rc = %d. handle = %lu, hdr_len = %d, payload_len = %d",
               grc, (uintptr_t) frag->endpoint->smsg_ep_handle, (int) hdr_len, (int) payload_len));

    return OPAL_ERROR;
}

static inline int mca_btl_ugni_send_frag (struct mca_btl_base_endpoint_t *btl_peer,
                                          mca_btl_ugni_base_frag_t *frag) {
    if (OPAL_LIKELY(!(frag->flags & MCA_BTL_UGNI_FRAG_EAGER))) {
        return opal_mca_btl_ugni_smsg_send (frag, &frag->hdr.send, frag->hdr_size,
                                            frag->segments[1].base.seg_addr.pval,
                                            frag->segments[1].base.seg_len,
                                            MCA_BTL_UGNI_TAG_SEND);
    }

    frag->hdr.eager.src_seg = frag->segments[1];
    frag->hdr.eager.ctx     = (void *) frag;

    return opal_mca_btl_ugni_smsg_send (frag, &frag->hdr.eager, frag->hdr_size,
                                        NULL, 0, MCA_BTL_UGNI_TAG_GET_INIT);
}

#endif /* MCA_BTL_UGNI_SMSG_H */
