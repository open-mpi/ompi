/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2018 Los Alamos National Security, LLC. All rights
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
    opal_free_list_item_t super;
    mca_btl_ugni_endpoint_attr_t attr;
} mca_btl_ugni_smsg_mbox_t;

OBJ_CLASS_DECLARATION(mca_btl_ugni_smsg_mbox_t);

int mca_btl_ugni_smsg_init (mca_btl_ugni_module_t *ugni_module);
int mca_btl_ugni_smsg_process (mca_btl_base_endpoint_t *ep);
int mca_btl_ugni_progress_remote_smsg (mca_btl_ugni_module_t *btl);

static inline int mca_btl_ugni_progress_local_smsg (mca_btl_ugni_module_t *ugni_module, mca_btl_ugni_device_t *device)
{
    mca_btl_ugni_base_frag_t *frag;
    gni_cq_entry_t event_data;
    gni_return_t grc;

    grc = mca_btl_ugni_cq_get_event (device, &device->dev_smsg_local_cq, &event_data);
    if (GNI_RC_NOT_DONE == grc) {
        return OPAL_SUCCESS;
    }

    if (OPAL_UNLIKELY((GNI_RC_SUCCESS != grc && !event_data) || GNI_CQ_OVERRUN(event_data))) {
        /* TODO -- need to handle overrun -- how do we do this without an event?
         * will the event eventually come back? Ask Cray */
        return mca_btl_ugni_event_fatal_error (grc, event_data);
    }

    assert (GNI_CQ_GET_TYPE(event_data) == GNI_CQ_EVENT_TYPE_SMSG);

    frag = (mca_btl_ugni_base_frag_t *) opal_pointer_array_get_item (&ugni_module->pending_smsg_frags_bb,
                                                                     GNI_CQ_GET_MSG_ID(event_data));
    if (OPAL_UNLIKELY(NULL == frag)) {
        assert (0);
        return OPAL_ERROR;
    }

    frag->flags |= MCA_BTL_UGNI_FRAG_SMSG_COMPLETE;

    if (!(frag->flags & MCA_BTL_UGNI_FRAG_IGNORE)) {
        mca_btl_ugni_frag_complete (frag, OPAL_SUCCESS);
    }

    return 1;
}

static inline int opal_mca_btl_ugni_smsg_send (mca_btl_ugni_base_frag_t *frag,
                                               void *hdr, size_t hdr_len,
                                               void *payload, size_t payload_len,
                                               mca_btl_ugni_smsg_tag_t tag)
{
    mca_btl_base_endpoint_t *endpoint = frag->endpoint;
    mca_btl_ugni_module_t *ugni_module = mca_btl_ugni_ep_btl (endpoint);
    gni_return_t grc;

    grc = mca_btl_ugni_endpoint_smsg_send_wtag (endpoint, hdr, hdr_len, payload, payload_len,
                                                frag->msg_id, tag);
    if (OPAL_LIKELY(GNI_RC_SUCCESS == grc)) {
        if (mca_btl_ugni_component.progress_thread_enabled) {
            if (frag->base.des_flags & MCA_BTL_DES_FLAGS_SIGNAL) {
                /* errors for PostCqWrite treated as non-fatal */
                (void) mca_btl_ugni_post_cqwrite (endpoint, &ugni_module->devices[0].dev_rdma_local_cq,
                                                  endpoint->rmt_irq_mem_hndl, 0xdead, NULL, NULL, NULL);
            }
        }

        (void) mca_btl_ugni_progress_local_smsg (ugni_module, endpoint->smsg_ep_handle.device);
        return OPAL_SUCCESS;
    }

    if (OPAL_LIKELY(GNI_RC_NOT_DONE == grc)) {
        BTL_VERBOSE(("out of credits"));

        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    BTL_ERROR(("GNI_SmsgSendWTag failed with rc = %d. handle = %lu, hdr_len = %d, payload_len = %d",
               grc, (uintptr_t) frag->endpoint->smsg_ep_handle.gni_handle, (int) hdr_len,
               (int) payload_len));

    return OPAL_ERROR;
}

static inline int mca_btl_ugni_send_frag (struct mca_btl_base_endpoint_t *btl_peer,
                                          mca_btl_ugni_base_frag_t *frag) {
    if (OPAL_LIKELY(!(frag->flags & MCA_BTL_UGNI_FRAG_EAGER))) {
        return opal_mca_btl_ugni_smsg_send (frag, &frag->hdr.send, frag->hdr_size,
                                            frag->segments[1].seg_addr.pval,
                                            frag->segments[1].seg_len,
                                            MCA_BTL_UGNI_TAG_SEND);
    }

    frag->hdr.eager.size    = frag->segments[1].seg_len;
    frag->hdr.eager.address = frag->segments[1].seg_addr.lval;
    frag->hdr.eager.ctx     = (void *) frag;

    return opal_mca_btl_ugni_smsg_send (frag, &frag->hdr.eager, frag->hdr_size,
                                        NULL, 0, MCA_BTL_UGNI_TAG_GET_INIT);
}

#endif /* MCA_BTL_UGNI_SMSG_H */
