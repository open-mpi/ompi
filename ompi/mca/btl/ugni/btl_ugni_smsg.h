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
        return OMPI_SUCCESS;
    }

    grc = GNI_CqGetEvent (ugni_module->smsg_local_cq, &event_data);
    if (GNI_RC_NOT_DONE == grc) {
        return OMPI_SUCCESS;
    }

    if (OPAL_UNLIKELY((GNI_RC_SUCCESS != grc && !event_data) || GNI_CQ_OVERRUN(event_data))) {
        /* TODO -- need to handle overrun -- how do we do this without an event?
           will the event eventually come back? Ask Cray */
        BTL_ERROR(("post error! cq overrun = %d", (int)GNI_CQ_OVERRUN(event_data)));
        assert (0);
        return ompi_common_rc_ugni_to_ompi (grc);
    }

    assert (GNI_CQ_GET_TYPE(event_data) == GNI_CQ_EVENT_TYPE_SMSG);

    frag = (mca_btl_ugni_base_frag_t *) opal_pointer_array_get_item (&ugni_module->pending_smsg_frags_bb,
                                                                     GNI_CQ_GET_MSG_ID(event_data));
    if (OPAL_UNLIKELY(NULL == frag)) {
        assert (0);
        return OMPI_ERROR;
    }

    ugni_module->active_send_count--;

    frag->flags |= MCA_BTL_UGNI_FRAG_SMSG_COMPLETE;

    if (!(frag->flags & MCA_BTL_UGNI_FRAG_IGNORE)) {
        mca_btl_ugni_frag_complete (frag, OMPI_SUCCESS);
    }

    return 1;
}

static inline int ompi_mca_btl_ugni_smsg_send (mca_btl_ugni_base_frag_t *frag,
                                               void *hdr, size_t hdr_len,
                                               void *payload, size_t payload_len,
                                               mca_btl_ugni_smsg_tag_t tag) {
    gni_return_t grc;

    grc = GNI_SmsgSendWTag (frag->endpoint->smsg_ep_handle, hdr, hdr_len,
                            payload, payload_len, frag->msg_id, tag);

    if (OPAL_LIKELY(GNI_RC_SUCCESS == grc)) {
        /* increment the active send counter */
        frag->endpoint->btl->active_send_count++;

        (void) mca_btl_ugni_progress_local_smsg ((mca_btl_ugni_module_t *) frag->endpoint->btl);
        return OMPI_SUCCESS;
    }

    if (OPAL_LIKELY(GNI_RC_NOT_DONE == grc)) {
        BTL_VERBOSE(("out of credits"));

        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    BTL_ERROR(("GNI_SmsgSendWTag failed with rc = %d. handle = %lu, hdr_len = %d, payload_len = %d",
               grc, (uintptr_t) frag->endpoint->smsg_ep_handle, (int) hdr_len, (int) payload_len));

    return OMPI_ERROR;
}

static inline int mca_btl_ugni_send_frag (struct mca_btl_base_endpoint_t *btl_peer,
                                          mca_btl_ugni_base_frag_t *frag) {
    if (OPAL_LIKELY(!(frag->flags & MCA_BTL_UGNI_FRAG_EAGER))) {
        return ompi_mca_btl_ugni_smsg_send (frag, &frag->hdr.send, frag->hdr_size,
                                            frag->segments[1].base.seg_addr.pval,
                                            frag->segments[1].base.seg_len,
                                            MCA_BTL_UGNI_TAG_SEND);
    }

    frag->hdr.eager.src_seg = frag->segments[1];
    frag->hdr.eager.ctx     = (void *) frag;

    return ompi_mca_btl_ugni_smsg_send (frag, &frag->hdr.eager, frag->hdr_size,
                                        NULL, 0, MCA_BTL_UGNI_TAG_GET_INIT);
}

#endif /* MCA_BTL_UGNI_SMSG_H */
