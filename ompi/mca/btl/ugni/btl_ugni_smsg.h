/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC. All rights
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

int mca_btl_ugni_smsg_process (mca_btl_base_endpoint_t *ep);
int mca_btl_ugni_progress_remote_smsg (mca_btl_ugni_module_t *btl);

static inline int mca_btl_ugni_smsg_next_local_completion (mca_btl_ugni_module_t *ugni_module, mca_btl_ugni_base_frag_t **frag)
{
    gni_cq_entry_t event_data;
    gni_return_t rc;
    uint32_t msg_id;

    *frag = NULL;

    rc = GNI_CqGetEvent (ugni_module->smsg_local_cq, &event_data);
    if (GNI_RC_NOT_DONE == rc) {
        return OMPI_SUCCESS;
    }

    if (OPAL_UNLIKELY((GNI_RC_SUCCESS != rc && !event_data) || GNI_CQ_OVERRUN(event_data))) {
        /* TODO -- need to handle overrun -- how do we do this without an event?
           will the event eventually come back? Ask Cray */
        BTL_ERROR(("post error! cq overrun = %d", (int)GNI_CQ_OVERRUN(event_data)));
        assert (0);
        return ompi_common_rc_ugni_to_ompi (rc);
    }

    assert (GNI_CQ_GET_TYPE(event_data) == GNI_CQ_EVENT_TYPE_SMSG);

    msg_id = GNI_CQ_GET_MSG_ID(event_data);
    if ((uint32_t) -1 == msg_id) {
        /* nothing to do */
        return OMPI_SUCCESS;
    }

    *frag = (mca_btl_ugni_base_frag_t *) opal_pointer_array_get_item (&ugni_module->pending_smsg_frags_bb, msg_id);
    assert (NULL != *frag);

    return GNI_CQ_STATUS_OK(event_data) ? OMPI_SUCCESS : OMPI_ERROR;
}

static inline int mca_btl_ugni_progress_local_smsg (mca_btl_ugni_module_t *ugni_module)
{
    mca_btl_ugni_base_frag_t *frag;
    int rc;

    rc = mca_btl_ugni_smsg_next_local_completion (ugni_module, &frag);
    if (NULL != frag) {
        mca_btl_ugni_frag_complete (frag, rc);
    }

    return 1;
}

static inline int ompi_mca_btl_ugni_smsg_send (mca_btl_ugni_base_frag_t *frag,
                                               const bool ignore_local_comp,
                                               void *hdr, size_t hdr_len,
                                               void *payload, size_t payload_len,
                                               mca_btl_ugni_smsg_tag_t tag) {
    gni_return_t grc;
    int rc;

    grc = GNI_SmsgSendWTag (frag->endpoint->smsg_ep_handle, hdr, hdr_len, payload, payload_len,
                           ignore_local_comp ? (uint32_t) -1 : frag->msg_id, tag);

    (void) mca_btl_ugni_progress_local_smsg ((mca_btl_ugni_module_t *) frag->endpoint->btl);

    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != grc)) {
        /* see if we can free up some credits */
        (void) mca_btl_ugni_progress_remote_smsg ((mca_btl_ugni_module_t *) frag->endpoint->btl);

        if (OPAL_LIKELY(GNI_RC_NOT_DONE == grc)) {
            BTL_VERBOSE(("out of credits"));

            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        BTL_ERROR(("GNI_SmsgSendWTag failed with rc = %d. handle = %d, hdr_len = %d, payload_len = %d",
                   grc, frag->endpoint->smsg_ep_handle, hdr_len, payload_len));

        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

#endif /* MCA_BTL_UGNI_SMSG_H */
