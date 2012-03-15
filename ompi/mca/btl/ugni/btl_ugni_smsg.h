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

typedef enum {
    MCA_BTL_UGNI_TAG_SEND,
    MCA_BTL_UGNI_TAG_DISCONNECT,
    MCA_BTL_UGNI_TAG_PUT_INIT,
    MCA_BTL_UGNI_TAG_GET_INIT,
    MCA_BTL_UGNI_TAG_RDMA_COMPLETE
} mca_btl_ugni_smsg_tag_t;

static inline int ompi_mca_btl_ugni_smsg_send (mca_btl_ugni_base_frag_t *frag,
                                               const bool ignore_local_comp,
                                               void *hdr, size_t hdr_len,
                                               void *payload, size_t payload_len,
                                               mca_btl_ugni_smsg_tag_t tag) {
    gni_return_t grc;
    int rc;

    if (!ignore_local_comp) {
        rc = opal_hash_table_set_value_uint32 (&frag->endpoint->btl->pending_smsg_frags,
                                               frag->msg_id, (void *) frag);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
            return rc;
        }
    }

    grc = GNI_SmsgSendWTag (frag->endpoint->common->ep_handle, hdr, hdr_len, payload, payload_len,
                           ignore_local_comp ? (uint32_t)-1 : frag->msg_id, tag);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != grc)) {
        BTL_VERBOSE(("GNI_SmsgSendWTag failed with rc = %d", rc));

        opal_hash_table_remove_value_uint32 (&frag->endpoint->btl->pending_smsg_frags,
                                             frag->msg_id);

        if (OPAL_LIKELY(GNI_RC_NOT_DONE == grc)) {
            BTL_VERBOSE(("out of credits"));

            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

#endif /* MCA_BTL_UGNI_SMSG_H */
