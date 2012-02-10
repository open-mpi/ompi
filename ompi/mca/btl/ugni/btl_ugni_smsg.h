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
    MCA_BTL_UGNI_TAG_PUT_COMPLETE,
    MCA_BTL_UGNI_TAG_GET_INIT,
    MCA_BTL_UGNI_TAG_GET_COMPLETE
} mca_btl_ugni_smsg_tag_t;

typedef struct mca_btl_ugni_smsg_mbox_t {
    ompi_free_list_item_t super;
    gni_smsg_attr_t  smsg_attrib;
} mca_btl_ugni_smsg_mbox_t;

OBJ_CLASS_DECLARATION(mca_btl_ugni_smsg_mbox_t);

static inline int ompi_mca_btl_ugni_smsg_send (mca_btl_ugni_base_frag_t *frag,
                                               const bool ignore_local_comp,
                                               void *hdr, size_t hdr_len,
                                               void *payload, size_t payload_len,
                                               mca_btl_ugni_smsg_tag_t tag) {
    static uint8_t msg_num = 0;
    gni_return_t rc;

    frag->msg_id = ignore_local_comp ? (uint32_t) -1 :
        (frag->endpoint->common->ep_rem_id & 0x00ffffff) | ((uint32_t)msg_num++ << 24);

    rc = GNI_SmsgSendWTag (frag->endpoint->common->ep_handle, hdr, hdr_len, payload, payload_len,
                           frag->msg_id, tag);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
        BTL_VERBOSE(("GNI_SmsgSendWTag failed with rc = %d", rc));

        if (OPAL_LIKELY(GNI_RC_NOT_DONE == rc)) {
            BTL_VERBOSE(("out of credits"));

            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        return OMPI_ERROR;
    }

    if (false == ignore_local_comp) {
        opal_list_append (&frag->endpoint->pending_smsg_sends, (opal_list_item_t *) frag);
    }

    return OMPI_SUCCESS;
}

#endif /* MCA_BTL_UGNI_SMSG_H */
