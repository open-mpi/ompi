/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_ugni.h"
#include "btl_ugni_frag.h"
#include "btl_ugni_endpoint.h"

int mca_btl_ugni_send (struct mca_btl_base_module_t *btl,
                       struct mca_btl_base_endpoint_t *btl_peer,
                       struct mca_btl_base_descriptor_t *descriptor,
                       mca_btl_base_tag_t tag)
{
    mca_btl_ugni_base_frag_t *frag = (mca_btl_ugni_base_frag_t *) descriptor;
    int rc;

    BTL_VERBOSE(("btl/ugni sending descriptor %p from %d -> %d. length = %d", (void *)descriptor,
                 ORTE_PROC_MY_NAME->vpid, btl_peer->common->ep_rem_id, frag->segments[0].seg_len));

    rc = mca_btl_ugni_check_endpoint_state (btl_peer);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        frag->tag = tag;
        descriptor->des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;

        opal_list_append (&btl_peer->pending_list, (opal_list_item_t *) frag);
        /* connection started and request queued or something bad happened */
        return OMPI_SUCCESS;
    }

    frag->hdr->tag = tag;
    frag->hdr->len = frag->segments[0].seg_len;

    /* check endpoint state */
    rc = GNI_SmsgSendWTag (btl_peer->common->ep_handle, frag->hdr,
                           descriptor->des_src->seg_len + sizeof (frag->hdr[0]),
                           NULL, 0, -1, MCA_BTL_UGNI_TAG_SEND);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
        BTL_VERBOSE(("GNI_SmsgSendWTag failed with rc = %d", rc));

        if (OPAL_LIKELY(GNI_RC_NOT_DONE == rc)) {
            BTL_VERBOSE(("out of credits"));

            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        return OMPI_ERROR;
    }

    if (MCA_BTL_DES_SEND_ALWAYS_CALLBACK & frag->base.des_flags) {
        /* completion callback */
        frag->base.des_cbfunc(&btl_peer->btl->super, btl_peer, &frag->base, OMPI_SUCCESS);
    }

    if (descriptor->des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP) {
        MCA_BTL_UGNI_FRAG_RETURN (frag);
    }

    return 1;
}
