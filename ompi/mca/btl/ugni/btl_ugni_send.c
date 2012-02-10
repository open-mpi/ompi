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

#include "btl_ugni.h"
#include "btl_ugni_frag.h"
#include "btl_ugni_smsg.h"

void mca_btl_ugni_local_smsg_complete (void *btl_ctx, uint32_t msg_id, int rc)
{
    mca_btl_base_endpoint_t *btl_peer = (mca_btl_base_endpoint_t *) btl_ctx;
    mca_btl_ugni_base_frag_t *frag;
    opal_list_item_t *item;

    for (item = opal_list_get_first (&btl_peer->pending_smsg_sends) ;
         item != opal_list_get_end (&btl_peer->pending_smsg_sends) ;
         item = opal_list_get_next (item)) {
        frag = (mca_btl_ugni_base_frag_t *) item;
        if (frag->msg_id == msg_id) {
            opal_list_remove_item (&btl_peer->pending_smsg_sends, item);
            break;
        }
        frag = NULL;
    }

    if (!frag) {
        return;
    }

    /* completion callback */
    if (NULL != frag->base.des_cbfunc) {
        frag->base.des_cbfunc(&btl_peer->btl->super, btl_peer, &frag->base, rc);
    }

    if (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP) {
        mca_btl_ugni_frag_return (frag);
    }    
}

int mca_btl_ugni_send (struct mca_btl_base_module_t *btl,
                       struct mca_btl_base_endpoint_t *btl_peer,
                       struct mca_btl_base_descriptor_t *descriptor,
                       mca_btl_base_tag_t tag)
{
    mca_btl_ugni_base_frag_t *frag = (mca_btl_ugni_base_frag_t *) descriptor;
    int rc;

    BTL_VERBOSE(("btl/ugni sending descriptor %p from %d -> %d. length = %d", (void *)descriptor,
                 ORTE_PROC_MY_NAME->vpid, btl_peer->common->ep_rem_id, frag->segments[0].seg_len));

    /* tag and len are at the same location in eager and smsg frag hdrs */
    frag->hdr.send.tag = tag;
    frag->hdr.send.len = frag->segments[0].seg_len;

    frag->endpoint = btl_peer;

    rc = mca_btl_ugni_check_endpoint_state (btl_peer);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        descriptor->des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;

        opal_list_append (&btl_peer->pending_list, (opal_list_item_t *) frag);
        /* connection started and request queued or something bad happened */
        return OMPI_SUCCESS;
    }

    if (frag->segments[0].seg_len <= mca_btl_ugni_component.smsg_max_data) {
        return ompi_mca_btl_ugni_smsg_send (frag, false, &frag->hdr.send, sizeof (frag->hdr.send),
                                            descriptor->des_src->seg_addr.pval, descriptor->des_src->seg_len,
                                            MCA_BTL_UGNI_TAG_SEND);
    } else {
        frag->hdr.eager.src_seg = frag->segments[0];
        frag->hdr.eager.ctx     = (void *) &frag->post_desc;

        return ompi_mca_btl_ugni_smsg_send (frag, true, &frag->hdr.eager, sizeof (frag->hdr.eager),
                                            NULL, 0, MCA_BTL_UGNI_TAG_GET_INIT);
    }
}
