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

int mca_btl_ugni_send (struct mca_btl_base_module_t *btl,
                       struct mca_btl_base_endpoint_t *btl_peer,
                       struct mca_btl_base_descriptor_t *descriptor,
                       mca_btl_base_tag_t tag)
{
    mca_btl_ugni_base_frag_t *frag = (mca_btl_ugni_base_frag_t *) descriptor;
    size_t size = frag->segments[0].seg_len + frag->segments[1].seg_len;
    bool use_eager_get = size > mca_btl_ugni_component.smsg_max_data;
    int rc;

    BTL_VERBOSE(("btl/ugni sending descriptor %p from %d -> %d. length = %d", (void *)descriptor,
                 ORTE_PROC_MY_NAME->vpid, btl_peer->common->ep_rem_id, frag->segments[0].seg_len));

    /* tag and len are at the same location in eager and smsg frag hdrs */
    frag->hdr.send.lag = (tag << 24) | size;

    if (OPAL_UNLIKELY(use_eager_get)) {
        frag->hdr.eager.src_seg = frag->segments[1];
        frag->hdr.eager.ctx     = (void *) frag;
    }

    frag->base.des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
    frag->endpoint = btl_peer;

    rc = mca_btl_ugni_check_endpoint_state (btl_peer);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        opal_list_append (&btl_peer->pending_list, (opal_list_item_t *) frag);
        return OMPI_SUCCESS;
    }

    rc = ompi_mca_btl_ugni_smsg_send (frag, use_eager_get, &frag->hdr.send, frag->hdr_size,
                                      frag->segments[1].seg_addr.pval, use_eager_get ? 0 : frag->segments[1].seg_len,
                                      use_eager_get ? MCA_BTL_UGNI_TAG_GET_INIT : MCA_BTL_UGNI_TAG_SEND);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    (void) mca_btl_ugni_progress_local_smsg ((mca_btl_ugni_module_t *) btl);

    return OMPI_SUCCESS;
}
