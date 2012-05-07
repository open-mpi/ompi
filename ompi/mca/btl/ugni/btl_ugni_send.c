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
    bool use_eager_get = !!(frag->flags & MCA_BTL_UGNI_FRAG_EAGER);
    int flags_save = frag->base.des_flags;
    int rc;

    BTL_VERBOSE(("btl/ugni sending descriptor %p from %d -> %d. length = %d", (void *)descriptor,
                 ORTE_PROC_MY_NAME->vpid, btl_peer->common->ep_rem_id, frag->segments[0].seg_len));

    /* tag and len are at the same location in eager and smsg frag hdrs */
    frag->hdr.send.lag = (tag << 24) | size;
    if (OPAL_UNLIKELY(use_eager_get)) {
        frag->hdr.eager.src_seg = frag->segments[1];
        frag->hdr.eager.ctx     = (void *) frag;
    }

    frag->endpoint = btl_peer;

    rc = mca_btl_ugni_check_endpoint_state (btl_peer);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        frag->base.des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
        opal_list_append (&btl_peer->pending_list, (opal_list_item_t *) frag);
        return OMPI_SUCCESS;
    }

    /* temporarily disable ownership and callback flags so we can reliably check the complete flag */
    frag->base.des_flags &= ~(MCA_BTL_DES_FLAGS_BTL_OWNERSHIP | MCA_BTL_DES_SEND_ALWAYS_CALLBACK);
    frag->flags &= ~MCA_BTL_UGNI_FRAG_COMPLETE;

    rc = ompi_mca_btl_ugni_smsg_send (frag, use_eager_get, &frag->hdr.send, frag->hdr_size,
                                      frag->segments[1].seg_addr.pval, use_eager_get ? 0 :
                                      frag->segments[1].seg_len, use_eager_get ?
                                      MCA_BTL_UGNI_TAG_GET_INIT : MCA_BTL_UGNI_TAG_SEND);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    if (OPAL_LIKELY(!use_eager_get)) {
        if (OPAL_LIKELY(frag->flags & MCA_BTL_UGNI_FRAG_COMPLETE)) {
            /* fast path: remote side has received the frag */
            frag->base.des_flags = flags_save;
            mca_btl_ugni_frag_complete (frag, OMPI_SUCCESS);

            return 1;
        }

        if ((frag->flags & MCA_BTL_UGNI_FRAG_BUFFERED) && (flags_save & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP)) {
            /* fast(ish) path: btl owned buffered frag. report send as complete */
            frag->base.des_flags = flags_save & ~MCA_BTL_DES_SEND_ALWAYS_CALLBACK;

            if (OPAL_LIKELY(flags_save & MCA_BTL_DES_SEND_ALWAYS_CALLBACK)) {
                frag->base.des_cbfunc(&frag->endpoint->btl->super, frag->endpoint, &frag->base, rc);
            }

            return 1;
        }
    }

    /* slow(ish) path: remote side hasn't received the frag. call the frag's callback when
       we get the local smsg/msgq or remote rdma completion */
    frag->base.des_flags = flags_save | MCA_BTL_DES_SEND_ALWAYS_CALLBACK;

    return OMPI_SUCCESS;
}
