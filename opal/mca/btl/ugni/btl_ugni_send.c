/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_ugni.h"
#include "btl_ugni_frag.h"
#include "btl_ugni_smsg.h"
#include "btl_ugni_prepare.h"

int mca_btl_ugni_send (struct mca_btl_base_module_t *btl,
                       struct mca_btl_base_endpoint_t *endpoint,
                       struct mca_btl_base_descriptor_t *descriptor,
                       mca_btl_base_tag_t tag)
{
    mca_btl_ugni_base_frag_t *frag = (mca_btl_ugni_base_frag_t *) descriptor;
    size_t size = frag->segments[0].seg_len + frag->segments[1].seg_len;
    mca_btl_ugni_module_t *ugni_module = (mca_btl_ugni_module_t *) btl;
    int flags_save = frag->base.des_flags;
    int rc;

    /* tag and len are at the same location in eager and smsg frag hdrs */
    frag->hdr.send.lag = (tag << 24) | size;

    rc = mca_btl_ugni_check_endpoint_state (endpoint);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        frag->base.des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
        OPAL_THREAD_LOCK(&endpoint->lock);
        opal_list_append (&endpoint->frag_wait_list, (opal_list_item_t *) frag);
        OPAL_THREAD_UNLOCK(&endpoint->lock);
        return OPAL_SUCCESS;
    }

    BTL_VERBOSE(("btl/ugni sending descriptor %p from %d -> %d. length = %" PRIu64, (void *)descriptor,
                 OPAL_PROC_MY_NAME.vpid, endpoint->common->ep_rem_id, size));

    /* temporarily disable ownership and callback flags so we can reliably check the complete flag */
    frag->base.des_flags &= ~(MCA_BTL_DES_FLAGS_BTL_OWNERSHIP | MCA_BTL_DES_SEND_ALWAYS_CALLBACK);
    frag->flags &= ~MCA_BTL_UGNI_FRAG_COMPLETE;

    rc = mca_btl_ugni_send_frag (endpoint, frag);

    if (OPAL_LIKELY(frag->flags & MCA_BTL_UGNI_FRAG_COMPLETE)) {
        /* fast path: remote side has received the frag */
        frag->base.des_flags = flags_save;
        mca_btl_ugni_frag_complete (frag, OPAL_SUCCESS);

        return 1;
    }

    if ((OPAL_SUCCESS == rc) && (frag->flags & MCA_BTL_UGNI_FRAG_BUFFERED) && (flags_save & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP)) {
        /* fast(ish) path: btl owned buffered frag. report send as complete */
        frag->base.des_flags = flags_save & ~MCA_BTL_DES_SEND_ALWAYS_CALLBACK;

        if (OPAL_LIKELY(flags_save & MCA_BTL_DES_SEND_ALWAYS_CALLBACK)) {
            frag->base.des_cbfunc(&frag->endpoint->btl->super, frag->endpoint, &frag->base, rc);
        }

        return 1;
    }

    /* slow(ish) path: remote side hasn't received the frag. call the frag's callback when
       we get the local smsg/msgq or remote rdma completion */
    frag->base.des_flags = flags_save | MCA_BTL_DES_SEND_ALWAYS_CALLBACK;

    if (OPAL_UNLIKELY(OPAL_ERR_OUT_OF_RESOURCE == rc)) {
        /* queue up request */
        if (false == endpoint->wait_listed) {
            OPAL_THREAD_LOCK(&ugni_module->ep_wait_list_lock);
            opal_list_append (&ugni_module->ep_wait_list, &endpoint->super);
            OPAL_THREAD_UNLOCK(&ugni_module->ep_wait_list_lock);
            endpoint->wait_listed = true;
        }

        OPAL_THREAD_LOCK(&endpoint->lock);
        opal_list_append (&endpoint->frag_wait_list, (opal_list_item_t *) frag);
        OPAL_THREAD_UNLOCK(&endpoint->lock);
        rc = OPAL_SUCCESS;
    }

    return rc;
}

int mca_btl_ugni_sendi (struct mca_btl_base_module_t *btl,
                        struct mca_btl_base_endpoint_t *endpoint,
                        struct opal_convertor_t *convertor,
                        void *header, size_t header_size,
                        size_t payload_size, uint8_t order,
                        uint32_t flags, mca_btl_base_tag_t tag,
                        mca_btl_base_descriptor_t **descriptor)
{
    size_t total_size = header_size + payload_size;
    mca_btl_ugni_base_frag_t *frag = NULL;
    size_t packed_size = payload_size;
    int rc;

    do {
        if (OPAL_UNLIKELY(OPAL_SUCCESS != mca_btl_ugni_check_endpoint_state (endpoint))) {
            break;
        }

        flags |= MCA_BTL_DES_FLAGS_BTL_OWNERSHIP;

        if (0 == payload_size) {
            frag = (mca_btl_ugni_base_frag_t *) mca_btl_ugni_prepare_src_send_nodata (btl, endpoint, order, header_size,
                                                                                      flags);
        } else {
            frag = (mca_btl_ugni_base_frag_t *) mca_btl_ugni_prepare_src_send_buffered (btl, endpoint, convertor, order,
                                                                                        header_size, &packed_size, flags);
        }

        assert (packed_size == payload_size);
        if (OPAL_UNLIKELY(NULL == frag)) {
            break;
        }

        frag->hdr.send.lag = (tag << 24) | total_size;
        memcpy (frag->segments[0].seg_addr.pval, header, header_size);

        rc = mca_btl_ugni_send_frag (endpoint, frag);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
            mca_btl_ugni_frag_return (frag);
            break;
        }

        return OPAL_SUCCESS;
    } while (0);

    if (NULL != descriptor) {
        *descriptor = NULL;
    }
    return OPAL_ERR_OUT_OF_RESOURCE;
}

int mca_btl_ugni_progress_send_wait_list (mca_btl_base_endpoint_t *endpoint)
{
    mca_btl_ugni_base_frag_t *frag=NULL;
    int rc;

    do {
        OPAL_THREAD_LOCK(&endpoint->lock);
        frag = (mca_btl_ugni_base_frag_t *) opal_list_remove_first (&endpoint->frag_wait_list);
        OPAL_THREAD_UNLOCK(&endpoint->lock);
        if (NULL == frag) {
            break;
        }
        if (OPAL_LIKELY(!(frag->flags & MCA_BTL_UGNI_FRAG_RESPONSE))) {
            rc = mca_btl_ugni_send_frag (endpoint, frag);
        } else {
            rc = opal_mca_btl_ugni_smsg_send (frag, &frag->hdr.rdma, sizeof (frag->hdr.rdma),
                                              NULL, 0, MCA_BTL_UGNI_TAG_RDMA_COMPLETE);
        }

        if (OPAL_UNLIKELY(OPAL_SUCCESS > rc)) {
            if (OPAL_LIKELY(OPAL_ERR_OUT_OF_RESOURCE == rc)) {
                OPAL_THREAD_LOCK(&endpoint->lock);
                opal_list_prepend (&endpoint->frag_wait_list, (opal_list_item_t *) frag);
                OPAL_THREAD_UNLOCK(&endpoint->lock);
            } else {
                mca_btl_ugni_frag_complete (frag, rc);
            }
            return rc;
        }
    } while(1);

    return OPAL_SUCCESS;
}
