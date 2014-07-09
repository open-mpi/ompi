/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2014 Los Alamos National Security, LLC. All rights
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
#include "btl_ugni_prepare.h"

int mca_btl_ugni_send (struct mca_btl_base_module_t *btl,
                       struct mca_btl_base_endpoint_t *endpoint,
                       struct mca_btl_base_descriptor_t *descriptor,
                       mca_btl_base_tag_t tag)
{
    mca_btl_ugni_base_frag_t *frag = (mca_btl_ugni_base_frag_t *) descriptor;
    size_t size = frag->segments[0].base.seg_len + frag->segments[1].base.seg_len;
    mca_btl_ugni_module_t *ugni_module = (mca_btl_ugni_module_t *) btl;
    int flags_save = frag->base.des_flags;
    int rc;

    /* tag and len are at the same location in eager and smsg frag hdrs */
    frag->hdr.send.lag = (tag << 24) | size;

    rc = mca_btl_ugni_check_endpoint_state (endpoint);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        frag->base.des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
        opal_list_append (&endpoint->frag_wait_list, (opal_list_item_t *) frag);
        return OMPI_SUCCESS;
    }

    BTL_VERBOSE(("btl/ugni sending descriptor %p from %d -> %d. length = %" PRIu64, (void *)descriptor,
                 OMPI_PROC_MY_NAME->vpid, endpoint->common->ep_rem_id, frag->segments[0].base.seg_len));

    /* temporarily disable ownership and callback flags so we can reliably check the complete flag */
    frag->base.des_flags &= ~(MCA_BTL_DES_FLAGS_BTL_OWNERSHIP | MCA_BTL_DES_SEND_ALWAYS_CALLBACK);
    frag->flags &= ~MCA_BTL_UGNI_FRAG_COMPLETE;

    rc = mca_btl_ugni_send_frag (endpoint, frag);

    if (OPAL_LIKELY(frag->flags & MCA_BTL_UGNI_FRAG_COMPLETE)) {
        /* fast path: remote side has received the frag */
        frag->base.des_flags = flags_save;
        mca_btl_ugni_frag_complete (frag, OMPI_SUCCESS);

        return 1;
    }

    if ((OMPI_SUCCESS == rc) && (frag->flags & MCA_BTL_UGNI_FRAG_BUFFERED) && (flags_save & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP)) {
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

    if (OPAL_UNLIKELY(OMPI_ERR_OUT_OF_RESOURCE == rc)) {
        /* queue up request */
        if (false == endpoint->wait_listed) {
            opal_list_append (&ugni_module->ep_wait_list, &endpoint->super);
            endpoint->wait_listed = true;
        }

        opal_list_append (&endpoint->frag_wait_list, (opal_list_item_t *) frag);
        rc = OMPI_SUCCESS;
    }

    return rc;
}

int
mca_btl_ugni_sendi (struct mca_btl_base_module_t *btl,
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
        if (OPAL_UNLIKELY(OMPI_SUCCESS != mca_btl_ugni_check_endpoint_state (endpoint))) {
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
        memcpy (frag->segments[0].base.seg_addr.pval, header, header_size);

        rc = mca_btl_ugni_send_frag (endpoint, frag);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            mca_btl_ugni_frag_return (frag);
            break;
        }

        return OMPI_SUCCESS;
    } while (0);

    *descriptor = NULL;
    return OMPI_ERR_OUT_OF_RESOURCE;
}

int mca_btl_ugni_progress_send_wait_list (mca_btl_base_endpoint_t *endpoint)
{
    mca_btl_ugni_base_frag_t *frag;
    int rc;

    while (NULL !=
           (frag = (mca_btl_ugni_base_frag_t *) opal_list_remove_first (&endpoint->frag_wait_list))) {
        rc = mca_btl_ugni_send_frag (endpoint, frag);
        if (OPAL_UNLIKELY(OMPI_SUCCESS > rc)) {
            if (OPAL_LIKELY(OMPI_ERR_OUT_OF_RESOURCE == rc)) {
                opal_list_prepend (&endpoint->frag_wait_list, (opal_list_item_t *) frag);
            } else {
                mca_btl_ugni_frag_complete (frag, rc);
            }

            return rc;
        }
    }

    return OMPI_SUCCESS;
}
