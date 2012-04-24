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
#include "btl_ugni_prepare.h"

int mca_btl_ugni_sendi (struct mca_btl_base_module_t *btl,
                        struct mca_btl_base_endpoint_t *endpoint,
                        struct opal_convertor_t *convertor,
                        void *header, size_t header_size,
                        size_t payload_size, uint8_t order,
                        uint32_t flags, mca_btl_base_tag_t tag,
                        mca_btl_base_descriptor_t **descriptor)
{
    const size_t length = header_size + payload_size;
    mca_btl_ugni_base_frag_t *frag;
    uint32_t iov_count = 1;
    struct iovec iov;
    size_t max_data;
    int rc;

    assert (length <= btl->btl_eager_limit && !(flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK));

    if (OPAL_UNLIKELY(OMPI_SUCCESS != mca_btl_ugni_check_endpoint_state (endpoint))) {
        /* can't complete inline send if the endpoint is not already connected */
        /* go ahead and start the connection */
        *descriptor = mca_btl_ugni_alloc (btl, endpoint, order, length, flags);

        return OMPI_ERR_RESOURCE_BUSY;
    }

    max_data = payload_size;
    frag = (mca_btl_ugni_base_frag_t *) mca_btl_ugni_prepare_src_send_buffered (btl, endpoint,
                                                                                convertor,
                                                                                order, header_size,
                                                                                &max_data, flags);
    if (OPAL_UNLIKELY(NULL == frag)) {
        *descriptor = NULL;
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    assert (payload_size == max_data);

    BTL_VERBOSE(("btl/ugni sending inline descriptor %p from %d -> %d. length = %u", (void *) frag,
                 ORTE_PROC_MY_NAME->vpid, endpoint->common->ep_rem_id, (unsigned int) length));

    frag->base.des_flags  = flags | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP;

    frag->hdr.send.lag = (tag << 24) | length;

    /* write match header (with MPI comm/tag/etc. info) */
    memmove (frag->base.des_src[0].seg_addr.pval, header, header_size);

    /* send message */
    if (OPAL_LIKELY(length <= mca_btl_ugni_component.smsg_max_data)) { 
        rc = ompi_mca_btl_ugni_smsg_send (frag, false, &frag->hdr.send_ex, frag->hdr_size,
                                          frag->segments[1].seg_addr.pval, frag->segments[1].seg_len,
                                          MCA_BTL_UGNI_TAG_SEND);
    } else {
        frag->hdr.eager.src_seg = frag->segments[1];
        frag->hdr.eager.ctx     = (void *) frag;

        rc = ompi_mca_btl_ugni_smsg_send (frag, true, &frag->hdr.eager_ex, frag->hdr_size,
                                          NULL, 0, MCA_BTL_UGNI_TAG_GET_INIT);
    }

    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        /* return this frag */
        mca_btl_ugni_frag_return (frag);
    }

    return rc;
}
