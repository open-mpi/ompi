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

int mca_btl_ugni_sendi (struct mca_btl_base_module_t *btl,
                        struct mca_btl_base_endpoint_t *endpoint,
                        struct opal_convertor_t *convertor,
                        void *header, size_t header_size,
                        size_t payload_size, uint8_t order,
                        uint32_t flags, mca_btl_base_tag_t tag,
                        mca_btl_base_descriptor_t **descriptor)
{
    size_t length = header_size + payload_size;
    mca_btl_ugni_base_frag_t *frag;
    void *data_ptr = NULL;
    size_t max_data;
    int rc;

    assert (length <= btl->btl_eager_limit && !(flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK));

    if (OPAL_UNLIKELY(OMPI_SUCCESS != mca_btl_ugni_check_endpoint_state (endpoint))) {
        /* can't complete inline send if the endpoint is not already connected */
        /* go ahead and start the connection */
        *descriptor = mca_btl_ugni_alloc (btl, endpoint, order, length, flags);

        return OMPI_ERR_RESOURCE_BUSY;
    }

    if (length <= mca_btl_ugni_component.smsg_max_data) {
        rc = MCA_BTL_UGNI_FRAG_ALLOC_SMSG(endpoint, frag);
    } else {
        rc = MCA_BTL_UGNI_FRAG_ALLOC_EAGER_SEND(endpoint, frag);
    }

    if (OPAL_UNLIKELY(NULL == frag)) {
        *descriptor = NULL;
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    BTL_VERBOSE(("btl/ugni sending inline descriptor %p from %d -> %d. length = %u", (void *) frag,
                 ORTE_PROC_MY_NAME->vpid, endpoint->common->ep_rem_id, (unsigned int) length));

    frag->base.des_cbfunc = NULL;
    frag->base.des_flags  = flags | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP;

    frag->segments[0].seg_len = length;

    frag->hdr.send.tag = tag;
    frag->hdr.send.len = length;

    /* write match header (with MPI comm/tag/etc. info) */
    memcpy (frag->segments[0].seg_addr.pval, header, header_size);

    /*
      We can add MEMCHECKER calls before and after the packing.
    */
    if (payload_size) {
        if (OPAL_UNLIKELY(opal_convertor_need_buffers (convertor))) {
            uint32_t iov_count = 1;
            struct iovec iov;

            /* pack the data into the supplied buffer */
            iov.iov_base = (IOVBASE_TYPE *)((uintptr_t)frag->segments[0].seg_addr.pval + header_size);
            iov.iov_len  = max_data = payload_size;

            (void) opal_convertor_pack (convertor, &iov, &iov_count, &max_data);

            assert (max_data == payload_size);
        } else {
            opal_convertor_get_current_pointer (convertor, &data_ptr);
            memmove ((void *)((uintptr_t)frag->segments[0].seg_addr.pval + header_size), data_ptr, payload_size);
        }
    }

    /* send message */
    if (length <= mca_btl_ugni_component.smsg_max_data) {
        return ompi_mca_btl_ugni_smsg_send (frag, false, &frag->hdr.send, sizeof (frag->hdr.send),
                                            frag->segments[0].seg_addr.pval, length, MCA_BTL_UGNI_TAG_SEND);
    } else {
        frag->hdr.eager.src_seg = frag->segments[0];
        frag->hdr.eager.ctx     = (void *) &frag->post_desc;

        return ompi_mca_btl_ugni_smsg_send (frag, true, &frag->hdr.eager, sizeof (frag->hdr.eager),
                                            NULL, 0, MCA_BTL_UGNI_TAG_GET_INIT);
    }
}
