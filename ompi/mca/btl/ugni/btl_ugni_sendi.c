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

int mca_btl_ugni_sendi (struct mca_btl_base_module_t *btl,
                        struct mca_btl_base_endpoint_t *endpoint,
                        struct opal_convertor_t *convertor,
                        void *header, size_t header_size,
                        size_t payload_size, uint8_t order,
                        uint32_t flags, mca_btl_base_tag_t tag,
                        mca_btl_base_descriptor_t **descriptor)
{
    size_t length = header_size + payload_size;
    uint32_t msg_id = ORTE_PROC_MY_NAME->vpid;
    mca_btl_ugni_base_frag_t *frag;
    uint32_t iov_count = 1;
    void *data_ptr = NULL;
    struct iovec iov;
    size_t max_data;
    int rc;

    assert (length < mca_btl_ugni_component.eager_limit);
    assert (0 == (flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK));

    if (OPAL_UNLIKELY(OMPI_SUCCESS != mca_btl_ugni_check_endpoint_state (endpoint))) {
        /* can't complete inline send if the endpoint is not already connected */
        /* go ahead and start the connection */
        *descriptor = mca_btl_ugni_alloc (btl, endpoint, order, length, flags);

        return OMPI_ERR_RESOURCE_BUSY;
    }

    MCA_BTL_UGNI_FRAG_ALLOC_EAGER((mca_btl_ugni_module_t *) btl, frag, rc);
    if (OPAL_UNLIKELY(NULL == frag)) {
        *descriptor = NULL;
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    BTL_VERBOSE(("btl/ugni sending inline descriptor %p from %d -> %d. length = %u", (void *) frag,
                 ORTE_PROC_MY_NAME->vpid, endpoint->common->ep_rem_id, (unsigned int) length));

    /* write match header (with MPI comm/tag/etc. info) */
    memcpy (frag->segments[0].seg_addr.pval, header, header_size);

    frag->hdr->tag = tag;
    frag->hdr->len = length;

    /*
      We can add MEMCHECKER calls before and after the packing.
    */
    if (OPAL_UNLIKELY(payload_size && opal_convertor_need_buffers (convertor))) {
        /* pack the data into the supplied buffer */
        iov.iov_base = (IOVBASE_TYPE *)((uintptr_t)frag->segments[0].seg_addr.pval + header_size);
        iov.iov_len  = max_data = payload_size;

        (void) opal_convertor_pack (convertor, &iov, &iov_count, &max_data);

        assert (max_data == payload_size);

        header_size += payload_size;
        payload_size = 0;
    } else if (payload_size) {
        opal_convertor_get_current_pointer (convertor, &data_ptr);
    }

    header_size += sizeof (frag->hdr[0]);

    /* check endpoint state */
    rc = GNI_SmsgSendWTag (endpoint->common->ep_handle, frag->hdr, header_size,
                           data_ptr, payload_size, msg_id,
                           MCA_BTL_UGNI_TAG_SEND);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
        BTL_VERBOSE(("GNI_SmsgSendWTag failed with rc = %d", rc));
        MCA_BTL_UGNI_FRAG_RETURN (frag);
        *descriptor = NULL;

        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    MCA_BTL_UGNI_FRAG_RETURN (frag);

    return OMPI_SUCCESS;
}
