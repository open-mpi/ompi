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
    size_t max_data;
    int rc;

    assert (length <= btl->btl_eager_limit && !(flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK));

    max_data = payload_size;
    frag = (mca_btl_ugni_base_frag_t *)
        mca_btl_ugni_prepare_src_send_buffered (btl, endpoint, convertor,
                                                order, header_size, &max_data,
                                                flags | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);

    if (OPAL_UNLIKELY(NULL == frag || OMPI_SUCCESS != mca_btl_ugni_check_endpoint_state (endpoint))) {
        /* can't complete inline send if the endpoint is not already connected */
        /* go ahead and start the connection */
        if (NULL != frag) {
            mca_btl_ugni_frag_return (frag);
        }

        *descriptor = NULL;

        return !frag ? OMPI_ERR_OUT_OF_RESOURCE : OMPI_ERR_RESOURCE_BUSY;
    }

    assert (payload_size == max_data);

    BTL_VERBOSE(("btl/ugni sending inline descriptor %p from %d -> %d. length = %u", (void *) frag,
                 ORTE_PROC_MY_NAME->vpid, endpoint->common->ep_rem_id, (unsigned int) length));

    frag->hdr.send.lag = (tag << 24) | length;

    /* write match header (with MPI comm/tag/etc. info) */
    memmove (frag->base.des_src[0].seg_addr.pval, header, header_size);

    /* send message */
    rc = mca_btl_ugni_send_frag (endpoint, frag);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        if (OPAL_UNLIKELY(OMPI_ERR_OUT_OF_RESOURCE == rc)) {
            mca_btl_ugni_module_t *ugni_module = (mca_btl_ugni_module_t *) btl;

            /* queue up request */
            if (0 == opal_list_get_size (&endpoint->frag_wait_list)) {
                opal_list_append (&ugni_module->ep_wait_list, &endpoint->super);
            }
            opal_list_append (&endpoint->frag_wait_list, (opal_list_item_t *) frag);
            rc = OMPI_SUCCESS;
        } else {
            /* return this frag */
            mca_btl_ugni_frag_return (frag);
            *descriptor = NULL;
        }
    }

    return rc;
}
