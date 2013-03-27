/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2011 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Voltaire. All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2013 Los Alamos National Security, LLC.  
 *                         All rights reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "btl_vader.h"
#include "btl_vader_frag.h"
#include "btl_vader_fifo.h"

#include "btl_vader_fbox.h"

/**
 * Initiate an inline send to the peer.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
int mca_btl_vader_sendi (struct mca_btl_base_module_t *btl,
                         struct mca_btl_base_endpoint_t *endpoint,
                         struct opal_convertor_t *convertor,
                         void *header, size_t header_size,
                         size_t payload_size, uint8_t order,
                         uint32_t flags, mca_btl_base_tag_t tag,
                         mca_btl_base_descriptor_t **descriptor)
{
    size_t length = (header_size + payload_size);
    mca_btl_vader_frag_t *frag;
    uint32_t iov_count = 1;
    struct iovec iov;
    size_t max_data;
    void *data_ptr = NULL;

    assert (length < mca_btl_vader.super.btl_eager_limit);
    assert (0 == (flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK));

    /* we won't ever return a descriptor */
    *descriptor = NULL;

    if (OPAL_LIKELY((payload_size + header_size) < mca_btl_vader_component.max_inline_send &&
                    !opal_convertor_need_buffers (convertor))) {
        if (payload_size) {
            opal_convertor_get_current_pointer (convertor, &data_ptr);
        }

        if (mca_btl_vader_fbox_sendi (endpoint, tag, header, header_size, data_ptr, payload_size)) {
            return OMPI_SUCCESS;
        }
    }

    /* allocate a fragment, giving up if we can't get one */
    frag = (mca_btl_vader_frag_t *) mca_btl_vader_alloc (btl, endpoint, order, length,
                                                         flags | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
    if (OPAL_UNLIKELY(NULL == frag)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* fill in fragment fields */
    frag->hdr->len = length;
    frag->hdr->tag = tag;

    /* write the match header (with MPI comm/tag/etc. info) */
    memcpy (frag->segments[0].seg_addr.pval, header, header_size);

    /* write the message data if there is any */
    /*
      We can add MEMCHECKER calls before and after the packing.
    */
    /* we can't use single-copy semantics here since as caller will consider the send
       complete if we return success */
    if (OPAL_UNLIKELY(payload_size && opal_convertor_need_buffers (convertor))) {
        /* pack the data into the supplied buffer */
        iov.iov_base = (IOVBASE_TYPE *)((uintptr_t)frag->segments[0].seg_addr.pval + header_size);
        iov.iov_len  = max_data = payload_size;

        (void) opal_convertor_pack (convertor, &iov, &iov_count, &max_data);

        assert (max_data == payload_size);
    } else if (payload_size) {
        /* bypassing the convertor may speed things up a little */
        opal_convertor_get_current_pointer (convertor, &data_ptr);
        memcpy ((void *)((uintptr_t)frag->segments[0].seg_addr.pval + header_size), data_ptr, payload_size);
    }

    opal_list_append (&mca_btl_vader_component.active_sends, (opal_list_item_t *) frag);

    /* write the fragment pointer to peer's the FIFO */
    vader_fifo_write (frag->hdr, endpoint);

    /* the progress function will return the fragment */

    return OMPI_SUCCESS;
}
