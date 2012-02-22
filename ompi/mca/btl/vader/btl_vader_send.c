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
 * Copyright (c) 2010-2012 Los Alamos National Security, LLC.  
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
 * Initiate a send to the peer.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
int mca_btl_vader_send (struct mca_btl_base_module_t *btl,
                        struct mca_btl_base_endpoint_t *endpoint,
                        struct mca_btl_base_descriptor_t *descriptor,
                        mca_btl_base_tag_t tag)
{
    mca_btl_vader_frag_t *frag = (mca_btl_vader_frag_t *) descriptor;

    if (frag->hdr->flags & MCA_BTL_VADER_FLAG_FBOX) {
        mca_btl_vader_fbox_send (frag->segment.seg_addr.pval, tag, frag->segment.seg_len);

        if (OPAL_LIKELY(frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP)) {
            MCA_BTL_VADER_FRAG_RETURN(frag);
        }

        return 1;
    }

    /* available header space */
    frag->hdr->len = frag->segment.seg_len;
    /* type of message, pt-2-pt, one-sided, etc */
    frag->hdr->tag = tag;

    opal_list_append (&mca_btl_vader_component.active_sends, (opal_list_item_t *) frag);

    /* post the relative address of the descriptor into the peer's fifo */
    vader_fifo_write (frag->hdr, endpoint->peer_smp_rank);

    if (frag->hdr->flags & MCA_BTL_VADER_FLAG_SINGLE_COPY) {
        frag->base.des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
        return 0;
    }

    /* data is gone */
    return 1;
}
