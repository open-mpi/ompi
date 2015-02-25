/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * Copyright (c) 2008-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal_stdint.h"

#include "opal/constants.h"

#if BTL_IN_OPAL
#include "opal/mca/btl/btl.h"
#include "opal/mca/btl/base/base.h"
#else
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"
#endif

#include "btl_usnic.h"
#include "btl_usnic_frag.h"
#include "btl_usnic_util.h"
#include "btl_usnic_send.h"
#include "btl_usnic_ack.h"


/*
 * This function is called when a send of a full-fragment segment completes
 * Return the WQE and also return the segment if no ACK pending
 */
void
opal_btl_usnic_frag_send_complete(opal_btl_usnic_module_t *module,
                                    opal_btl_usnic_send_segment_t *sseg)
{
    opal_btl_usnic_send_frag_t *frag;

    frag = sseg->ss_parent_frag;

    /* Reap a frag that was sent */
    --sseg->ss_send_posted;
    --frag->sf_seg_post_cnt;

    /* checks for returnability made inside */
    opal_btl_usnic_send_frag_return_cond(module, frag);

    /* do bookkeeping */
    ++frag->sf_endpoint->endpoint_send_credits;

    /* see if this endpoint needs to be made ready-to-send */
    opal_btl_usnic_check_rts(frag->sf_endpoint);
}

/*
 * This function is called when a send segment completes
 * Return the WQE and also return the segment if no ACK pending
 */
void
opal_btl_usnic_chunk_send_complete(opal_btl_usnic_module_t *module,
                                    opal_btl_usnic_send_segment_t *sseg)
{
    opal_btl_usnic_send_frag_t *frag;

    frag = sseg->ss_parent_frag;

    /* Reap a frag that was sent */
    --sseg->ss_send_posted;
    --frag->sf_seg_post_cnt;

    if (sseg->ss_send_posted == 0 && !sseg->ss_ack_pending) {
        opal_btl_usnic_release_send_segment(module, frag, sseg);
    }

    /* done with whole fragment? */
    /* checks for returnability made inside */
    opal_btl_usnic_send_frag_return_cond(module, frag);

    /* do bookkeeping */
    ++frag->sf_endpoint->endpoint_send_credits;

    /* see if this endpoint needs to be made ready-to-send */
    opal_btl_usnic_check_rts(frag->sf_endpoint);
}

/* Responsible for completing non-fastpath parts of a put or send operation,
 * including initializing any large frag bookkeeping fields and enqueuing the
 * frag on the endpoint.
 *
 * This routine lives in this file to help prevent automatic inlining by the
 * compiler.
 *
 * The "tag" only applies to sends.
 */
int
opal_btl_usnic_finish_put_or_send(
    opal_btl_usnic_module_t *module,
    opal_btl_usnic_endpoint_t *endpoint,
    opal_btl_usnic_send_frag_t *frag,
    mca_btl_base_tag_t tag)
{
    int rc;
    opal_btl_usnic_small_send_frag_t *sfrag;
    opal_btl_usnic_send_segment_t *sseg;

    /*
     * If this is small, need to do the copyin now.
     * We don't do this earlier in case we got lucky and were
     * able to do an inline send.  We did not, so here we are...
     */
    if (frag->sf_base.uf_type == OPAL_BTL_USNIC_FRAG_SMALL_SEND) {

        sfrag = (opal_btl_usnic_small_send_frag_t *)frag;
        sseg = &sfrag->ssf_segment;

        /* Copy in user data if there is any, collapsing 2 segments into 1.
         * We already packed via the convertor if necessary, so we only need to
         * handle the simple memcpy case here.
         */
        if (frag->sf_base.uf_base.USNIC_SEND_LOCAL_COUNT > 1) {
            /* no convertor */
            assert(NULL != frag->sf_base.uf_local_seg[1].seg_addr.pval);

            memcpy(((char *)(intptr_t)frag->sf_base.uf_local_seg[0].seg_addr.lval +
                        frag->sf_base.uf_local_seg[0].seg_len),
                    frag->sf_base.uf_local_seg[1].seg_addr.pval,
                    frag->sf_base.uf_local_seg[1].seg_len);

            /* update 1st segment length */
            frag->sf_base.uf_base.USNIC_SEND_LOCAL_COUNT = 1;
            frag->sf_base.uf_local_seg[0].seg_len +=
                frag->sf_base.uf_local_seg[1].seg_len;
        }

        sseg->ss_len = sizeof(opal_btl_usnic_btl_header_t) + frag->sf_size;

        /* use standard channel */
        sseg->ss_channel = USNIC_DATA_CHANNEL;
        sseg->ss_base.us_btl_header->tag = tag;
    } else {
        opal_btl_usnic_large_send_frag_t *lfrag;

        /* Save info about the frag so that future invocations of
         * usnic_handle_large_send can generate segments to put on the wire. */
        lfrag = (opal_btl_usnic_large_send_frag_t *)frag;
        lfrag->lsf_tag = tag;
        lfrag->lsf_cur_offset = 0;
        lfrag->lsf_cur_ptr = lfrag->lsf_des_src[0].seg_addr.pval;
        lfrag->lsf_cur_sge = 0;
        lfrag->lsf_bytes_left_in_sge = lfrag->lsf_des_src[0].seg_len;
        lfrag->lsf_bytes_left = frag->sf_size;

        if (lfrag->lsf_pack_on_the_fly) {
            lfrag->lsf_pack_bytes_left = frag->sf_size;
        } else {
            /* we pre-packed the convertor into a chain in prepare_src */
            lfrag->lsf_pack_bytes_left = 0;
        }
    }

    /* queue this fragment into the send engine */
    rc = opal_btl_usnic_endpoint_enqueue_frag(endpoint, frag);
    return rc;
}
