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
 * Copyright (c) 2008-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <infiniband/verbs.h>

#include "opal_stdint.h"

#include "ompi/constants.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/mca/common/verbs/common_verbs.h"

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
ompi_btl_usnic_frag_send_complete(ompi_btl_usnic_module_t *module,
                                    ompi_btl_usnic_send_segment_t *sseg)
{
    ompi_btl_usnic_send_frag_t *frag;

    frag = sseg->ss_parent_frag;

    /* Reap a frag that was sent */
    --sseg->ss_send_posted;
    --frag->sf_seg_post_cnt;

    /* checks for returnability made inside */
    ompi_btl_usnic_send_frag_return_cond(module, frag);

    /* do bookkeeping */
    ++frag->sf_endpoint->endpoint_send_credits;
    ++module->mod_channels[sseg->ss_channel].sd_wqe;

    /* see if this endpoint needs to be made ready-to-send */
    ompi_btl_usnic_check_rts(frag->sf_endpoint);

}

/*
 * This function is called when a send segment completes
 * Return the WQE and also return the segment if no ACK pending
 */
void
ompi_btl_usnic_chunk_send_complete(ompi_btl_usnic_module_t *module,
                                    ompi_btl_usnic_send_segment_t *sseg)
{
    ompi_btl_usnic_send_frag_t *frag;

    frag = sseg->ss_parent_frag;

    /* Reap a frag that was sent */
    --sseg->ss_send_posted;
    --frag->sf_seg_post_cnt;

    if (sseg->ss_send_posted == 0 && !sseg->ss_ack_pending) {
        ompi_btl_usnic_chunk_segment_return(module, sseg);
    }

    /* done with whole fragment? */
    /* checks for returnability made inside */
    ompi_btl_usnic_send_frag_return_cond(module, frag);

    /* do bookkeeping */
    ++frag->sf_endpoint->endpoint_send_credits;
    ++module->mod_channels[sseg->ss_channel].sd_wqe;

    /* see if this endpoint needs to be made ready-to-send */
    ompi_btl_usnic_check_rts(frag->sf_endpoint);
}

/*
 * This routine handles the non-fastpath part of usnic_send().
 * The reason it is here is to prevent it getting inlined with
 * the rest of the function.
 */
int
ompi_btl_usnic_send_slower(
    ompi_btl_usnic_module_t *module,
    ompi_btl_usnic_endpoint_t *endpoint,
    ompi_btl_usnic_send_frag_t *frag,
    mca_btl_base_tag_t tag)
{
    int rc;
    ompi_btl_usnic_small_send_frag_t *sfrag;
    ompi_btl_usnic_send_segment_t *sseg;

    /*
     * If this is small, need to do the copyin now.
     * We don't do this earlier in case we got lucky and were
     * able to do an inline send.  We did not, so here we are...
     */
    if (frag->sf_base.uf_type == OMPI_BTL_USNIC_FRAG_SMALL_SEND) {

        sfrag = (ompi_btl_usnic_small_send_frag_t *)frag;
        sseg = &sfrag->ssf_segment;

        /*
         * copy in user data if there is any, collapsing 2 segments into 1
         */
        if (frag->sf_base.uf_base.des_src_cnt > 1) {

            /* If not convertor, copy now.  Already copied in convertor case */
            if (frag->sf_convertor == NULL) {
                memcpy(((char *)frag->sf_base.uf_src_seg[0].seg_addr.lval +
                         frag->sf_base.uf_src_seg[0].seg_len),
                        frag->sf_base.uf_src_seg[1].seg_addr.pval,
                        frag->sf_base.uf_src_seg[1].seg_len);

            }

            /* update 1st segment length */
            frag->sf_base.uf_base.des_src_cnt = 1;
            frag->sf_base.uf_src_seg[0].seg_len +=
                frag->sf_base.uf_src_seg[1].seg_len;
        }

        /* set up VERBS SG list */
        sseg->ss_send_desc.num_sge = 1;
        sseg->ss_base.us_sg_entry[0].length =
            sizeof(ompi_btl_usnic_btl_header_t) + frag->sf_size;

        /* use standard channel */
        sseg->ss_channel = USNIC_DATA_CHANNEL;
#if MSGDEBUG2
    opal_output(0, "  small frag %d segs %p(%d) + %p(%d)\n",
            (int)frag->sf_base.uf_base.des_src_cnt,
            frag->sf_base.uf_src_seg[0].seg_addr.pval,
            (int)frag->sf_base.uf_src_seg[0].seg_len,
            frag->sf_base.uf_src_seg[1].seg_addr.pval,
            (int)frag->sf_base.uf_src_seg[1].seg_len);
    opal_output(0, "  small seg  %d segs %p(%d) + %p(%d)\n",
            sseg->ss_send_desc.num_sge,
            (void *)sseg->ss_send_desc.sg_list[0].addr,
            sseg->ss_send_desc.sg_list[0].length,
            (void *)sseg->ss_send_desc.sg_list[1].addr,
            sseg->ss_send_desc.sg_list[1].length);
#endif
    }

    /* queue this fragment into the send engine */
    rc = ompi_btl_usnic_endpoint_enqueue_frag(endpoint, frag);
    frag->sf_base.uf_base.des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;

    /* Stats */
    ++(((ompi_btl_usnic_module_t*)module)->pml_module_sends);

    return rc;
}
