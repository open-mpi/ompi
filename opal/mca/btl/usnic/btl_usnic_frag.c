/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * Copyright (c) 2013-2015 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <string.h>

#include "btl_usnic.h"
#include "btl_usnic_endpoint.h"
#include "btl_usnic_module.h"
#include "btl_usnic_frag.h"
#include "btl_usnic_ack.h"

static void
common_send_seg_helper(
    opal_btl_usnic_send_segment_t *seg,
    int offset)
{
    opal_btl_usnic_segment_t *bseg;

    bseg = &seg->ss_base;

    bseg->us_btl_header = (opal_btl_usnic_btl_header_t *)
        (((char*) bseg->us_list.ptr) + offset);
    bseg->us_btl_header->sender = mca_btl_usnic_component.my_hashed_rte_name;

    seg->ss_send_posted = 0;
    seg->ss_ack_pending = false;

    /* send ptr, len will be filled in just before send */
    seg->ss_ptr = (uint8_t *)bseg->us_btl_header;
}

static void
chunk_seg_constructor(
    opal_btl_usnic_send_segment_t *seg)
{
    opal_btl_usnic_segment_t *bseg;

    bseg = &seg->ss_base;
    bseg->us_type = OPAL_BTL_USNIC_SEG_CHUNK;

    /* some more common initializaiton */
    common_send_seg_helper(seg, mca_btl_usnic_component.transport_header_len);

    /* payload starts next byte beyond BTL chunk header */
    bseg->us_payload.raw = (uint8_t *)(bseg->us_btl_chunk_header + 1);

    bseg->us_btl_header->payload_type = OPAL_BTL_USNIC_PAYLOAD_TYPE_CHUNK;
}

static void
frag_seg_constructor(
    opal_btl_usnic_send_segment_t *seg)
{
    opal_btl_usnic_segment_t *bseg;

    bseg = &seg->ss_base;
    bseg->us_type = OPAL_BTL_USNIC_SEG_FRAG;

    /* some more common initializaiton */
    common_send_seg_helper(seg, mca_btl_usnic_component.transport_header_len);

    /* payload starts next byte beyond BTL header */
    bseg->us_payload.raw = (uint8_t *)(bseg->us_btl_header + 1);

    bseg->us_btl_header->payload_type = OPAL_BTL_USNIC_PAYLOAD_TYPE_FRAG;
}

static void
ack_seg_constructor(
    opal_btl_usnic_send_segment_t *ack)
{
    opal_btl_usnic_segment_t *bseg;

    bseg = &ack->ss_base;
    bseg->us_type = OPAL_BTL_USNIC_SEG_ACK;

    /* some more common initializaiton */
    common_send_seg_helper(ack, mca_btl_usnic_component.transport_header_len);

    /* ACK value embedded in BTL header */
    bseg->us_btl_header->payload_type = OPAL_BTL_USNIC_PAYLOAD_TYPE_ACK;
    bseg->us_btl_header->payload_len = 0;
    bseg->us_btl_header->ack_present = 1;

    ack->ss_len = sizeof(bseg->us_btl_header);
}


static void
recv_seg_constructor(
    opal_btl_usnic_recv_segment_t *seg)
{
    opal_btl_usnic_segment_t *bseg;

    bseg = &seg->rs_base;
    bseg->us_type = OPAL_BTL_USNIC_SEG_RECV;

    /* on receive, BTL header starts after protocol header */
    seg->rs_protocol_header = bseg->us_list.ptr;
    bseg->us_btl_header = (opal_btl_usnic_btl_header_t *)(
        ((char *) seg->rs_protocol_header) +
        mca_btl_usnic_component.transport_header_len);

    /* initialize descriptor */
    /* JMS Initializing RECV_REMOTE for receive frags is unnecessary
       with BTL 3.0.  The only reason to keep this here would be for
       compatibility with the BTL 2.0 usnic-v1.8 git branch (i.e.,
       it's harmless to do this assignment first, before the
       RECV_LOCAL assignments -- the compiler will likely compile out
       this dead code, anyway). */
    seg->rs_desc.USNIC_RECV_REMOTE = NULL;
    seg->rs_desc.USNIC_RECV_REMOTE_COUNT = 0;

    seg->rs_desc.USNIC_RECV_LOCAL = &seg->rs_segment;
    seg->rs_desc.USNIC_RECV_LOCAL_COUNT = 1;

    /*
     * This pointer is only correct for incoming segments of type
     * OPAL_BTL_USNIC_PAYLOAD_TYPE_FRAG, but that's the only time
     * we ever give segment directly to upper layer, so its OK
     */
    bseg->us_payload.ompi_header = (mca_btl_base_header_t *)
        (bseg->us_btl_header+1);
    seg->rs_segment.seg_addr.pval = bseg->us_payload.ompi_header;
}

static void
send_frag_constructor(opal_btl_usnic_send_frag_t *frag)
{
    mca_btl_base_descriptor_t *desc;

    /* Fill in source descriptor */
    desc = &frag->sf_base.uf_base;

    /* JMS Initializing SEND_REMOTE for receive frags is unnecessary
       with BTL 3.0.  The only reason to keep this here would be for
       compatibility with the BTL 2.0 usnic-v1.8 git branch (i.e.,
       it's harmless to do this assignment first, before the
       SEND_LOCAL assignments -- the compiler will likely compile out
       this dead code, anyway). */
    desc->USNIC_SEND_REMOTE = frag->sf_base.uf_remote_seg;
    desc->USNIC_SEND_REMOTE_COUNT = 0;

    desc->USNIC_SEND_LOCAL = frag->sf_base.uf_local_seg;
    frag->sf_base.uf_local_seg[0].seg_len = 0;
    frag->sf_base.uf_local_seg[1].seg_len = 0;
    desc->USNIC_SEND_LOCAL_COUNT = 2;

    desc->order = MCA_BTL_NO_ORDER;
    desc->des_flags = 0;

    OBJ_CONSTRUCT(&frag->sf_convertor, opal_convertor_t);
    frag->sf_seg_post_cnt = 0;
}

static void
send_frag_destructor(opal_btl_usnic_send_frag_t *frag)
{
    mca_btl_base_descriptor_t *desc;

    /* make sure nobody twiddled these values after the constructor */
    desc = &frag->sf_base.uf_base;
    assert(desc->USNIC_SEND_LOCAL == frag->sf_base.uf_local_seg);
    assert(0 == frag->sf_base.uf_local_seg[0].seg_len);

    /* PML may change desc->des_remote to point elsewhere, cannot assert that it
     * still points to our embedded segment */

    OBJ_DESTRUCT(&frag->sf_convertor);
}

static void
small_send_frag_constructor(opal_btl_usnic_small_send_frag_t *frag)
{
    opal_btl_usnic_frag_segment_t *fseg;

    /* construct the embedded segment */
    fseg = &frag->ssf_segment;
    /* us_list.ptr is "input" to the constructor, must come before ctor */
    fseg->ss_base.us_list.ptr = frag->ssf_base.sf_base.uf_base.super.ptr;
    OBJ_CONSTRUCT(fseg, opal_btl_usnic_frag_segment_t);

    /* set us as parent in dedicated frag */
    fseg->ss_parent_frag = (struct opal_btl_usnic_send_frag_t *)frag;

    frag->ssf_base.sf_base.uf_type = OPAL_BTL_USNIC_FRAG_SMALL_SEND;

    /* save data pointer for PML */
    frag->ssf_base.sf_base.uf_local_seg[0].seg_addr.pval =
        fseg->ss_base.us_payload.raw;
}

static void
small_send_frag_destructor(opal_btl_usnic_small_send_frag_t *frag)
{
    opal_btl_usnic_frag_segment_t *fseg;

    fseg = &frag->ssf_segment;
    assert(fseg->ss_parent_frag == (struct opal_btl_usnic_send_frag_t *)frag);
    assert(frag->ssf_base.sf_base.uf_type == OPAL_BTL_USNIC_FRAG_SMALL_SEND);
    assert(frag->ssf_base.sf_base.uf_local_seg[0].seg_addr.pval ==
           fseg->ss_base.us_payload.raw);
    OBJ_DESTRUCT(fseg);
}

static void
large_send_frag_constructor(opal_btl_usnic_large_send_frag_t *lfrag)
{
    lfrag->lsf_base.sf_base.uf_type = OPAL_BTL_USNIC_FRAG_LARGE_SEND;

    /* save data pointer for upper layer */
    lfrag->lsf_base.sf_base.uf_local_seg[0].seg_addr.pval =
                    &lfrag->lsf_ompi_header;

    lfrag->lsf_buffer = NULL;
    OBJ_CONSTRUCT(&lfrag->lsf_seg_chain, opal_list_t);
    lfrag->lsf_pack_on_the_fly = false;
}

static void
put_dest_frag_constructor(opal_btl_usnic_put_dest_frag_t *pfrag)
{
    pfrag->uf_type = OPAL_BTL_USNIC_FRAG_PUT_DEST;

    /* point dest to our utility segment */
    pfrag->uf_base.USNIC_PUT_LOCAL = pfrag->uf_remote_seg;
    pfrag->uf_base.USNIC_PUT_LOCAL_COUNT = 1;
}

static void
put_dest_frag_destructor(opal_btl_usnic_put_dest_frag_t *pfrag)
{
    assert(pfrag->uf_base.USNIC_PUT_LOCAL == pfrag->uf_remote_seg);
    assert(1 == pfrag->uf_base.USNIC_PUT_LOCAL_COUNT);
}

OBJ_CLASS_INSTANCE(opal_btl_usnic_segment_t,
                   opal_free_list_item_t,
                   NULL,
                   NULL);

OBJ_CLASS_INSTANCE(opal_btl_usnic_frag_segment_t,
                   opal_btl_usnic_segment_t,
                   frag_seg_constructor,
                   NULL);

OBJ_CLASS_INSTANCE(opal_btl_usnic_chunk_segment_t,
                   opal_btl_usnic_segment_t,
                   chunk_seg_constructor,
                   NULL);

OBJ_CLASS_INSTANCE(opal_btl_usnic_recv_segment_t,
                   opal_btl_usnic_segment_t,
                   recv_seg_constructor,
                   NULL);

OBJ_CLASS_INSTANCE(opal_btl_usnic_ack_segment_t,
                   opal_btl_usnic_segment_t,
                   ack_seg_constructor,
                   NULL);

/*
 * Fragments
 */
OBJ_CLASS_INSTANCE(opal_btl_usnic_frag_t,
                   mca_btl_base_descriptor_t,
                   NULL,
                   NULL);

OBJ_CLASS_INSTANCE(opal_btl_usnic_send_frag_t,
                   opal_btl_usnic_frag_t,
                   send_frag_constructor,
                   send_frag_destructor);

OBJ_CLASS_INSTANCE(opal_btl_usnic_large_send_frag_t,
                   opal_btl_usnic_send_frag_t,
                   large_send_frag_constructor,
                   NULL);

OBJ_CLASS_INSTANCE(opal_btl_usnic_small_send_frag_t,
                   opal_btl_usnic_send_frag_t,
                   small_send_frag_constructor,
                   small_send_frag_destructor);

OBJ_CLASS_INSTANCE(opal_btl_usnic_put_dest_frag_t,
                   opal_btl_usnic_frag_t,
                   put_dest_frag_constructor,
                   put_dest_frag_destructor);

OBJ_CLASS_INSTANCE(opal_btl_usnic_rx_buf_t,
                   opal_free_list_item_t,
                   NULL,
                   NULL);
