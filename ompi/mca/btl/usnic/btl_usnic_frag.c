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
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>

#include "ompi/proc/proc.h"

#include "btl_usnic.h"
#include "btl_usnic_endpoint.h"
#include "btl_usnic_module.h"
#include "btl_usnic_frag.h"
#include "btl_usnic_ack.h"

static void
common_send_seg_helper(
    ompi_btl_usnic_send_segment_t *seg)
{
    ompi_btl_usnic_segment_t *bseg;

    bseg = &seg->ss_base;

    bseg->us_btl_header = (ompi_btl_usnic_btl_header_t *)bseg->us_list.ptr;
    bseg->us_btl_header->sender = mca_btl_usnic_component.my_hashed_rte_name;

    /* build verbs work request descriptor */
    seg->ss_send_desc.wr_id = (unsigned long) seg;
    seg->ss_send_desc.sg_list = bseg->us_sg_entry;
    seg->ss_send_desc.num_sge = 1;
    seg->ss_send_desc.opcode = IBV_WR_SEND;
    seg->ss_send_desc.next = NULL;

    seg->ss_send_desc.send_flags = IBV_SEND_SIGNALED;

    seg->ss_send_posted = 0;
    seg->ss_ack_pending = false;

    /* verbs SG entry, len will be filled in just before send */
    bseg->us_sg_entry[0].addr = (unsigned long) bseg->us_btl_header;
}

static void
chunk_seg_constructor(
    ompi_btl_usnic_send_segment_t *seg)
{
    ompi_btl_usnic_segment_t *bseg;

    bseg = &seg->ss_base;
    bseg->us_type = OMPI_BTL_USNIC_SEG_CHUNK;

    /* some more common initializaiton */
    common_send_seg_helper(seg);
    seg->ss_flags = 0;

    /* payload starts next byte beyond BTL chunk header */
    bseg->us_payload.raw = (uint8_t *)(bseg->us_btl_chunk_header + 1);

    bseg->us_btl_header->payload_type = OMPI_BTL_USNIC_PAYLOAD_TYPE_CHUNK;
}

static void
frag_seg_constructor(
    ompi_btl_usnic_send_segment_t *seg)
{
    ompi_btl_usnic_segment_t *bseg;

    bseg = &seg->ss_base;
    bseg->us_type = OMPI_BTL_USNIC_SEG_FRAG;

    /* some more common initializaiton */
    common_send_seg_helper(seg);
    seg->ss_flags = 0;

    /* payload starts next byte beyond BTL header */
    bseg->us_payload.raw = (uint8_t *)(bseg->us_btl_header + 1);

    bseg->us_btl_header->payload_type = OMPI_BTL_USNIC_PAYLOAD_TYPE_FRAG;
}

static void
ack_seg_constructor(
    ompi_btl_usnic_send_segment_t *ack)
{
    ompi_btl_usnic_segment_t *bseg;

    bseg = &ack->ss_base;
    bseg->us_type = OMPI_BTL_USNIC_SEG_ACK;

    /* some more common initializaiton */
    common_send_seg_helper(ack);

    /* ACK value embedded in BTL header */
    bseg->us_btl_header->payload_type = OMPI_BTL_USNIC_PAYLOAD_TYPE_ACK;
    bseg->us_btl_header->payload_len = 0;

    bseg->us_sg_entry[0].length = sizeof(bseg->us_btl_header);
}


static void
recv_seg_constructor(
    ompi_btl_usnic_recv_segment_t *seg)
{
    ompi_btl_usnic_segment_t *bseg;

    bseg = &seg->rs_base;
    bseg->us_type = OMPI_BTL_USNIC_SEG_RECV;

    /* on receive, BTL header starts after protocol header */
    seg->rs_protocol_header = bseg->us_list.ptr;
    bseg->us_btl_header =
        (ompi_btl_usnic_btl_header_t *) (seg->rs_protocol_header + 1);

    /* initialize verbs work request */
    seg->rs_recv_desc.wr_id = (unsigned long) seg;
    seg->rs_recv_desc.sg_list = bseg->us_sg_entry;
    seg->rs_recv_desc.num_sge = 1;

    /* verbs SG entry, len filled in by caller b/c we don't have value */
    bseg->us_sg_entry[0].addr = (unsigned long) seg->rs_protocol_header;

    /* initialize mca descriptor */
    seg->rs_desc.des_dst = &seg->rs_segment;
    seg->rs_desc.des_dst_cnt = 1;
    seg->rs_desc.des_src = NULL;
    seg->rs_desc.des_src_cnt = 0;

    /* PML want to see its header
     * This pointer is only correct for incoming segments of type
     * OMPI_BTL_USNIC_PAYLOAD_TYPE_FRAG, but that's the only time
     * we ever give segment directly to PML, so its OK
     */
    bseg->us_payload.pml_header = (mca_btl_base_header_t *)
        (bseg->us_btl_header+1);
    seg->rs_segment.seg_addr.pval = bseg->us_payload.pml_header;
}

static void
send_frag_constructor(ompi_btl_usnic_send_frag_t *frag)
{
    mca_btl_base_descriptor_t *desc;

    /* Fill in source descriptor */
    desc = &frag->sf_base.uf_base;
    desc->des_src = frag->sf_base.uf_src_seg;
    desc->des_src_cnt = 2;
    desc->des_dst = frag->sf_base.uf_dst_seg;
    desc->des_dst_cnt = 0;

    desc->order = MCA_BTL_NO_ORDER;
    desc->des_flags = 0;

    frag->sf_seg_post_cnt = 0;
}


static void
small_send_frag_constructor(ompi_btl_usnic_small_send_frag_t *frag)
{
    ompi_btl_usnic_frag_segment_t *fseg;

    /* construct the embedded segment */
    fseg = &frag->ssf_segment;
    /* us_list.ptr is "input" to the constructor, must come before ctor */
    fseg->ss_base.us_list.ptr = frag->ssf_base.sf_base.uf_base.super.ptr;
    OBJ_CONSTRUCT(fseg, ompi_btl_usnic_frag_segment_t);

    /* set us as parent in dedicated frag */
    fseg->ss_parent_frag = (struct ompi_btl_usnic_send_frag_t *)frag;

    frag->ssf_base.sf_base.uf_type = OMPI_BTL_USNIC_FRAG_SMALL_SEND;

    /* save data pointer for PML */
    frag->ssf_base.sf_base.uf_src_seg[0].seg_addr.pval =
        fseg->ss_base.us_payload.raw;
}

static void
small_send_frag_destructor(ompi_btl_usnic_small_send_frag_t *frag)
{
    ompi_btl_usnic_frag_segment_t *fseg;

    fseg = &frag->ssf_segment;
    assert(fseg->ss_parent_frag == (struct ompi_btl_usnic_send_frag_t *)frag);
    assert(frag->ssf_base.sf_base.uf_type == OMPI_BTL_USNIC_FRAG_SMALL_SEND);
    OBJ_DESTRUCT(fseg);
}

static void
large_send_frag_constructor(ompi_btl_usnic_large_send_frag_t *lfrag)
{
    lfrag->lsf_base.sf_base.uf_type = OMPI_BTL_USNIC_FRAG_LARGE_SEND;

    /* save data pointer for PML */
    lfrag->lsf_base.sf_base.uf_src_seg[0].seg_addr.pval =
                    &lfrag->lsf_pml_header;
}

static void
put_dest_frag_constructor(ompi_btl_usnic_put_dest_frag_t *pfrag)
{
    pfrag->uf_type = OMPI_BTL_USNIC_FRAG_PUT_DEST;

    /* point dest to our utility segment */
    pfrag->uf_base.des_dst = pfrag->uf_dst_seg;
    pfrag->uf_base.des_dst_cnt = 1;
}

OBJ_CLASS_INSTANCE(ompi_btl_usnic_segment_t,
                   ompi_free_list_item_t,
                   NULL,
                   NULL);

OBJ_CLASS_INSTANCE(ompi_btl_usnic_frag_segment_t,
                   ompi_btl_usnic_segment_t,
                   frag_seg_constructor,
                   NULL);

OBJ_CLASS_INSTANCE(ompi_btl_usnic_chunk_segment_t,
                   ompi_btl_usnic_segment_t,
                   chunk_seg_constructor,
                   NULL);

OBJ_CLASS_INSTANCE(ompi_btl_usnic_recv_segment_t,
                   ompi_btl_usnic_segment_t,
                   recv_seg_constructor,
                   NULL);

OBJ_CLASS_INSTANCE(ompi_btl_usnic_ack_segment_t,
                   ompi_btl_usnic_segment_t,
                   ack_seg_constructor,
                   NULL);

/*
 * Fragments
 */
OBJ_CLASS_INSTANCE(ompi_btl_usnic_frag_t,
                   mca_btl_base_descriptor_t,
                   NULL,
                   NULL);

OBJ_CLASS_INSTANCE(ompi_btl_usnic_send_frag_t,
                   ompi_btl_usnic_frag_t,
                   send_frag_constructor,
                   NULL);

OBJ_CLASS_INSTANCE(ompi_btl_usnic_large_send_frag_t,
                   ompi_btl_usnic_send_frag_t,
                   large_send_frag_constructor,
                   NULL);

OBJ_CLASS_INSTANCE(ompi_btl_usnic_small_send_frag_t,
                   ompi_btl_usnic_send_frag_t,
                   small_send_frag_constructor,
                   small_send_frag_destructor);

OBJ_CLASS_INSTANCE(ompi_btl_usnic_put_dest_frag_t,
                   ompi_btl_usnic_frag_t,
                   put_dest_frag_constructor,
                   NULL);


/*******************************************************************************/

#if MSGDEBUG
static void dump_ack_frag(ompi_btl_usnic_frag_t* frag)
{
    char out[256];
    memset(out, 0, sizeof(out));

    snprintf(out, sizeof(out),
             "=== ACK frag %p (MCW %d): alloced %d", 
             (void*) frag, 
             ompi_proc_local()->proc_name.vpid,
             FRAG_STATE_ISSET(frag, FRAG_ALLOCED));
    opal_output(0, out);
}

static void dump_send_frag(ompi_btl_usnic_frag_t* frag)
{
    char out[256];
    memset(out, 0, sizeof(out));

    snprintf(out, sizeof(out),
             "=== SEND frag %p (MCW %d): alloced %d send_wr %d acked %d enqueued %d pml_callback %d hotel %d || seq %lu", 
             (void*) frag, 
             ompi_proc_local()->proc_name.vpid,
             FRAG_STATE_ISSET(frag, FRAG_ALLOCED),
             frag->send_wr_posted,
             FRAG_STATE_ISSET(frag, FRAG_SEND_ACKED),
             FRAG_STATE_ISSET(frag, FRAG_SEND_ENQUEUED),
             FRAG_STATE_ISSET(frag, FRAG_PML_CALLED_BACK),
             FRAG_STATE_ISSET(frag, FRAG_IN_HOTEL),
             FRAG_STATE_ISSET(frag, FRAG_ALLOCED) ?
                 frag->btl_header->seq : (ompi_btl_usnic_seq_t) ~0
             );
    opal_output(0, out);
}

static void dump_recv_frag(ompi_btl_usnic_frag_t* frag)
{
    char out[256];
    memset(out, 0, sizeof(out));

    snprintf(out, sizeof(out),
             "=== RECV frag %p (MCW %d): alloced %d posted %d", 
             (void*) frag, 
             ompi_proc_local()->proc_name.vpid,
             FRAG_STATE_ISSET(frag, FRAG_ALLOCED),
             FRAG_STATE_ISSET(frag, FRAG_RECV_WR_POSTED));
    opal_output(0, out);
}

void ompi_btl_usnic_frag_dump(ompi_btl_usnic_frag_t *frag)
{
    switch(frag->type) {
    case OMPI_BTL_USNIC_FRAG_ACK:
        dump_ack_frag(frag);
        break;

    case OMPI_BTL_USNIC_FRAG_SEND:
        dump_send_frag(frag);
        break;

    case OMPI_BTL_USNIC_FRAG_RECV:
        dump_recv_frag(frag);
        break;

    default:
        opal_output(0, "=== UNKNOWN type frag %p: (!)", (void*) frag);
        break;
    }
}
#endif

/*******************************************************************************/

#if HISTORY
void ompi_btl_usnic_frag_history(ompi_btl_usnic_frag_t *frag,
                                   char *file, int line,
                                   const char *message)
{
    int i = frag->history_next;
    ompi_btl_usnic_frag_history_t *h = &(frag->history[i]);

    memset(h, 0, sizeof(*h));
    strncpy(h->file, file, sizeof(h->file));
    h->line = line;
    strncpy(h->message, message, sizeof(h->message));

    frag->history_next = (frag->history_next + 1) % NUM_FRAG_HISTORY;
    if (frag->history_start == frag->history_next) {
        frag->history_start = (frag->history_start + 1) % NUM_FRAG_HISTORY;
    }
}
#endif
