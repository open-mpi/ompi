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
#include <unistd.h>

#include "opal_stdint.h"
#include "opal/mca/memchecker/base/base.h"

#include "ompi/constants.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/mca/common/verbs/common_verbs.h"

#include "btl_usnic.h"
#include "btl_usnic_frag.h"
#include "btl_usnic_endpoint.h"
#include "btl_usnic_module.h"
#include "btl_usnic_proc.h"
#include "btl_usnic_ack.h"
#include "btl_usnic_recv.h"
#include "btl_usnic_util.h"


/*
 * Given an incoming segment, lookup the endpoint that sent it
 */
static inline ompi_btl_usnic_endpoint_t *
lookup_sender(ompi_btl_usnic_module_t *module, ompi_btl_usnic_segment_t *seg)
{
    int ret;
    ompi_btl_usnic_endpoint_t *sender;

    /* Use the hashed RTE process name in the BTL header to uniquely
       identify the sending process (using the MAC/hardware address
       only identifies the sending server -- not the sending RTE
       process). */
    /* JMS We've experimented with using a handshake before sending
       any data so that instead of looking up a hash on the
       btl_header->sender, echo back the ptr to the sender's
       ompi_proc.  There was limited speedup with this scheme; more
       investigation is required. */
    ret = opal_hash_table_get_value_uint64(&module->senders, 
                                           seg->us_btl_header->sender,
                                           (void**) &sender);
    if (OPAL_LIKELY(OPAL_SUCCESS == ret)) {
        return sender;
    }

    /* The sender wasn't in the hash table, so do a slow lookup and
       put the result in the hash table */
    sender = ompi_btl_usnic_proc_lookup_endpoint(module, 
                                                 seg->us_btl_header->sender);
    if (NULL != sender) {
        opal_hash_table_set_value_uint64(&module->senders, 
                                         seg->us_btl_header->sender, sender);
        return sender;
    }

    /* Whoa -- not found at all! */
    return NULL;
}

static inline int
ompi_btl_usnic_check_rx_seq(
    ompi_btl_usnic_endpoint_t *endpoint,
    ompi_btl_usnic_recv_segment_t *seg,
    uint32_t *window_index)
{
    uint32_t i;
    ompi_btl_usnic_seq_t seq;

    /*
     * Handle piggy-backed ACK if present
     */
    if (seg->rs_base.us_btl_header->ack_seq != 0) {
#if MSGDEBUG1
        opal_output(0, "Handle piggy-packed ACK seq %d\n", seg->rs_base.us_btl_header->ack_seq);
#endif
        ompi_btl_usnic_handle_ack(endpoint,
                seg->rs_base.us_btl_header->ack_seq);
    }

    /* Do we have room in the endpoint's receiver window?
           
       Receiver window:

                   |-------- WINDOW_SIZE ----------|
                  +---------------------------------+
                  |         highest_seq_rcvd        |
                  |     somewhere in this range     |
                  +^--------------------------------+
                   |
                   +-- next_contig_seq_to_recv: the window left edge;
                       will always be less than highest_seq_rcvd

       The good condition is 

         next_contig_seq_to_recv <= seq < next_contig_seq_to_recv + WINDOW_SIZE

       And the bad condition is

         seq < next_contig_seq_to_recv
           or
         seq >= next_contig_seg_to_recv + WINDOW_SIZE
    */
    seq = seg->rs_base.us_btl_header->seq;
    if (seq < endpoint->endpoint_next_contig_seq_to_recv ||
        seq >= endpoint->endpoint_next_contig_seq_to_recv + WINDOW_SIZE) {
#if MSGDEBUG
            opal_output(0, "<-- Received FRAG/CHUNK ep %p, seq %" UDSEQ " outside of window (%" UDSEQ " - %" UDSEQ "), %p, module %p -- DROPPED\n",
                        (void*)endpoint, seg->rs_base.us_btl_header->seq, 
                        endpoint->endpoint_next_contig_seq_to_recv,
                        (endpoint->endpoint_next_contig_seq_to_recv + 
                         WINDOW_SIZE - 1),
                        (void*) seg,
                        (void*) endpoint->endpoint_module);
#endif

        /* Stats */
        if (seq < endpoint->endpoint_next_contig_seq_to_recv) {
            ++endpoint->endpoint_module->num_oow_low_recvs;
        } else {
            ++endpoint->endpoint_module->num_oow_high_recvs;
        }
        goto dup_needs_ack;
    }

    /* Ok, this segment is within the receiver window.  Have we
       already received it?  It's possible that the sender has
       re-sent a segment that we've already received (but not yet
       ACKed).

       We have saved all un-ACKed segment in an array on the
       endpoint that is the same legnth as the receiver's window
       (i.e., WINDOW_SIZE).  We can use the incoming segment sequence
       number to find its position in the array.  It's a little
       tricky because the left edge of the receiver window keeps
       moving, so we use a starting reference point in the array
       that is updated when we sent ACKs (and therefore move the
       left edge of the receiver's window).

       So this segment's index into the endpoint array is:

           rel_posn_in_recv_win = seq - next_contig_seq_to_recv
           array_posn = (rel_posn_in_recv_win + rfstart) % WINDOW_SIZE
       
       rfstart is then updated when we send ACKs:

           rfstart = (rfstart + num_acks_sent) % WINDOW_SIZE
    */
    i = seq - endpoint->endpoint_next_contig_seq_to_recv;
    i = WINDOW_SIZE_MOD(i + endpoint->endpoint_rfstart);
    if (endpoint->endpoint_rcvd_segs[i]) {
#if MSGDEBUG
        opal_output(0, "<-- Received FRAG/CHUNK ep %p, seq %" UDSEQ " from %s to %s, seg %p: duplicate -- DROPPED\n",
            (void*) endpoint, bseg->us_btl_header->seq, src_mac, dest_mac,
            (void*) seg);
#endif
        /* highest_seq_rcvd is for debug stats only; it's not used
           in any window calculations */
        assert(seq <= endpoint->endpoint_highest_seq_rcvd);
        /* next_contig_seq_to_recv-1 is the ack number we'll
           send */
        assert (seq > endpoint->endpoint_next_contig_seq_to_recv - 1);

        /* Stats */
        ++endpoint->endpoint_module->num_dup_recvs;
        goto dup_needs_ack;
    }

    /* Stats: is this the highest sequence number we've received? */
    if (seq > endpoint->endpoint_highest_seq_rcvd) {
        endpoint->endpoint_highest_seq_rcvd = seq;
    }

    *window_index = i;
    return true;

dup_needs_ack:
    if (!endpoint->endpoint_ack_needed) {
        ompi_btl_usnic_add_to_endpoints_needing_ack(endpoint);
    }
    return false;
}

/*
 * Packet has been fully processed, update the receive window
 * to indicate that it and possible following contiguous sequence
 * numbers have been received.
 */
static inline void
ompi_btl_usnic_update_window(
    ompi_btl_usnic_endpoint_t *endpoint,
    uint32_t window_index)
{
    uint32_t i;

    /* Enable ACK reply if not enabled */
#if MSGDEBUG1
    opal_output(0, "ep: %p, ack_needed = %s\n", endpoint, endpoint->endpoint_ack_needed?"true":"false");
#endif
    if (!endpoint->endpoint_ack_needed) {
        ompi_btl_usnic_add_to_endpoints_needing_ack(endpoint);
    }

    /* give this process a chance to send something before ACKing */
    if (0 == endpoint->endpoint_acktime) {
        endpoint->endpoint_acktime = get_nsec() + 50000;    /* 50 usec */
    }

    /* Save this incoming segment in the received segmentss array on the
       endpoint. */
    /* JMS Another optimization: make rcvd_segs be a bitmask (i.e.,
       more cache friendly) */
    endpoint->endpoint_rcvd_segs[window_index] = true;

    /* See if the leftmost segment in the receiver window is
       occupied.  If so, advance the window.  Repeat until we hit
       an unoccupied position in the window. */
    i = endpoint->endpoint_rfstart;
    while (endpoint->endpoint_rcvd_segs[i]) {
        endpoint->endpoint_rcvd_segs[i] = false;
        endpoint->endpoint_next_contig_seq_to_recv++;
        i = WINDOW_SIZE_MOD(i + 1);

#if MSGDEBUG
        opal_output(0, "Advance window to %d; next seq to send %" UDSEQ, i,
                    endpoint->endpoint_next_contig_seq_to_recv);
#endif
    }
    endpoint->endpoint_rfstart = i;
}

/*
 * We have received a segment, take action based on the 
 * packet type in the BTL header
 */
void ompi_btl_usnic_recv(ompi_btl_usnic_module_t *module,
                           ompi_btl_usnic_recv_segment_t *seg,
                           struct ibv_recv_wr **repost_recv_head)
{
    ompi_btl_usnic_segment_t *bseg;
    mca_btl_active_message_callback_t* reg;
    ompi_btl_usnic_endpoint_t *endpoint;
    ompi_btl_usnic_btl_chunk_header_t *chunk_hdr;
    uint32_t window_index;
#if MSGDEBUG1
    char src_mac[32];
    char dest_mac[32];
#endif

    bseg = &seg->rs_base;

    ++module->num_total_recvs;

    /* Valgrind help */
    opal_memchecker_base_mem_defined((void*)(seg->rs_recv_desc.sg_list[0].addr),
                                     seg->rs_recv_desc.sg_list[0].length);

#if MSGDEBUG1
    memset(src_mac, 0, sizeof(src_mac));
    memset(dest_mac, 0, sizeof(dest_mac));
    ompi_btl_usnic_sprintf_gid_mac(src_mac,
            &seg->rs_protocol_header->grh.sgid);
    ompi_btl_usnic_sprintf_gid_mac(dest_mac, 
            &seg->rs_protocol_header->grh.dgid);

#if MSGDEBUG
    opal_output(0, "Got message from MAC %s", src_mac);
    opal_output(0, "Looking for sender: 0x%016lx",
        bseg->us_btl_header->sender);
#endif
#endif

    /* Find out who sent this segment */
    endpoint = lookup_sender(module, bseg);
    seg->rs_endpoint = endpoint;
    if (FAKE_RECV_FRAG_DROP || OPAL_UNLIKELY(NULL == endpoint)) {
        /* No idea who this was from, so drop it */
#if MSGDEBUG1
        opal_output(0, "=== Unknown sender; dropped: from MAC %s to MAC %s, seq %" UDSEQ, 
                    src_mac, 
                    dest_mac, 
                    bseg->us_btl_header->seq);
#endif
        ++module->num_unk_recvs;
        goto repost_no_endpoint;
    }

    /***********************************************************************/
    /* Segment is an incoming frag */
    if (OMPI_BTL_USNIC_PAYLOAD_TYPE_FRAG == bseg->us_btl_header->payload_type) {

        /* Is incoming sequence # ok? */
        if (!ompi_btl_usnic_check_rx_seq(endpoint, seg, &window_index)) {
            goto repost;
        }

#if MSGDEBUG1
        opal_output(0, "<-- Received FRAG ep %p, seq %" UDSEQ ", len=%d\n",
                    (void*) endpoint,
                    seg->rs_base.us_btl_header->seq,
                    seg->rs_base.us_btl_header->payload_len);
#if 0

        opal_output(0, "<-- Received FRAG ep %p, seq %" UDSEQ " from %s to %s: GOOD! (rel seq %d, lowest seq %" UDSEQ ", highest seq: %" UDSEQ ", rwstart %d) seg %p, module %p\n",
                    (void*) endpoint,
                    seg->rs_base.us_btl_header->seq, 
                    src_mac, dest_mac,
                    window_index,
                    endpoint->endpoint_next_contig_seq_to_recv,
                    endpoint->endpoint_highest_seq_rcvd,
                    endpoint->endpoint_rfstart,
                    (void*) seg, (void*) module);
        if (seg->rs_base.us_btl_header->put_addr != NULL) {
            opal_output(0, "  put_addr = %p\n",
                    seg->rs_base.us_btl_header->put_addr);
        }
#endif
#endif

        /*
         * update window before callback because callback might
         * generate a send, and we'd like to piggy-back ACK if possible
         */
        ompi_btl_usnic_update_window(endpoint, window_index);

        /* Stats */
        ++module->num_frag_recvs;

        /* If this it not a PUT, Pass this segment up to the PML.
         * Be sure to get the payload length from the BTL header because
         * the L2 layer may artificially inflate (or otherwise change)
         * the frame length to meet minimum sizes, add protocol information,
         * etc.
         */
        if (seg->rs_base.us_btl_header->put_addr == NULL) {
            reg = mca_btl_base_active_message_trigger +
                bseg->us_payload.pml_header->tag;
            seg->rs_segment.seg_len = bseg->us_btl_header->payload_len;
            reg->cbfunc(&module->super, bseg->us_payload.pml_header->tag, 
                        &seg->rs_desc, reg->cbdata);

        /*
         * If this is a PUT, need to copy it to user buffer
         */
        } else {
#if MSGDEBUG1
            opal_output(0, "Copy %d PUT bytes to %p\n", 
                seg->rs_base.us_btl_header->payload_len,
                chunk_hdr->ch_hdr.put_addr);
#endif
            memcpy(seg->rs_base.us_btl_header->put_addr,
                    seg->rs_base.us_payload.raw,
                    seg->rs_base.us_btl_header->payload_len);
        }

        goto repost;
    }

    /***********************************************************************/
    /* Segment is an incoming chunk */
    if (OMPI_BTL_USNIC_PAYLOAD_TYPE_CHUNK == bseg->us_btl_header->payload_type) {
        int frag_index;
        ompi_btl_usnic_rx_frag_info_t *fip;

        /* Is incoming sequence # ok? */
        if (!ompi_btl_usnic_check_rx_seq(endpoint, seg, &window_index)) {
            goto repost;
        }

#if MSGDEBUG1
        opal_output(0, "<-- Received CHUNK fid %d ep %p, seq %" UDSEQ " from %s to %s: GOOD! (rel seq %d, lowest seq %" UDSEQ ", highest seq: %" UDSEQ ", rwstart %d) seg %p, module %p\n",
                    seg->rs_base.us_btl_chunk_header->ch_frag_id,
                    (void*) endpoint,
                    seg->rs_base.us_btl_chunk_header->ch_hdr.seq, 
                    src_mac, dest_mac,
                    window_index,
                    endpoint->endpoint_next_contig_seq_to_recv,
                    endpoint->endpoint_highest_seq_rcvd,
                    endpoint->endpoint_rfstart,
                    (void*) seg, (void*) module);
#endif

        /* start a new fragment if not one in progress
         * alloc memory, etc.  when last byte arrives, dealloc the
         * frag_id and pass data to PML
         */
        chunk_hdr = seg->rs_base.us_btl_chunk_header;
        frag_index = chunk_hdr->ch_frag_id % MAX_ACTIVE_FRAGS;
        fip = &(endpoint->endpoint_rx_frag_info[frag_index]);

        /* frag_id == 0 means this slot it empty, grab it! */
        if (0 == fip->rfi_frag_id) {
            fip->rfi_frag_id = chunk_hdr->ch_frag_id;
            fip->rfi_frag_size = chunk_hdr->ch_frag_size;
            if (chunk_hdr->ch_hdr.put_addr == NULL) {
                int pool;

                fip->rfi_data = NULL;

                /* See which data pool this should come from,
                 * or if it should be malloc()ed
                 */
                pool = fls(chunk_hdr->ch_frag_size-1);
                if (pool >= module->first_pool &&
                        pool <= module->last_pool) {
                    ompi_free_list_item_t* item;
                    OMPI_FREE_LIST_GET_MT(&module->module_recv_buffers[pool],
                                          item);
                    if (OPAL_LIKELY(NULL != item)) {
                        fip->rfi_data = (char *)item;
                        fip->rfi_data_pool = pool;
                    }
                }
                if (fip->rfi_data == NULL) {
                    fip->rfi_data = malloc(chunk_hdr->ch_frag_size);
                    fip->rfi_data_pool = 0;
                }
                if (fip->rfi_data == NULL) {
                    abort();
                }
#if MSGDEBUG2
opal_output(0, "Start large recv to %p, size=%d\n",
        fip->rfi_data, chunk_hdr->ch_frag_size);
#endif
            } else {
#if MSGDEBUG2
opal_output(0, "Start PUT to %p\n", chunk_hdr->ch_hdr.put_addr);
#endif
                fip->rfi_data = chunk_hdr->ch_hdr.put_addr;
            }
            fip->rfi_bytes_left = chunk_hdr->ch_frag_size;
            fip->rfi_frag_id = chunk_hdr->ch_frag_id;

        /* frag_id is not 0 - it must match, drop if not */
        } else if (fip->rfi_frag_id != chunk_hdr->ch_frag_id) {
            ++module->num_badfrag_recvs;
            goto repost;
        }
#if MSGDEBUG1
        opal_output(0, "put_addr=%p, copy_addr=%p, off=%d\n",
                chunk_hdr->ch_hdr.put_addr,
                fip->rfi_data+chunk_hdr->ch_frag_offset,
                chunk_hdr->ch_frag_offset);
#endif

        /* Stats */
        ++module->num_chunk_recvs;

        /* validate offset and len to be within fragment */
        assert(chunk_hdr->ch_frag_offset + chunk_hdr->ch_hdr.payload_len <=
                fip->rfi_frag_size);
        assert(fip->rfi_frag_size == chunk_hdr->ch_frag_size);

        /* copy the data into place */
        memcpy(fip->rfi_data + chunk_hdr->ch_frag_offset, (char *)(chunk_hdr+1),
                chunk_hdr->ch_hdr.payload_len);

        /* update sliding window */
        ompi_btl_usnic_update_window(endpoint, window_index);

        fip->rfi_bytes_left -= chunk_hdr->ch_hdr.payload_len;
        if (0 == fip->rfi_bytes_left) {
            mca_btl_base_header_t *pml_header;
            mca_btl_base_descriptor_t desc;
            mca_btl_base_segment_t segment;

            /* Get access to PML header in assembled fragment so we
             * can pull out the tag
             */
            pml_header = (mca_btl_base_header_t *)(fip->rfi_data);
            segment.seg_addr.pval = pml_header;
            segment.seg_len = fip->rfi_frag_size;
            desc.des_dst = &segment;
            desc.des_dst_cnt = 1;

            /* only up to PML if this was not a put */
            if (chunk_hdr->ch_hdr.put_addr == NULL) {

                /* Pass this segment up to the PML */
#if MSGDEBUG2
                opal_output(0, "  large FRAG complete, pass up %p, %d bytes, tag=%d\n",
                        desc.des_dst->seg_addr.pval, desc.des_dst->seg_len,
                        pml_header->tag);
#endif
                reg = mca_btl_base_active_message_trigger + pml_header->tag;

                /* mca_pml_ob1_recv_frag_callback_frag() */
                reg->cbfunc(&module->super, pml_header->tag,
                        &desc, reg->cbdata);

                /* free temp buffer for non-put */
                if (0 == fip->rfi_data_pool) {
                    free(fip->rfi_data);
                } else {
                    OMPI_FREE_LIST_RETURN_MT(
                            &module->module_recv_buffers[fip->rfi_data_pool],
                            (ompi_free_list_item_t *)fip->rfi_data);
                }

#if MSGDEBUG2
            } else {
                opal_output(0, "PUT complete, suppressing callback\n");
#endif
            }

            /* release the fragment ID */
            fip->rfi_frag_id = 0;

            /* force immediate ACK */
            endpoint->endpoint_acktime = 0;
        }
        goto repost;
    }

    /***********************************************************************/
    /* Frag is an incoming ACK */
    else if (OPAL_LIKELY(OMPI_BTL_USNIC_PAYLOAD_TYPE_ACK == 
                         bseg->us_btl_header->payload_type)) {
        ompi_btl_usnic_seq_t ack_seq;

        /* sequence being ACKed */
        ack_seq = bseg->us_btl_header->ack_seq;

        /* Stats */
        ++module->num_ack_recvs;

#if MSGDEBUG1
        opal_output(0, "    Received ACK for sequence number %" UDSEQ " from %s to %s\n",
                    bseg->us_btl_header->ack_seq, src_mac, dest_mac);
#endif
        ompi_btl_usnic_handle_ack(endpoint, ack_seq);

        goto repost;
    }

    /***********************************************************************/
    /* Have no idea what the frag is; drop it */
    else {
        ++module->num_unk_recvs;
        opal_output(0, "==========================unknown 2");
        goto repost;
    }

    /***********************************************************************/
 repost:

    /* if endpoint exiting, and all ACKs received, release the endpoint */
    if (endpoint->endpoint_exiting && ENDPOINT_DRAINED(endpoint)) {
        OBJ_RELEASE(endpoint);
    }
 repost_no_endpoint:
    ++module->num_recv_reposts;

    /* Add recv to linked list for reposting */
    seg->rs_recv_desc.next = *repost_recv_head;
    *repost_recv_head = &seg->rs_recv_desc;
}
