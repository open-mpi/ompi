/*
 * Copyright (c) 2013-2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef BTL_USNIC_RECV_H
#define BTL_USNIC_RECV_H

#include <infiniband/verbs.h>

#include "btl_usnic.h"
#include "btl_usnic_util.h"
#include "btl_usnic_frag.h"
#include "btl_usnic_proc.h"


void ompi_btl_usnic_recv_call(ompi_btl_usnic_module_t *module,
                              ompi_btl_usnic_recv_segment_t *rseg,
                              ompi_btl_usnic_channel_t *channel,
                              uint32_t l2_bytes_rcvd);

/*
 * Given an incoming segment, lookup the endpoint that sent it
 */
static inline ompi_btl_usnic_endpoint_t *
lookup_sender(ompi_btl_usnic_module_t *module, ompi_btl_usnic_segment_t *seg)
{
    int ret;
    ompi_btl_usnic_endpoint_t *sender;

    /* Use the hashed ORTE process name in the BTL header to uniquely
       identify the sending process (using the MAC/hardware address
       only identifies the sending server -- not the sending ORTE
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
    opal_output(0, "ep: %p, ack_needed = %s\n", (void*)endpoint, endpoint->endpoint_ack_needed?"true":"false");
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

#if MSGDEBUG1
        opal_output(0, "Advance window to %d; next seq to send %" UDSEQ, i,
                    endpoint->endpoint_next_contig_seq_to_recv);
#endif
    }
    endpoint->endpoint_rfstart = i;
}

static inline int
ompi_btl_usnic_check_rx_seq(
    ompi_btl_usnic_endpoint_t *endpoint,
    ompi_btl_usnic_recv_segment_t *seg,
    uint32_t *window_index)
{
    uint32_t i;
    ompi_btl_usnic_seq_t seq;
    int delta;

    /*
     * Handle piggy-backed ACK if present
     */
    if (seg->rs_base.us_btl_header->ack_present) {
#if MSGDEBUG1
        opal_output(0, "Handle piggy-packed ACK seq %"UDSEQ"\n", seg->rs_base.us_btl_header->ack_seq);
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
    seq = seg->rs_base.us_btl_header->pkt_seq;
    delta = SEQ_DIFF(seq, endpoint->endpoint_next_contig_seq_to_recv);
    if (delta < 0 || delta >= WINDOW_SIZE) {
#if MSGDEBUG1
            opal_output(0, "<-- Received FRAG/CHUNK ep %p, seq %" UDSEQ " outside of window (%" UDSEQ " - %" UDSEQ "), %p, module %p -- DROPPED\n",
                        (void*)endpoint, seg->rs_base.us_btl_header->pkt_seq, 
                        endpoint->endpoint_next_contig_seq_to_recv,
                        (endpoint->endpoint_next_contig_seq_to_recv + 
                         WINDOW_SIZE - 1),
                        (void*) seg,
                        (void*) endpoint->endpoint_module);
#endif

        /* Stats */
        if (delta < 0) {
            ++endpoint->endpoint_module->stats.num_oow_low_recvs;
        } else {
            ++endpoint->endpoint_module->stats.num_oow_high_recvs;
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
    i = SEQ_DIFF(seq, endpoint->endpoint_next_contig_seq_to_recv);
    i = WINDOW_SIZE_MOD(i + endpoint->endpoint_rfstart);
    if (endpoint->endpoint_rcvd_segs[i]) {
#if MSGDEBUG1
        opal_output(0, "<-- Received FRAG/CHUNK ep %p, seq %" UDSEQ ", seg %p: duplicate -- DROPPED\n",
            (void*) endpoint, seg->rs_base.us_btl_header->pkt_seq, (void*) seg);
#endif
        /* highest_seq_rcvd is for debug stats only; it's not used
           in any window calculations */
        assert(SEQ_LE(seq, endpoint->endpoint_highest_seq_rcvd));
        /* next_contig_seq_to_recv-1 is the ack number we'll
           send */
        assert (SEQ_GT(seq, endpoint->endpoint_next_contig_seq_to_recv - 1));

        /* Stats */
        ++endpoint->endpoint_module->stats.num_dup_recvs;
        goto dup_needs_ack;
    }

    /* Stats: is this the highest sequence number we've received? */
    if (SEQ_GT(seq, endpoint->endpoint_highest_seq_rcvd)) {
        endpoint->endpoint_highest_seq_rcvd = seq;
    }

    *window_index = i;
    return 0;

dup_needs_ack:
    if (!endpoint->endpoint_ack_needed) {
        ompi_btl_usnic_add_to_endpoints_needing_ack(endpoint);
    }
    return -1;
}

/*
 * We have received a segment, take action based on the 
 * packet type in the BTL header.
 * Try to be fast here - defer as much bookkeeping until later as
 * possible.
 * See README.txt for a discussion of receive fastpath
 */
static inline void
ompi_btl_usnic_recv_fast(ompi_btl_usnic_module_t *module,
                         ompi_btl_usnic_recv_segment_t *seg,
                         ompi_btl_usnic_channel_t *channel,
                         uint32_t l2_bytes_rcvd)
{
    ompi_btl_usnic_segment_t *bseg;
    mca_btl_active_message_callback_t* reg;
    ompi_btl_usnic_seq_t seq;
    ompi_btl_usnic_endpoint_t *endpoint;
    int delta;
    int i;

    bseg = &seg->rs_base;

    /* Find out who sent this segment */
    endpoint = lookup_sender(module, bseg);
    seg->rs_endpoint = endpoint;

    if (endpoint != NULL && !endpoint->endpoint_exiting &&
            (OMPI_BTL_USNIC_PAYLOAD_TYPE_FRAG ==
                bseg->us_btl_header->payload_type) &&
            seg->rs_base.us_btl_header->put_addr == NULL) {

        /* Valgrind help */
        opal_memchecker_base_mem_defined(
                (void*)(seg->rs_recv_desc.sg_list[0].addr),
                seg->rs_recv_desc.sg_list[0].length);

        seq = seg->rs_base.us_btl_header->pkt_seq;
        delta = SEQ_DIFF(seq, endpoint->endpoint_next_contig_seq_to_recv);
        if (delta < 0 || delta >= WINDOW_SIZE) {
            goto drop;
        }

        i = seq - endpoint->endpoint_next_contig_seq_to_recv;
        i = WINDOW_SIZE_MOD(i + endpoint->endpoint_rfstart);
        if (endpoint->endpoint_rcvd_segs[i]) {
            goto drop;
        }

        /* Pass this segment up to the PML.
         * Be sure to get the payload length from the BTL header because
         * the L2 layer may artificially inflate (or otherwise change)
         * the frame length to meet minimum sizes, add protocol information,
         * etc.
         */
        reg = mca_btl_base_active_message_trigger + bseg->us_btl_header->tag;
        seg->rs_segment.seg_len = bseg->us_btl_header->payload_len;
        reg->cbfunc(&module->super, bseg->us_btl_header->tag, 
                    &seg->rs_desc, reg->cbdata);

drop:
        channel->chan_deferred_recv = seg;

    } else {
        ompi_btl_usnic_recv_call(module, seg, channel, l2_bytes_rcvd);
    }
}

/*
 */
static inline int
ompi_btl_usnic_recv_frag_bookkeeping(
    ompi_btl_usnic_module_t* module,
    ompi_btl_usnic_recv_segment_t *seg,
    ompi_btl_usnic_channel_t *channel)
{
    ompi_btl_usnic_endpoint_t* endpoint;
    uint32_t window_index;
    int rc;

    endpoint = seg->rs_endpoint;

    /* Valgrind help */
    opal_memchecker_base_mem_defined(
            (void*)(seg->rs_recv_desc.sg_list[0].addr),
            seg->rs_recv_desc.sg_list[0].length);

    ++module->stats.num_total_recvs;

    /* Do late processing of incoming sequence # */
    rc = ompi_btl_usnic_check_rx_seq(endpoint, seg, &window_index);
    if (OPAL_UNLIKELY(rc != 0)) {
        goto repost;
    }

    ++module->stats.num_frag_recvs;

    ompi_btl_usnic_update_window(endpoint, window_index);

repost:
    /* if endpoint exiting, and all ACKs received, release the endpoint */
    if (endpoint->endpoint_exiting && ENDPOINT_DRAINED(endpoint)) {
        OBJ_RELEASE(endpoint);
    }

    ++module->stats.num_recv_reposts;

    /* Add recv to linked list for reposting */
    seg->rs_recv_desc.next = channel->repost_recv_head;
    channel->repost_recv_head = &seg->rs_recv_desc;

    return rc;
}

/*
 * We have received a segment, take action based on the 
 * packet type in the BTL header
 */
static inline void
ompi_btl_usnic_recv(ompi_btl_usnic_module_t *module,
                    ompi_btl_usnic_recv_segment_t *seg,
                    ompi_btl_usnic_channel_t *channel,
                    uint32_t l2_bytes_rcvd)
{
    ompi_btl_usnic_segment_t *bseg;
    mca_btl_active_message_callback_t* reg;
    ompi_btl_usnic_endpoint_t *endpoint;
    int rc;

    bseg = &seg->rs_base;

    /* Find out who sent this segment */
    endpoint = lookup_sender(module, bseg);
    seg->rs_endpoint = endpoint;

    if (endpoint != NULL && !endpoint->endpoint_exiting &&
            (OMPI_BTL_USNIC_PAYLOAD_TYPE_FRAG ==
                bseg->us_btl_header->payload_type) &&
            seg->rs_base.us_btl_header->put_addr == NULL) {

        MSGDEBUG1_OUT("<-- Received FRAG (fastpath) ep %p, seq %" UDSEQ ", len=%" PRIu16 "\n",
                      (void*) endpoint, bseg->us_btl_header->pkt_seq,
                      bseg->us_btl_header->payload_len);

        if (OPAL_UNLIKELY(ompi_btl_usnic_frag_seg_proto_size(seg) !=
                          l2_bytes_rcvd)) {
            BTL_ERROR(("L2 packet size and segment payload len do not agree!"
                       " l2_bytes_rcvd=%" PRIu32 " expected=%" PRIu32,
                       l2_bytes_rcvd, ompi_btl_usnic_frag_seg_proto_size(seg)));
            abort();
        }

        /* do the receive bookkeeping */
        rc = ompi_btl_usnic_recv_frag_bookkeeping(module, seg, channel);
        if (rc != 0) {
            return;
        }

        /* Pass this segment up to the PML.
         * Be sure to get the payload length from the BTL header because
         * the L2 layer may artificially inflate (or otherwise change)
         * the frame length to meet minimum sizes, add protocol information,
         * etc.
         */
        reg = mca_btl_base_active_message_trigger + bseg->us_btl_header->tag;
        seg->rs_segment.seg_len = bseg->us_btl_header->payload_len;
        reg->cbfunc(&module->super, bseg->us_btl_header->tag,
                    &seg->rs_desc, reg->cbdata);

    } else {
        ompi_btl_usnic_recv_call(module, seg, channel, l2_bytes_rcvd);
    }
}

#endif /* BTL_USNIC_RECV_H */
