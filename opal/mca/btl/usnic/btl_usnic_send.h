/*
 * Copyright (c) 2013-2016 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef BTL_USNIC_SEND_H
#define BTL_USNIC_SEND_H

#include "btl_usnic.h"
#include "btl_usnic_frag.h"
#include "btl_usnic_ack.h"
#if MSGDEBUG1
#include "btl_usnic_util.h"
#endif

/*
 * Check if conditions are right, and if so, put endpoint on
 * list of endpoints that have sends to be done
 */
static inline void
opal_btl_usnic_check_rts(
    opal_btl_usnic_endpoint_t *endpoint)
{
    /*
     * If endpoint not already ready,
     * and has packets to send,
     * and it has send credits,
     * and its retransmission window is open,
     * make it ready
     */
    if (!endpoint->endpoint_ready_to_send &&
        !opal_list_is_empty(&endpoint->endpoint_frag_send_queue) &&
         endpoint->endpoint_send_credits > 0 &&
         WINDOW_OPEN(endpoint)) {
        opal_list_append(&endpoint->endpoint_module->endpoints_with_sends,
                &endpoint->super);
        endpoint->endpoint_ready_to_send = true;
#if MSGDEBUG1
        opal_output(0, "make endpoint %p RTS\n", (void*)endpoint);
    } else {
        opal_output(0, "rts:%d empty:%d cred:%d open%d\n",
                endpoint->endpoint_ready_to_send,
                opal_list_is_empty(&endpoint->endpoint_frag_send_queue),
                endpoint->endpoint_send_credits,
                WINDOW_OPEN(endpoint));
#endif
    }
}

/*
 * Common point for posting a segment
 */
static inline void
opal_btl_usnic_post_segment(
    opal_btl_usnic_module_t *module,
    opal_btl_usnic_endpoint_t *endpoint,
    opal_btl_usnic_send_segment_t *sseg)
{
    int ret;

    /* get channel and remote channel */
    opal_btl_usnic_channel_id_t channel_id = sseg->ss_channel;
    opal_btl_usnic_channel_t *channel = &module->mod_channels[channel_id];

#if MSGDEBUG1
    opal_output(0, "post_send: type=%s, ep=%p, remote_addr=%p, addr=%p, len=%"
                PRIsize_t,
                usnic_seg_type_str(sseg->ss_base.us_type),
                (void*) channel->ep,
                (void*) endpoint->endpoint_remote_addrs[channel_id],
                (void*) sseg->ss_ptr,
                sseg->ss_len);
#endif

    assert(channel_id == USNIC_DATA_CHANNEL);

    /* Send the segment */
    ret = fi_send(channel->ep,
            sseg->ss_ptr,
            sseg->ss_len + mca_btl_usnic_component.prefix_send_offset,
            NULL,
            endpoint->endpoint_remote_addrs[channel_id],
            sseg);
    if (OPAL_UNLIKELY(0 != ret)) {
        opal_btl_usnic_util_abort("fi_send() failed",
                                  __FILE__, __LINE__);
        /* Never returns */
    }

    /* track # of time non-ACKs are posted */
    if (sseg->ss_base.us_type != OPAL_BTL_USNIC_SEG_ACK) {
        ++sseg->ss_send_posted;
        ++sseg->ss_parent_frag->sf_seg_post_cnt;
    }

    /* Stats */
    ++module->stats.num_total_sends;
    ++channel->num_channel_sends;
    --channel->credits;
}

/*
 * Common point for posting an ACK
 */
static inline void
opal_btl_usnic_post_ack(
    opal_btl_usnic_module_t *module,
    opal_btl_usnic_endpoint_t *endpoint,
    opal_btl_usnic_send_segment_t *sseg)
{
    int ret;

    /* get channel and remote channel */
    opal_btl_usnic_channel_id_t channel_id = sseg->ss_channel;
    opal_btl_usnic_channel_t *channel = &module->mod_channels[channel_id];

#if MSGDEBUG1
    opal_output(0, "post_send ACK: type=%s, ep=%p, remote_addr=%p, addr=%p, len=%"
                PRIsize_t,
                usnic_seg_type_str(sseg->ss_base.us_type),
                (void*) channel->ep,
                (void*) endpoint->endpoint_remote_addrs[channel_id],
                (void*) sseg->ss_ptr,
                sseg->ss_len);
#endif

    assert(channel_id == USNIC_PRIORITY_CHANNEL);

    ret = fi_send(channel->ep,
            sseg->ss_ptr,
            sseg->ss_len + mca_btl_usnic_component.prefix_send_offset,
            NULL,
            endpoint->endpoint_remote_addrs[channel_id],
            sseg);
    if (OPAL_UNLIKELY(0 != ret)) {
        opal_btl_usnic_util_abort("fi_send() failed",
                                  __FILE__, __LINE__);
        /* Never returns */
    }

    /* Stats */
    ++module->stats.num_total_sends;
    ++channel->num_channel_sends;
    --channel->credits;
}

/*
 * Post a send to the work queue
 */
static inline void
opal_btl_usnic_endpoint_send_segment(
    opal_btl_usnic_module_t *module,
    opal_btl_usnic_send_segment_t *sseg)
{
    opal_btl_usnic_send_frag_t *frag;
    opal_btl_usnic_endpoint_t *endpoint;
    uint16_t sfi;

    frag = sseg->ss_parent_frag;
    endpoint = frag->sf_endpoint;

    /* Do we have room in the endpoint's sender window?

       Sender window:

                       |-------- WINDOW_SIZE ----------|
                      +---------------------------------+
                      |         next_seq_to_send        |
                      |     somewhere in this range     |
                     ^+---------------------------------+
                     |
                     +-- ack_seq_rcvd: one less than the window left edge

       Assuming that next_seq_to_send is > ack_seq_rcvd (verified
       by assert), then the good condition to send is:

            next_seq_to_send <= ack_seq_rcvd + WINDOW_SIZE

       And therefore the bad condition is

            next_seq_to_send > ack_seq_rcvd + WINDOW_SIZE
    */
    assert(SEQ_GT(endpoint->endpoint_next_seq_to_send,
                  endpoint->endpoint_ack_seq_rcvd));
    assert(WINDOW_OPEN(endpoint));

    /* Assign sequence number and increment */
    sseg->ss_base.us_btl_header->pkt_seq =
        endpoint->endpoint_next_seq_to_send++;

    /* Fill in remote address to indicate PUT or not */
    sseg->ss_base.us_btl_header->put_addr =
        frag->sf_base.uf_remote_seg[0].seg_addr.pval;

    /* piggy-back an ACK if needed */
    opal_btl_usnic_piggyback_ack(endpoint, sseg);

#if MSGDEBUG1
    {
        char local_ip[32];
        char remote_ip[32];

        opal_btl_usnic_snprintf_ipv4_addr(local_ip, sizeof(local_ip),
                                          module->local_modex.ipv4_addr,
                                          module->local_modex.netmask);
        opal_btl_usnic_snprintf_ipv4_addr(remote_ip, sizeof(remote_ip),
                                          endpoint->endpoint_remote_modex.ipv4_addr,
                                          endpoint->endpoint_remote_modex.netmask);

        opal_output(0, "--> Sending %s: seq: %" UDSEQ ", sender: 0x%016lx from device %s, IP %s, port %u, seg %p, room %d, wc len %u, remote IP %s, port %u",
            (sseg->ss_parent_frag->sf_base.uf_type == OPAL_BTL_USNIC_FRAG_LARGE_SEND)?
                    "CHUNK" : "FRAG",
                    sseg->ss_base.us_btl_header->pkt_seq,
                    sseg->ss_base.us_btl_header->sender,
                    endpoint->endpoint_module->fabric_info->fabric_attr->name,
                    local_ip,
                    module->local_modex.ports[sseg->ss_channel],
                    (void*)sseg,
                    sseg->ss_hotel_room,
                    sseg->ss_ptr,
                    remote_ip,
                    endpoint->endpoint_remote_modex.ports[sseg->ss_channel]);
    }
#endif

    /* do the actual send */
    opal_btl_usnic_post_segment(module, endpoint, sseg);

    /* Track this header by stashing in an array on the endpoint that
       is the same length as the sender's window (i.e., WINDOW_SIZE).
       To find a unique slot in this array, use (seq % WINDOW_SIZE).
     */
    sfi = WINDOW_SIZE_MOD(sseg->ss_base.us_btl_header->pkt_seq);
    endpoint->endpoint_sent_segs[sfi] = sseg;
    sseg->ss_ack_pending = true;

    /* bookkeeping */
    --endpoint->endpoint_send_credits;

    /* Stats */
    if (sseg->ss_parent_frag->sf_base.uf_type
            == OPAL_BTL_USNIC_FRAG_LARGE_SEND) {
        ++module->stats.num_chunk_sends;
    } else {
        ++module->stats.num_frag_sends;
    }

    /* If we have room in the sender's window, we also have room in
       endpoint hotel */
    opal_hotel_checkin_with_res(&endpoint->endpoint_hotel, sseg,
            &sseg->ss_hotel_room);
}

/*
 * This enqueues a fragment send into the system.  A send of a fragment
 * may result in the sending of multiple segments
 */
static inline int
opal_btl_usnic_endpoint_enqueue_frag(
    opal_btl_usnic_endpoint_t *endpoint,
    opal_btl_usnic_send_frag_t *frag)
{
#if MSGDEBUG1
    opal_output(0, "enq_frag: frag=%p, endpoint=%p, %s, len=%lu\n",
            (void*)frag, (void*)endpoint,
            usnic_frag_type(frag->sf_base.uf_type),
            (long unsigned)frag->sf_base.uf_base.des_src->seg_len);
    if (frag->sf_base.uf_type == OPAL_BTL_USNIC_FRAG_LARGE_SEND) {
        opal_btl_usnic_large_send_frag_t *lfrag;
        lfrag = (opal_btl_usnic_large_send_frag_t *)frag;
        opal_output(0, "   large size=%zd\n", lfrag->lsf_base.sf_size);
    }
#endif

    /* add to tail of in-progress list */
    opal_list_append(&endpoint->endpoint_frag_send_queue,
            &frag->sf_base.uf_base.super.super);

    /* possibly make this endpoint ready to send again */
    opal_btl_usnic_check_rts(endpoint);

    return OPAL_SUCCESS;
}

static inline void
opal_btl_usnic_release_send_segment(
    opal_btl_usnic_module_t *module,
    opal_btl_usnic_send_frag_t *frag,
    opal_btl_usnic_send_segment_t *sseg)
{
    /* We only return CHUNK segments because they are the only send-style
     * segments that are dynamically allocated.
     */
    if (sseg->ss_base.us_type == OPAL_BTL_USNIC_SEG_CHUNK) {
        opal_btl_usnic_chunk_segment_return(module, sseg);
    }
}

void opal_btl_usnic_frag_complete(opal_btl_usnic_send_frag_t *frag);

void opal_btl_usnic_frag_send_complete(opal_btl_usnic_module_t *module,
                                    opal_btl_usnic_send_segment_t *sseg);

void opal_btl_usnic_chunk_send_complete(opal_btl_usnic_module_t *module,
                                    opal_btl_usnic_send_segment_t *sseg);

int
opal_btl_usnic_finish_put_or_send(
    opal_btl_usnic_module_t *module,
    opal_btl_usnic_endpoint_t *endpoint,
    opal_btl_usnic_send_frag_t *frag,
    mca_btl_base_tag_t tag)
__opal_attribute_noinline__;

#endif /* BTL_USNIC_SEND_H */
