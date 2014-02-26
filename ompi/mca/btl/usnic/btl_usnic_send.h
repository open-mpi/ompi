/*
 * Copyright (c) 2013-2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef BTL_USNIC_SEND_H
#define BTL_USNIC_SEND_H

#include <infiniband/verbs.h>

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
ompi_btl_usnic_check_rts(
    ompi_btl_usnic_endpoint_t *endpoint)
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

#if MSGDEBUG2
static inline
int sge_total(struct ibv_send_wr *wr)
{
    int i;
    int len;
    len=0;
    for (i=0; i<wr->num_sge; ++i) {
        len += wr->sg_list[i].length;
    }

    return len;
}
#endif

/*
 * Common point for posting a segment to VERBS
 */
static inline void
ompi_btl_usnic_post_segment(
    ompi_btl_usnic_module_t *module,
    ompi_btl_usnic_endpoint_t *endpoint,
    ompi_btl_usnic_send_segment_t *sseg)
{
    struct ibv_send_wr *bad_wr;
    ompi_btl_usnic_channel_t *channel;
    struct ibv_send_wr *wr;
    int ret;

#if MSGDEBUG1
    opal_output(0, "post_send: type=%s, addr=%p, len=%d, payload=%d\n",
                usnic_seg_type(sseg->ss_base.us_type),
                (void*) sseg->ss_send_desc.sg_list->addr, 
                sge_total(&sseg->ss_send_desc),
                sseg->ss_base.us_btl_header->payload_len);
    /*ompi_btl_usnic_dump_hex((void *)(sseg->ss_send_desc.sg_list->addr + sizeof(ompi_btl_usnic_btl_header_t)), 16); */
#endif

    /* set target address */
    wr = &sseg->ss_send_desc;
    wr->wr.ud.ah = endpoint->endpoint_remote_ah;

    /* get channel and remote QPN */
    channel = &module->mod_channels[sseg->ss_channel];
    wr->wr.ud.remote_qpn =
        endpoint->endpoint_remote_addr.qp_num[sseg->ss_channel];

    /* Post segment to the NIC */
    ret = ibv_post_send(channel->qp, &sseg->ss_send_desc, &bad_wr);
    if (OPAL_UNLIKELY(0 != ret)) {
        ompi_btl_usnic_util_abort("ibv_post_send() failed", 
                                  __FILE__, __LINE__, ret);
        /* Never returns */
    }

    /* track # of time non-ACKs are posted */
    if (sseg->ss_base.us_type != OMPI_BTL_USNIC_SEG_ACK) {
        ++sseg->ss_send_posted;
        ++sseg->ss_parent_frag->sf_seg_post_cnt;
    }

    /* consume a WQE */
    --channel->sd_wqe;

    /* Stats */
    ++module->stats.num_total_sends;
    ++channel->num_channel_sends;
}
/*
 * Post a send to the verbs work queue
 */
static inline void
ompi_btl_usnic_endpoint_send_segment(
    ompi_btl_usnic_module_t *module,
    ompi_btl_usnic_send_segment_t *sseg)
{
    ompi_btl_usnic_send_frag_t *frag;
    ompi_btl_usnic_endpoint_t *endpoint;
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
        frag->sf_base.uf_dst_seg[0].seg_addr.pval;

    /* piggy-back an ACK if needed */
    ompi_btl_usnic_piggyback_ack(endpoint, sseg);

#if MSGDEBUG1
    {
        uint8_t mac[6];
    char mac_str1[128];
    char mac_str2[128];
    ompi_btl_usnic_sprintf_mac(mac_str1, module->local_addr.mac);
        ompi_btl_usnic_gid_to_mac(&endpoint->endpoint_remote_addr.gid, mac);
    ompi_btl_usnic_sprintf_mac(mac_str2, mac);

        opal_output(0, "--> Sending %s: seq: %" UDSEQ ", sender: 0x%016lx from device %s MAC %s, qp %u, seg %p, room %d, wc len %u, remote MAC %s, qp %u",
            (sseg->ss_parent_frag->sf_base.uf_type == OMPI_BTL_USNIC_FRAG_LARGE_SEND)?
                "CHUNK" : "FRAG",
            sseg->ss_base.us_btl_header->pkt_seq,
            sseg->ss_base.us_btl_header->sender, 
            endpoint->endpoint_module->device->name,
            mac_str1, module->local_addr.qp_num[sseg->ss_channel],
            (void*)sseg, sseg->ss_hotel_room,
            sseg->ss_base.us_sg_entry[0].length,
            mac_str2, endpoint->endpoint_remote_addr.qp_num[sseg->ss_channel]);
    }
#endif

    /* do the actual send */
    ompi_btl_usnic_post_segment(module, endpoint, sseg);

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
            == OMPI_BTL_USNIC_FRAG_LARGE_SEND) {
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
ompi_btl_usnic_endpoint_enqueue_frag(
    ompi_btl_usnic_endpoint_t *endpoint,
    ompi_btl_usnic_send_frag_t *frag)
{
#if MSGDEBUG1
    opal_output(0, "enq_frag: frag=%p, endpoint=%p, %s, len=%lu\n",
            (void*)frag, (void*)endpoint,
            usnic_frag_type(frag->sf_base.uf_type),
            (long unsigned)frag->sf_base.uf_base.des_src->seg_len);
    if (frag->sf_base.uf_type == OMPI_BTL_USNIC_FRAG_LARGE_SEND) {
        ompi_btl_usnic_large_send_frag_t *lfrag;
        lfrag = (ompi_btl_usnic_large_send_frag_t *)frag;
        opal_output(0, "   large size=%zd\n", lfrag->lsf_base.sf_size);
    }
#endif

    /* add to tail of in-progress list */
    opal_list_append(&endpoint->endpoint_frag_send_queue,
            &frag->sf_base.uf_base.super.super);

    /* possibly make this endpoint ready to send again */
    ompi_btl_usnic_check_rts(endpoint);

    return OMPI_SUCCESS;
}

static inline void
ompi_btl_usnic_release_send_segment(
    ompi_btl_usnic_module_t *module,
    ompi_btl_usnic_send_frag_t *frag,
    ompi_btl_usnic_send_segment_t *sseg)
{
    /* We only return CHUNK segments because they are the only send-style 
     * segments that are dynamically allocated.
     */
    if (sseg->ss_base.us_type == OMPI_BTL_USNIC_SEG_CHUNK) {
        ompi_btl_usnic_chunk_segment_return(module, sseg);
    }
}

void ompi_btl_usnic_frag_complete(ompi_btl_usnic_send_frag_t *frag);

void ompi_btl_usnic_frag_send_complete(ompi_btl_usnic_module_t *module,
                                    ompi_btl_usnic_send_segment_t *sseg);

void ompi_btl_usnic_chunk_send_complete(ompi_btl_usnic_module_t *module,
                                    ompi_btl_usnic_send_segment_t *sseg);

int
ompi_btl_usnic_finish_put_or_send(
    ompi_btl_usnic_module_t *module,
    ompi_btl_usnic_endpoint_t *endpoint,
    ompi_btl_usnic_send_frag_t *frag,
    mca_btl_base_tag_t tag)
__opal_attribute_noinline__;

#endif /* BTL_USNIC_SEND_H */
