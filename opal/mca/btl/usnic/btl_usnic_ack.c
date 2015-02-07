/*
 * Copyright (c) 2013-2015 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <errno.h>
#include <string.h>
#include <unistd.h>

#include "opal/util/output.h"
#include "opal/class/opal_hotel.h"

#include "btl_usnic.h"
#include "btl_usnic_frag.h"
#include "btl_usnic_endpoint.h"
#include "btl_usnic_module.h"
#include "btl_usnic_ack.h"
#include "btl_usnic_util.h"
#include "btl_usnic_send.h"
#include "btl_usnic_connectivity.h"

/*
 * Force a retrans of a segment
 */
static void
opal_btl_usnic_force_retrans(
    opal_btl_usnic_endpoint_t *endpoint,
    opal_btl_usnic_seq_t ack_seq)
{
    opal_btl_usnic_send_segment_t *sseg;
    int is;

    is = WINDOW_SIZE_MOD(ack_seq+1);
    sseg = endpoint->endpoint_sent_segs[is];
    if (sseg == NULL || sseg->ss_hotel_room == -1) {
        return;
    }

    /* cancel retrans timer */
    opal_hotel_checkout(&endpoint->endpoint_hotel, sseg->ss_hotel_room);
    sseg->ss_hotel_room = -1;

    /* Queue up this segment to be resent */
    opal_list_append(&(endpoint->endpoint_module->pending_resend_segs),
                     &(sseg->ss_base.us_list.super));

    ++endpoint->endpoint_module->stats.num_fast_retrans;
}


/*
 * We have received an ACK for a given sequence number (either standalone
 * or via piggy-back on a regular send)
 */
void
opal_btl_usnic_handle_ack(
    opal_btl_usnic_endpoint_t *endpoint,
    opal_btl_usnic_seq_t ack_seq)
{
    opal_btl_usnic_seq_t is;
    opal_btl_usnic_send_segment_t *sseg;
    opal_btl_usnic_send_frag_t *frag;
    opal_btl_usnic_module_t *module;
    uint32_t bytes_acked;

    module = endpoint->endpoint_module;

    /* ignore if this is an old ACK */
    if (SEQ_LT(ack_seq, endpoint->endpoint_ack_seq_rcvd)) {
#if MSGDEBUG1
        opal_output(0, "Got OLD DUP ACK seq %"UDSEQ" < %"UDSEQ"\n",
                ack_seq, endpoint->endpoint_ack_seq_rcvd);
#endif
        ++module->stats.num_old_dup_acks;
        return;

    /* A duplicate ACK means next seg was lost */
    } else if (ack_seq == endpoint->endpoint_ack_seq_rcvd) {
        ++module->stats.num_dup_acks;

        opal_btl_usnic_force_retrans(endpoint, ack_seq);
        return;
    }

    /* Does this ACK have a new sequence number that we haven't
       seen before? */
    for (is = endpoint->endpoint_ack_seq_rcvd + 1; SEQ_LE(is, ack_seq); ++is) {
        sseg = endpoint->endpoint_sent_segs[WINDOW_SIZE_MOD(is)];

#if MSGDEBUG1
        opal_output(0, "  Checking ACK/sent_segs window %p, index %lu, seq %lu, occupied=%p, seg_room=%d",
            (void*) endpoint->endpoint_sent_segs,
            WINDOW_SIZE_MOD(is), is, (void*)sseg, (sseg?sseg->ss_hotel_room:-2));
#endif

        assert(sseg != NULL);
        assert(sseg->ss_base.us_btl_header->pkt_seq == is);
#if MSGDEBUG1
        if (sseg->ss_hotel_room == -1) {
            opal_output(0, "=== ACKed frag in sent_frags array is not in hotel/enqueued, module %p, endpoint %p, seg %p, seq %" UDSEQ ", slot %lu",
                        (void*) module, (void*) endpoint,
                        (void*) sseg, is, WINDOW_SIZE_MOD(is));
        }
#endif

        /* Check the sending segment out from the hotel.  NOTE: The
           segment might not actually be in a hotel room if it has
           already been evicted and queued for resend.
           If it's not in the hotel, don't check it out! */
        if (OPAL_LIKELY(sseg->ss_hotel_room != -1)) {

            opal_hotel_checkout(&endpoint->endpoint_hotel, sseg->ss_hotel_room);
            sseg->ss_hotel_room = -1;

        /* hotel_room == -1 means queued for resend, remove it */
        } else {
            opal_list_remove_item((&module->pending_resend_segs),
                    &sseg->ss_base.us_list.super);
        }

        /* update the owning fragment */
        bytes_acked = sseg->ss_base.us_btl_header->payload_len;
        frag = sseg->ss_parent_frag;

#if MSGDEBUG1
        opal_output(0, "   ACKED seg %p frag %p ack_bytes=%"PRIu32" left=%zd dst_seg[0].seg_addr=%p des_flags=0x%x\n",
                (void*)sseg, (void*)frag, bytes_acked,
                frag->sf_ack_bytes_left - bytes_acked,
                frag->sf_base.uf_local_seg[0].seg_addr.pval,
                frag->sf_base.uf_base.des_flags);
#endif

        /* If all ACKs received, and this is a put or a regular send
         * that needs a callback, perform the callback now
         *
         * NOTE on sf_ack_bytes_left - here we check for
         *      sf_ack_bytes_left == bytes_acked
         * as opposed to adjusting sf_ack_bytes_left and checking for 0 because
         * if we don't, the callback function may call usnic_free() and free
         * the fragment out from under us which we do not want.  If the
         * fragment really needs to be freed, we'll take care of it in a few
         * lines below.
         */
        if (frag->sf_ack_bytes_left == bytes_acked) {
#if BTL_VERSION == 30
            if (frag->sf_base.uf_remote_seg[0].seg_addr.pval != NULL) {
                OPAL_BTL_USNIC_DO_PUT_FRAG_CB(module, frag, "put completion");
            } else if (frag->sf_base.uf_base.des_flags &
                       MCA_BTL_DES_SEND_ALWAYS_CALLBACK) {
                OPAL_BTL_USNIC_DO_SEND_FRAG_CB(module, frag, "send completion");
            }
#else
            if ((frag->sf_base.uf_remote_seg[0].seg_addr.pval != NULL) ||
                (frag->sf_base.uf_base.des_flags &
                 MCA_BTL_DES_SEND_ALWAYS_CALLBACK)) {
                OPAL_BTL_USNIC_DO_SEND_FRAG_CB(module, frag, "send completion");
            }
#endif
        }

        /* free this segment */
        sseg->ss_ack_pending = false;
        if (sseg->ss_send_posted == 0) {
            opal_btl_usnic_release_send_segment(module, frag, sseg);
        }

        /* when no bytes left to ACK, fragment send is truly done */
        /* see note above on why this is done here as opposed to earlier */
        frag->sf_ack_bytes_left -= bytes_acked;

        /* OK to return this fragment? */
        opal_btl_usnic_send_frag_return_cond(module, frag);

        /* indicate this segment has been ACKed */
        endpoint->endpoint_sent_segs[WINDOW_SIZE_MOD(is)] = NULL;
    }

    /* update ACK received */
    endpoint->endpoint_ack_seq_rcvd = ack_seq;

    /* send window may have opened, possibly make endpoint ready-to-send */
    opal_btl_usnic_check_rts(endpoint);
}

/*
 * Send an ACK
 */
void
opal_btl_usnic_ack_send(
    opal_btl_usnic_module_t *module,
    opal_btl_usnic_endpoint_t *endpoint)
{
    opal_btl_usnic_ack_segment_t *ack;

    /* Get an ACK frag.  If we don't get one, just discard this ACK. */
    ack = opal_btl_usnic_ack_segment_alloc(module);
    if (OPAL_UNLIKELY(NULL == ack)) {
        opal_output(0, "====================== No frag for sending the ACK -- skipped");
        abort();
    }

    /* send the seq of the lowest item in the window that
       we've received */
    ack->ss_base.us_btl_header->ack_seq =
        endpoint->endpoint_next_contig_seq_to_recv - 1;

    ack->ss_len = sizeof(opal_btl_usnic_btl_header_t);

#if MSGDEBUG1
    {
        char remote_ip[IPV4STRADDRLEN];
        struct opal_btl_usnic_modex_t *modex =
            &endpoint->endpoint_remote_modex;
        opal_btl_usnic_snprintf_ipv4_addr(remote_ip, sizeof(remote_ip),
                                          modex->ipv4_addr,
                                          modex->netmask);


        opal_output(0, "--> Sending ACK, length %d, seq %" UDSEQ " to %s, port %u",
                    ack->ss_len,
                    ack->ss_base.us_btl_header->ack_seq,
                    remote_ip,
                    modex->ports[ack->ss_channel]);
    }
#endif

    /* Do we need to check the connectivity?  If enabled, we'll check
       the connectivity at either first send to peer X or first ACK to
       peer X. */
    opal_btl_usnic_check_connectivity(module, endpoint);

    /* send the ACK */
    opal_btl_usnic_post_ack(module, endpoint, ack);

    /* Stats */
    ++module->stats.num_ack_sends;

    return;
}

/*
 * Sending an ACK has completed, return the segment to the free list
 */
void
opal_btl_usnic_ack_complete(opal_btl_usnic_module_t *module,
                                   opal_btl_usnic_ack_segment_t *ack)
{
    opal_btl_usnic_ack_segment_return(module, ack);
}

/*****************************************************************************/

/*
 * Callback for when a send times out without receiving a
 * corresponding ACK.
 */
void
opal_btl_usnic_ack_timeout(
    opal_hotel_t *hotel,
    int room_num,
    void *occupant)
{
    opal_btl_usnic_send_segment_t *seg;
    opal_btl_usnic_endpoint_t *endpoint;
    opal_btl_usnic_module_t *module;

    seg = (opal_btl_usnic_send_segment_t*) occupant;
    endpoint = seg->ss_parent_frag->sf_endpoint;
    module = endpoint->endpoint_module;

#if MSGDEBUG1
    {
        opal_output(0, "Send timeout!  seg %p, room %d, seq %" UDSEQ "\n",
                    (void*)seg, seg->ss_hotel_room,
                    seg->ss_base.us_btl_header->pkt_seq);
    }
#endif

    /* timeout checks us out, note this */
    seg->ss_hotel_room = -1;

    /* Queue up this frag to be resent */
    opal_list_append(&(module->pending_resend_segs),
                     &(seg->ss_base.us_list.super));

    /* Stats */
    ++module->stats.num_timeout_retrans;
}

