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
 * Copyright (c) 2008-2014 Cisco Systems, Inc.  All rights reserved.
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
 * We have received a segment, take action based on the 
 * packet type in the BTL header
 */
void ompi_btl_usnic_recv_call(ompi_btl_usnic_module_t *module,
                              ompi_btl_usnic_recv_segment_t *seg,
                              ompi_btl_usnic_channel_t *channel,
                              uint32_t l2_bytes_rcvd)
{
    ompi_btl_usnic_segment_t *bseg;
    mca_btl_active_message_callback_t* reg;
    ompi_btl_usnic_endpoint_t *endpoint;
    ompi_btl_usnic_btl_chunk_header_t *chunk_hdr;
    ompi_btl_usnic_btl_header_t *hdr;
    uint32_t window_index;
    int rc;
#if MSGDEBUG1
    char src_mac[32];
    char dest_mac[32];
#endif

    bseg = &seg->rs_base;

    ++module->stats.num_total_recvs;

    /* Valgrind help */
    opal_memchecker_base_mem_defined((void*)(seg->rs_recv_desc.sg_list[0].addr),
                                     seg->rs_recv_desc.sg_list[0].length);

    /* Find out who sent this segment */
    endpoint = seg->rs_endpoint;
    if (FAKE_RECV_FRAG_DROP || OPAL_UNLIKELY(NULL == endpoint)) {
        /* No idea who this was from, so drop it */
#if MSGDEBUG1
        opal_output(0, "=== Unknown sender; dropped: from MAC %s to MAC %s, seq %" UDSEQ, 
                    src_mac, 
                    dest_mac, 
                    bseg->us_btl_header->pkt_seq);
#endif
        ++module->stats.num_unk_recvs;
        goto repost_no_endpoint;
    }

    /***********************************************************************/
    /* Segment is an incoming frag */
    if (OMPI_BTL_USNIC_PAYLOAD_TYPE_FRAG == bseg->us_btl_header->payload_type) {

        /* do the receive bookkeeping */
        rc = ompi_btl_usnic_recv_frag_bookkeeping(module, seg, channel);
        if (rc != 0) {
            return;
        }

        hdr = seg->rs_base.us_btl_header;

#if MSGDEBUG1
        opal_output(0, "<-- Received FRAG ep %p, seq %" UDSEQ ", len=%d\n",
                    (void*) endpoint, hdr->pkt_seq, hdr->payload_len);
#if 0

        opal_output(0, "<-- Received FRAG ep %p, seq %" UDSEQ " from %s to %s: GOOD! (rel seq %d, lowest seq %" UDSEQ ", highest seq: %" UDSEQ ", rwstart %d) seg %p, module %p\n",
                    (void*) endpoint,
                    seg->rs_base.us_btl_header->pkt_seq,
                    src_mac, dest_mac,
                    window_index,
                    endpoint->endpoint_next_contig_seq_to_recv,
                    endpoint->endpoint_highest_seq_rcvd,
                    endpoint->endpoint_rfstart,
                    (void*) seg, (void*) module);
        if (hdr->put_addr != NULL) {
            opal_output(0, "  put_addr = %p\n",
                    seg->rs_base.us_btl_header->put_addr);
        }
#endif
#endif

        if (OPAL_UNLIKELY(ompi_btl_usnic_frag_seg_proto_size(seg) !=
                          l2_bytes_rcvd)) {
            BTL_ERROR(("L2 packet size and segment payload len do not agree!"
                       " l2_bytes_rcvd=%" PRIu32 " expected=%" PRIu32,
                       l2_bytes_rcvd, ompi_btl_usnic_frag_seg_proto_size(seg)));
            abort();
        }

        /* If this it not a PUT, Pass this segment up to the PML.
         * Be sure to get the payload length from the BTL header because
         * the L2 layer may artificially inflate (or otherwise change)
         * the frame length to meet minimum sizes, add protocol information,
         * etc.
         */
        if (hdr->put_addr == NULL) {
            reg = mca_btl_base_active_message_trigger + hdr->tag;
            seg->rs_segment.seg_len = hdr->payload_len;
#if MSGDEBUG2
                opal_output(0, "small recv complete, pass up %u bytes, tag=%d\n",
                        (unsigned)bseg->us_btl_header->payload_len,
                        (int)bseg->us_btl_header->tag);
#endif
            reg->cbfunc(&module->super, hdr->tag, &seg->rs_desc, reg->cbdata);

        /*
         * If this is a PUT, need to copy it to user buffer
         */
        } else {
#if MSGDEBUG1
            opal_output(0, "Copy %d PUT bytes to %p\n", 
                seg->rs_base.us_btl_header->payload_len,
                (void*)seg->rs_base.us_btl_header->put_addr);
#endif
            memcpy(seg->rs_base.us_btl_header->put_addr,
                    seg->rs_base.us_payload.raw,
                    seg->rs_base.us_btl_header->payload_len);
        }

        /* do not jump to repost, already done by bookkeeping */
        return;
    }

    /***********************************************************************/
    /* Segment is an incoming chunk */
    if (OMPI_BTL_USNIC_PAYLOAD_TYPE_CHUNK == bseg->us_btl_header->payload_type) {
        int frag_index;
        ompi_btl_usnic_rx_frag_info_t *fip;

        if (OPAL_UNLIKELY(ompi_btl_usnic_chunk_seg_proto_size(seg) !=
                          l2_bytes_rcvd)) {
            BTL_ERROR(("L2 packet size and segment payload len do not agree!"
                       " l2_bytes_rcvd=%" PRIu32 " expected=%" PRIu32,
                       l2_bytes_rcvd, ompi_btl_usnic_chunk_seg_proto_size(seg)));
            abort();
        }

        /* Is incoming sequence # ok? */
        if (OPAL_UNLIKELY(ompi_btl_usnic_check_rx_seq(endpoint, seg,
                        &window_index) != 0)) {
            goto repost;
        }

#if MSGDEBUG1
        opal_output(0, "<-- Received CHUNK fid %d ep %p, seq %" UDSEQ " from %s to %s: GOOD! (rel seq %d, lowest seq %" UDSEQ ", highest seq: %" UDSEQ ", rwstart %d) seg %p, module %p\n",
                    seg->rs_base.us_btl_chunk_header->ch_frag_id,
                    (void*) endpoint,
                    seg->rs_base.us_btl_chunk_header->ch_hdr.pkt_seq,
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
                pool = usnic_fls(chunk_hdr->ch_frag_size-1);
                if (pool >= module->first_pool &&
                        pool <= module->last_pool) {
                    ompi_free_list_item_t* item;
                    ompi_btl_usnic_rx_buf_t *rx_buf;
                    OMPI_FREE_LIST_GET_MT(&module->module_recv_buffers[pool],
                                          item);
                    rx_buf = (ompi_btl_usnic_rx_buf_t *)item;
                    if (OPAL_LIKELY(NULL != rx_buf)) {
                        fip->rfi_fl_elt = item;
                        fip->rfi_data = rx_buf->buf;
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
#if MSGDEBUG1
                opal_output(0, "Start large recv to %p, size=%"PRIu32"\n",
                    (void *)fip->rfi_data, chunk_hdr->ch_frag_size);
#endif
            } else {
#if MSGDEBUG1
                opal_output(0, "Start PUT to %p\n",
                        (void *)chunk_hdr->ch_hdr.put_addr);
#endif
                fip->rfi_data = chunk_hdr->ch_hdr.put_addr;
            }
            fip->rfi_bytes_left = chunk_hdr->ch_frag_size;
            fip->rfi_frag_id = chunk_hdr->ch_frag_id;

        /* frag_id is not 0 - it must match, drop if not */
        } else if (fip->rfi_frag_id != chunk_hdr->ch_frag_id) {
            ++module->stats.num_badfrag_recvs;
            goto repost;
        }
#if MSGDEBUG1
        opal_output(0, "put_addr=%p, copy_addr=%p, off=%d\n",
                chunk_hdr->ch_hdr.put_addr,
                fip->rfi_data+chunk_hdr->ch_frag_offset,
                chunk_hdr->ch_frag_offset);
#endif

        /* Stats */
        ++module->stats.num_chunk_recvs;

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
            mca_btl_base_descriptor_t desc;
            mca_btl_base_segment_t segment;

            segment.seg_addr.pval = fip->rfi_data;
            segment.seg_len = fip->rfi_frag_size;
            desc.des_dst = &segment;
            desc.des_dst_cnt = 1;

            /* only up to PML if this was not a put */
            if (chunk_hdr->ch_hdr.put_addr == NULL) {

                /* Pass this segment up to the PML */
#if MSGDEBUG2
                opal_output(0, "large recv complete, pass up %p, %u bytes, tag=%d\n",
                        desc.des_dst->seg_addr.pval,
                        (unsigned)desc.des_dst->seg_len,
                        (int)chunk_hdr->ch_hdr.tag);
#endif
                reg = mca_btl_base_active_message_trigger +
                    chunk_hdr->ch_hdr.tag;

                /* mca_pml_ob1_recv_frag_callback_frag() */
                reg->cbfunc(&module->super, chunk_hdr->ch_hdr.tag,
                        &desc, reg->cbdata);

                /* free temp buffer for non-put */
                if (0 == fip->rfi_data_pool) {
                    free(fip->rfi_data);
                } else {
                    OMPI_FREE_LIST_RETURN_MT(
                            &module->module_recv_buffers[fip->rfi_data_pool],
                            fip->rfi_fl_elt);
                }

#if MSGDEBUG1
            } else {
                opal_output(0, "PUT recv complete, no callback\n");
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
        ++module->stats.num_ack_recvs;

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
        ++module->stats.num_unk_recvs;
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
    ++module->stats.num_recv_reposts;

    /* Add recv to linked list for reposting */
    seg->rs_recv_desc.next = channel->repost_recv_head;
    channel->repost_recv_head = &seg->rs_recv_desc;
}
