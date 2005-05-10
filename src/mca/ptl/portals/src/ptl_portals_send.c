/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "portals_config.h"

#include "ptl_portals.h"
#include "ptl_portals_compat.h"
#include "ptl_portals_send.h"
#include "mca/pml/pml.h"
#include "mca/ptl/base/ptl_base_sendreq.h"

static void mca_ptl_portals_send_frag_construct(mca_ptl_portals_send_frag_t* frag);
static void mca_ptl_portals_send_frag_destruct(mca_ptl_portals_send_frag_t* frag);

OBJ_CLASS_INSTANCE(mca_ptl_portals_send_frag_t,
                   mca_ptl_base_send_frag_t,
                   mca_ptl_portals_send_frag_construct,
                   mca_ptl_portals_send_frag_destruct);

static void
mca_ptl_portals_send_frag_construct(mca_ptl_portals_send_frag_t* frag)
{
    frag->frag_vector[0].iov_base = &(frag->frag_send.frag_base.frag_header);
    frag->frag_vector[0].iov_len = sizeof(mca_ptl_base_header_t);
}


static void
mca_ptl_portals_send_frag_destruct(mca_ptl_portals_send_frag_t* frag)
{
}


static void*
mca_ptl_portals_alloc(size_t *size)
{
    return malloc(*size);
}


int
mca_ptl_portals_send(struct mca_ptl_base_module_t *ptl_base,
		     struct mca_ptl_base_peer_t *ptl_peer,
		     struct mca_ptl_base_send_request_t *sendreq,
		     size_t offset, size_t size, int flags)
{
    mca_ptl_portals_module_t* ptl = (mca_ptl_portals_module_t*) ptl_base;
    ptl_process_id_t *peer_id = (ptl_process_id_t*) ptl_peer;
    mca_ptl_portals_send_frag_t* sendfrag;
    mca_ptl_base_header_t* hdr;
    int ret;

    ompi_output_verbose(100, mca_ptl_portals_component.portals_output,
                        "mca_ptl_portals_send to %lu, %lu",
                        peer_id->nid, peer_id->pid);

    if (sendreq->req_cached) {
        sendfrag = (mca_ptl_portals_send_frag_t*)(sendreq+1);
    } else {
        ompi_list_item_t *item;
        OMPI_FREE_LIST_GET(&mca_ptl_portals_component.portals_send_frags,
                           item, ret);
        if (NULL == item) return ret;
        sendfrag = (mca_ptl_portals_send_frag_t *) item;
    }

    /* initialize convertor */
    if (size > 0) {
       ompi_convertor_t *convertor;
       struct iovec iov;
       unsigned int iov_count;
       unsigned int max_data;
       int32_t freeAfter;
       int rc;

       convertor = &sendfrag->frag_send.frag_base.frag_convertor;
       ompi_convertor_copy(&sendreq->req_send.req_convertor, convertor);
       ompi_convertor_init_for_send(
                    convertor,
                    0,
                    sendreq->req_send.req_datatype,
                    sendreq->req_send.req_count,
                    sendreq->req_send.req_addr,
                    offset,
                    mca_ptl_portals_alloc );
                                                                                                                      
        /* if data is contigous convertor will return an offset
         * into users buffer - otherwise will return an allocated buffer
         * that holds the packed data
         */
        iov.iov_base = NULL;
        iov.iov_len = size;
        iov_count = 1;
        max_data = size;
        if((rc = ompi_convertor_pack(
            convertor,
            &iov,
            &iov_count,
            &max_data,
            &freeAfter)) < 0) {
            return OMPI_ERROR;
        }
        sendfrag->frag_vector[1].iov_base = iov.iov_base;
        sendfrag->frag_vector[1].iov_len = iov.iov_len;
        sendfrag->frag_send.frag_base.frag_addr = iov.iov_base;
        sendfrag->frag_send.frag_base.frag_size = iov.iov_len;
    } else {
        sendfrag->frag_send.frag_base.frag_addr = NULL;
        sendfrag->frag_send.frag_base.frag_size = 0;
    }

    /* setup message header */
    hdr = &sendfrag->frag_send.frag_base.frag_header;

    /* first frag - needs all matching */
    if (offset == 0) {
        hdr->hdr_common.hdr_flags = flags;
        hdr->hdr_match.hdr_contextid = sendreq->req_send.req_base.req_comm->c_contextid;
        hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
        hdr->hdr_match.hdr_dst = sendreq->req_send.req_base.req_peer;
        hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
        hdr->hdr_match.hdr_msg_length = sendreq->req_send.req_bytes_packed;
        hdr->hdr_match.hdr_msg_seq = sendreq->req_send.req_base.req_sequence;
        
        /* if an acknoweldgment is not required - can get by with a
           shorter header */
        if ((flags & MCA_PTL_FLAGS_ACK) == 0) {
            hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_MATCH;
            sendfrag->frag_vector[0].iov_len = sizeof(mca_ptl_base_match_header_t);
        } else {
            hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_RNDV;
            hdr->hdr_rndv.hdr_frag_length = sendfrag->frag_send.frag_base.frag_size;
            hdr->hdr_rndv.hdr_src_ptr.lval = 0; /* for VALGRIND/PURIFY - REPLACE WITH MACRO */
            hdr->hdr_rndv.hdr_src_ptr.pval = sendfrag;
            sendfrag->frag_vector[0].iov_len = sizeof(mca_ptl_base_rendezvous_header_t);
        }

    } else {
        hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_FRAG;
        hdr->hdr_common.hdr_flags = flags;
        hdr->hdr_frag.hdr_frag_offset = offset;
        hdr->hdr_frag.hdr_frag_length = sendfrag->frag_send.frag_base.frag_size;
        hdr->hdr_frag.hdr_src_ptr.lval = 0; /* for VALGRIND/PURIFY - REPLACE WITH MACRO */
        hdr->hdr_frag.hdr_src_ptr.pval = sendfrag;
        hdr->hdr_frag.hdr_dst_ptr = sendreq->req_peer_match;
    }

    /* fragment state */
    sendfrag->frag_send.frag_base.frag_owner = ptl_base;
    sendfrag->frag_send.frag_request = sendreq;
    sendfrag->frag_send.frag_base.frag_peer = ptl_peer;


    /* must update the offset after actual fragment size is determined 
     * before attempting to send the fragment
     */
    mca_ptl_base_send_request_offset(sendreq,
        sendfrag->frag_send.frag_base.frag_size);

    return mca_ptl_portals_send_frag(ptl, sendfrag);
}


int
mca_ptl_portals_process_send_event(ptl_event_t *ev)
{
    mca_ptl_portals_send_frag_t* frag = 
        (mca_ptl_portals_send_frag_t*) ev->md.user_ptr;
    mca_ptl_base_header_t* hdr = 
        &(frag->frag_send.frag_base.frag_header);

    if (ev->type == PTL_EVENT_SEND_START) {
        ompi_output_verbose(100, mca_ptl_portals_component.portals_output,
                            "SEND_START event for msg %d, length: %d",
                            (int) hdr->hdr_match.hdr_msg_seq,
                            (int) ev->mlength);
    } else if (ev->type == PTL_EVENT_SEND_END) {
        ompi_output_verbose(100, mca_ptl_portals_component.portals_output,
                            "SEND_END event for msg %d",
                            (int) hdr->hdr_match.hdr_msg_seq);
    } else if (ev->type == PTL_EVENT_ACK) {
        bool frag_ack;
        ompi_output_verbose(100, mca_ptl_portals_component.portals_output,
                            "ACK event for msg %d",
                            (int) hdr->hdr_match.hdr_msg_seq);

        /* discard ACKs for acks */
        if (frag->frag_send.frag_request == NULL) {
            if (frag->frag_send.frag_base.frag_addr != NULL) {
                free(frag->frag_send.frag_base.frag_addr);
            }
            OMPI_FREE_LIST_RETURN(&mca_ptl_portals_component.portals_send_frags,
                                  (ompi_list_item_t*) frag);
        } else {

            frag_ack = (hdr->hdr_common.hdr_flags & MCA_PTL_FLAGS_ACK) ? true : false;
            if (frag_ack == false) {
                /* this frag is done! */

                /* let the PML know */
                frag->frag_send.frag_base.frag_owner->
                    ptl_send_progress(frag->frag_send.frag_base.frag_owner,
                                      frag->frag_send.frag_request,
                                      frag->frag_send.frag_base.frag_size);

                /* return frag to freelist if not part of request */
                if (frag->frag_send.frag_request->req_cached == false) {
                    if (frag->frag_send.frag_base.frag_addr != NULL) {
                        free(frag->frag_send.frag_base.frag_addr);
                    }
                    OMPI_FREE_LIST_RETURN(&mca_ptl_portals_component.portals_send_frags,
                                          (ompi_list_item_t*) frag);
                }
            } else {
                /* need to wait for the ack... */
                ;
            }
        }

        /* unlink memory descriptor */
        PtlMDUnlink(ev->md_handle);

    } else {
        ompi_output_verbose(10, mca_ptl_portals_component.portals_output,
                            "unknown event for msg %d: %d",
                            (int) hdr->hdr_match.hdr_msg_seq, ev->type);
    }

    return OMPI_SUCCESS;
}
