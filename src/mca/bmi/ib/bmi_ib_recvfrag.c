/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
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
#include "mca/pml/base/pml_base_sendreq.h"
#include "bmi_ib.h"
#include "bmi_ib_peer.h"
#include "bmi_ib_recvfrag.h"
#include "bmi_ib_sendfrag.h"
#include "bmi_ib_memory.h"

static void mca_bmi_ib_recv_frag_construct(mca_bmi_ib_recv_frag_t* frag);
static void mca_bmi_ib_recv_frag_destruct(mca_bmi_ib_recv_frag_t* frag);

OBJ_CLASS_INSTANCE(mca_bmi_ib_recv_frag_t, 
        mca_bmi_base_recv_frag_t,
        mca_bmi_ib_recv_frag_construct, 
        mca_bmi_ib_recv_frag_destruct);

/*
 * IB fragment constructor
 */

static void mca_bmi_ib_recv_frag_construct(mca_bmi_ib_recv_frag_t* frag)
{
}


/*
 * IB fragment destructor
 */

static void mca_bmi_ib_recv_frag_destruct(mca_bmi_ib_recv_frag_t* frag)
{
}

void
mca_bmi_ib_recv_frag_done (
    mca_bmi_base_header_t *header,
    mca_bmi_base_recv_frag_t* frag,
    mca_bmi_base_recv_request_t *request)
{
    D_PRINT("");
    frag->frag_base.frag_owner->bmi_recv_progress (
            frag->frag_base.frag_owner,
            request,
            frag->frag_base.frag_size,
            frag->frag_base.frag_size);

    /* Return recv frag to free list */
    OMPI_FREE_LIST_RETURN(&mca_bmi_ib_component.ib_recv_frags,
            (ompi_list_item_t*)frag);
}

static void mca_bmi_ib_data_frag(
    mca_bmi_ib_module_t *ib_bmi,
    mca_bmi_base_header_t *hdr)
{
    bool matched;
    int rc;
    ompi_list_item_t *item;
    mca_bmi_ib_recv_frag_t *recv_frag;
    size_t hdr_length;

    OMPI_FREE_LIST_WAIT (&mca_bmi_ib_component.ib_recv_frags, item, rc);

    recv_frag = (mca_bmi_ib_recv_frag_t *) item;
    recv_frag->super.frag_base.frag_owner = &ib_bmi->super; 
    recv_frag->super.frag_base.frag_peer = NULL; 
    recv_frag->super.frag_request = NULL; 
    recv_frag->super.frag_is_buffered = false;

    /* Copy the header, mca_bmi_base_match() */
    recv_frag->super.frag_base.frag_header = *hdr;

    switch(hdr->hdr_common.hdr_type) {
        case MCA_BMI_HDR_TYPE_MATCH:
            hdr_length = sizeof(mca_bmi_base_match_header_t);
            recv_frag->super.frag_base.frag_size = hdr->hdr_match.hdr_msg_length;
            break;
        case MCA_BMI_HDR_TYPE_RNDV:
            hdr_length = sizeof(mca_bmi_base_rendezvous_header_t);
            recv_frag->super.frag_base.frag_size = hdr->hdr_rndv.hdr_frag_length;
            break;
    }

    /* Taking the data starting point be default */
    recv_frag->super.frag_base.frag_addr = (char *) hdr + hdr_length;

    /* match against preposted requests */
    matched = ib_bmi->super.bmi_match(
            recv_frag->super.frag_base.frag_owner,
            &recv_frag->super,
            &recv_frag->super.frag_base.frag_header.hdr_match);

    if (!matched) {
        memcpy (recv_frag->unex_buf, (char *) hdr + hdr_length, recv_frag->super.frag_base.frag_size);
        recv_frag->super.frag_is_buffered = true; 
        recv_frag->super.frag_base.frag_addr = recv_frag->unex_buf;
    } 
}

static void mca_bmi_ib_ctrl_frag(
    mca_bmi_ib_module_t *ib_bmi,
    mca_bmi_base_header_t *header)
{
    mca_bmi_ib_send_frag_t *send_frag;
    mca_bmi_base_send_request_t *req;
    void *data_ptr;

    send_frag = (mca_bmi_ib_send_frag_t *)
        header->hdr_ack.hdr_src_ptr.pval;
    req = (mca_bmi_base_send_request_t *) 
        send_frag->frag_send.frag_request;

    req->req_peer_match = header->hdr_ack.hdr_dst_match;
    req->req_peer_addr = header->hdr_ack.hdr_dst_addr;
    req->req_peer_size = header->hdr_ack.hdr_dst_size;

    /* Locate data in the ACK buffer */
    data_ptr = (void*)
        ((char*) header + sizeof(mca_bmi_base_ack_header_t));

    /* Copy over data to request buffer */
    memcpy(&((mca_bmi_ib_send_request_t *) req)->req_key,
            data_ptr, sizeof(VAPI_rkey_t));

    /* Progress & release fragments */
    mca_bmi_ib_send_frag_send_complete(ib_bmi, send_frag);
}

static void mca_bmi_ib_last_frag(mca_bmi_ib_module_t *ib_bmi,
        mca_bmi_base_header_t *hdr)
{
    mca_bmi_ib_fin_header_t *fin_hdr = (mca_bmi_ib_fin_header_t *)hdr;
    mca_bmi_base_recv_request_t *request;
    request = (mca_bmi_base_recv_request_t*) hdr->hdr_frag.hdr_dst_ptr.pval;

    /* deregister memory if this is the last fragment */
    if ((request->req_bytes_received + hdr->hdr_frag.hdr_frag_length) >= 
        request->req_recv.req_bytes_packed) {
        mca_bmi_ib_deregister_mem_with_registry(ib_bmi,
            fin_hdr->mr_addr.pval, (size_t)fin_hdr->mr_size);
    }

    ib_bmi->super.bmi_recv_progress (
            &ib_bmi->super,
            request,
            hdr->hdr_frag.hdr_frag_length,
            hdr->hdr_frag.hdr_frag_length);

}

/*
 * Process incoming receive fragments
 *
 */

void mca_bmi_ib_process_recv(mca_bmi_ib_module_t *ib_bmi, void* addr)
{
    ib_buffer_t *ib_buf;
    mca_bmi_base_header_t *header;

    ib_buf = (ib_buffer_t *) addr;
    header = (mca_bmi_base_header_t *) &ib_buf->buf[0];

    switch(header->hdr_common.hdr_type) {
        case MCA_BMI_HDR_TYPE_MATCH :
        case MCA_BMI_HDR_TYPE_RNDV :
        case MCA_BMI_HDR_TYPE_FRAG :
            mca_bmi_ib_data_frag(ib_bmi, header);
            break;
        case MCA_BMI_HDR_TYPE_ACK :
            mca_bmi_ib_ctrl_frag(ib_bmi, header);
            break;
        case MCA_BMI_HDR_TYPE_FIN :
            A_PRINT("Fin");
            mca_bmi_ib_last_frag(ib_bmi, header);
            break;
        default :
            ompi_output(0, "Unknown fragment type");
            break;
    }
}
