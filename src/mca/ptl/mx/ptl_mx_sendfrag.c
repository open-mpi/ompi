/*
 * $HEADER$
 */
#include "ptl_mx.h"
#include "ptl_mx_peer.h"
#include "ptl_mx_sendfrag.h"


static void mca_ptl_mx_send_frag_construct(mca_ptl_mx_send_frag_t* frag);
static void mca_ptl_mx_send_frag_destruct(mca_ptl_mx_send_frag_t* frag);


OBJ_CLASS_INSTANCE(
    mca_ptl_mx_send_frag_t,
    mca_ptl_base_send_frag_t,
    mca_ptl_mx_send_frag_construct,
    mca_ptl_mx_send_frag_destruct);

                                                                                                           
/*
 * Placeholders for send fragment constructor/destructors.
 */

static void mca_ptl_mx_send_frag_construct(mca_ptl_mx_send_frag_t* frag)
{
}


static void mca_ptl_mx_send_frag_destruct(mca_ptl_mx_send_frag_t* frag)
{
}


static void *mca_ptl_mx_alloc(size_t *size)
{
    return malloc(*size);
}
                                                                                                          
/*
 *  Initialize the fragment based on the current offset into the users
 *  data buffer, and the indicated size.
 */

int mca_ptl_mx_send_frag_init(
    mca_ptl_mx_send_frag_t* sendfrag,
    mca_ptl_mx_peer_t* ptl_peer,
    mca_pml_base_send_request_t* sendreq,
    size_t offset,
    size_t* size,
    int flags)
{
    /* message header */
    size_t size_in = *size;
    size_t size_out;
    unsigned int iov_count, max_data;

    mca_ptl_base_header_t* hdr = &sendfrag->frag_send.frag_base.frag_header;
    sendfrag->frag_segments[0].segment_ptr = hdr;
    sendfrag->frag_segments[0].segment_length = sizeof(mca_ptl_base_header_t);

    if(offset == 0) {
        hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_MATCH;
        hdr->hdr_common.hdr_flags = flags;
        hdr->hdr_common.hdr_size = sizeof(mca_ptl_base_match_header_t);
        hdr->hdr_frag.hdr_frag_offset = offset;
        hdr->hdr_frag.hdr_frag_seq = 0;
        hdr->hdr_frag.hdr_src_ptr.lval = 0; /* for VALGRIND/PURIFY - REPLACE WITH MACRO */
        hdr->hdr_frag.hdr_src_ptr.pval = sendfrag;
        hdr->hdr_frag.hdr_dst_ptr.lval = 0;
        hdr->hdr_match.hdr_contextid = sendreq->req_base.req_comm->c_contextid;
        hdr->hdr_match.hdr_src = sendreq->req_base.req_comm->c_my_rank;
        hdr->hdr_match.hdr_dst = sendreq->req_base.req_peer;
        hdr->hdr_match.hdr_tag = sendreq->req_base.req_tag;
        hdr->hdr_match.hdr_msg_length = sendreq->req_bytes_packed;
        hdr->hdr_match.hdr_msg_seq = sendreq->req_base.req_sequence;
    } else {
        hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_FRAG;
        hdr->hdr_common.hdr_flags = flags;
        hdr->hdr_common.hdr_size = sizeof(mca_ptl_base_frag_header_t);
        hdr->hdr_frag.hdr_frag_offset = offset;
        hdr->hdr_frag.hdr_frag_seq = 0;
        hdr->hdr_frag.hdr_src_ptr.lval = 0; /* for VALGRIND/PURIFY - REPLACE WITH MACRO */
        hdr->hdr_frag.hdr_src_ptr.pval = sendfrag;
        hdr->hdr_frag.hdr_dst_ptr = sendreq->req_peer_match;
    }
    sendfrag->frag_free = 0;

    /* initialize convertor */
    if(size_in > 0) {
       ompi_convertor_t *convertor;
       struct iovec iov;
       int rc;

       convertor = &sendfrag->frag_send.frag_base.frag_convertor;
       ompi_convertor_copy(&sendreq->req_convertor, convertor);
       ompi_convertor_init_for_send(
                    convertor,
                    0,
                    sendreq->req_datatype,
                    sendreq->req_count,
                    sendreq->req_addr,
                    offset,
                    mca_ptl_mx_alloc );

        /* if data is contigous convertor will return an offset
         * into users buffer - otherwise will return an allocated buffer
         * that holds the packed data
         */
        iov.iov_base = NULL;
        iov.iov_len = size_in;
        iov_count = 1;
        max_data = size_in;
        if((rc = ompi_convertor_pack( 
            convertor, 
            &iov,
            &iov_count, 
            &max_data, 
            &(sendfrag->frag_free))) < 0) {
            return OMPI_ERROR;
        }
        /* adjust the freeAfter as the position zero is reserved for the header */
        sendfrag->frag_free <<= 1;
        sendfrag->frag_segments[1].segment_ptr = iov.iov_base;
        sendfrag->frag_segments[1].segment_length = size_out;
        sendfrag->frag_segment_count = 2;
        sendfrag->frag_send.frag_base.frag_addr = iov.iov_base;

        /* adjust size and request offset to reflect actual number of bytes packed by convertor */
        size_out = iov.iov_len;
    } else {
        size_out = size_in;
        sendfrag->frag_send.frag_base.frag_addr = NULL;
        sendfrag->frag_send.frag_base.frag_size = 0;
        sendfrag->frag_segment_count = 1;
    }
    hdr->hdr_frag.hdr_frag_length = size_out;

    /* convert to network byte order if required */
    if(ptl_peer->peer_byte_swap) {
        hdr->hdr_common.hdr_flags |= MCA_PTL_FLAGS_NBO;
        if(offset == 0) {
            MCA_PTL_BASE_MATCH_HDR_HTON(hdr->hdr_match);
        } else {
            MCA_PTL_BASE_FRAG_HDR_HTON(hdr->hdr_frag);
        }
    }

    /* fragment state */
    sendfrag->frag_send.frag_base.frag_owner = &ptl_peer->peer_ptl->super;
    sendfrag->frag_send.frag_request = sendreq;
    sendfrag->frag_send.frag_base.frag_size = size_out;
    sendfrag->frag_send.frag_base.frag_peer = ptl_peer;

    *size = size_out;
    return OMPI_SUCCESS;
}


void mca_ptl_mx_send_frag_init_ack(
    mca_ptl_mx_send_frag_t* ack,
    mca_ptl_mx_module_t* ptl,
    struct mca_ptl_mx_recv_frag_t* recv_frag)
{

}
                                                                                                       

