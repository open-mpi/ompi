#include "include/types.h"
#include "datatype/datatype.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "ptl_ib.h"
#include "ptl_ib_peer.h"
#include "ptl_ib_proc.h"
#include "ptl_ib_sendfrag.h"

static void mca_ptl_ib_send_frag_construct(mca_ptl_ib_send_frag_t* frag);
static void mca_ptl_ib_send_frag_destruct(mca_ptl_ib_send_frag_t* frag);

OBJ_CLASS_INSTANCE(mca_ptl_ib_send_frag_t, 
        mca_ptl_base_send_frag_t,
        mca_ptl_ib_send_frag_construct, 
        mca_ptl_ib_send_frag_destruct);

/*
 * Placeholders for send fragment constructor/destructors.
 */

static void mca_ptl_ib_send_frag_construct(mca_ptl_ib_send_frag_t* frag)
{
    D_PRINT("\n");
}

static void mca_ptl_ib_send_frag_destruct(mca_ptl_ib_send_frag_t* frag)
{
    D_PRINT("\n");
}

int mca_ptl_ib_send_frag_init(mca_ptl_ib_send_frag_t* sendfrag,
        struct mca_ptl_base_peer_t* ptl_peer,
        struct mca_pml_base_send_request_t* sendreq,
        size_t offset,
        size_t* size,
        int flags)
{
    /* message header */
    size_t size_in = *size;
    size_t size_out;

    D_PRINT("");
#if 0

    mca_ptl_base_header_t* hdr = &sendfrag->frag_header;

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

    /* initialize convertor */
    if(size_in > 0) {
        ompi_convertor_t *convertor;
        int rc;

        /* first fragment (eager send) and first fragment of long
         * protocol can use the convertor initialized on the request,
         * remaining fragments must copy/reinit the convertor as the
         * transfer could be in parallel.
         */
        if( offset <= mca_ptl_ib_module.super.ptl_first_frag_size ) {
            convertor = &sendreq->req_convertor;
        } else {

            convertor = &sendfrag->frag_convertor;
            ompi_convertor_copy(&sendreq->req_convertor, convertor);
            ompi_convertor_init_for_send(
                    convertor,
                    0,
                    sendreq->req_base.req_datatype,
                    sendreq->req_base.req_count,
                    sendreq->req_base.req_addr,
                    offset);
        }


        /* if data is contigous convertor will return an offset
         * into users buffer - otherwise will return an allocated buffer 
         * that holds the packed data
         */
        sendfrag->frag_vec[1].iov_base = NULL;
        sendfrag->frag_vec[1].iov_len = size_in;
        if((rc = ompi_convertor_pack(convertor, &sendfrag->frag_vec[1], 1)) < 0)
            return OMPI_ERROR;

        /* adjust size and request offset to reflect actual 
         * number of bytes packed by convertor */
        size_out = sendfrag->frag_vec[1].iov_len;
    } else {
        size_out = size_in;
    }
    hdr->hdr_frag.hdr_frag_length = size_out;

    /* fragment state */
    sendfrag->frag_owner = &ptl_peer->peer_ptl->super;
    sendfrag->frag_send.frag_request = sendreq;
    sendfrag->frag_send.frag_base.frag_addr = sendfrag->frag_vec[1].iov_base;
    sendfrag->frag_send.frag_base.frag_size = size_out;

    sendfrag->frag_peer = ptl_peer;
    sendfrag->frag_vec_ptr = sendfrag->frag_vec;
    sendfrag->frag_vec_cnt = (size_out == 0) ? 1 : 2;
    sendfrag->frag_vec[0].iov_base = (ompi_iov_base_ptr_t)hdr;
    sendfrag->frag_vec[0].iov_len = sizeof(mca_ptl_base_header_t);
    sendfrag->frag_progressed = 0;
    *size = size_out;
#endif

    return OMPI_SUCCESS;
}
