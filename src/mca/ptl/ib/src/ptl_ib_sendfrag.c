#include "include/types.h"
#include "datatype/datatype.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "ptl_ib.h"
#include "ptl_ib_peer.h"
#include "ptl_ib_proc.h"
#include "ptl_ib_sendfrag.h"
#include "ptl_ib_priv.h"

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
    frag->frag_progressed = 0;
    frag->frag_ack_pending = 0;
}

static void mca_ptl_ib_send_frag_destruct(mca_ptl_ib_send_frag_t* frag)
{
}

int mca_ptl_ib_send_frag_init(mca_ptl_ib_send_frag_t* sendfrag,
        struct mca_ptl_base_peer_t* ptl_peer,
        struct mca_pml_base_send_request_t* sendreq,
        size_t offset,
        size_t* size,
        int flags)
{
    size_t size_in = *size;
    size_t size_out;
    mca_ptl_base_header_t *hdr;
    struct iovec iov;
    int header_length;

    D_PRINT("");

    /* Start of the IB buffer */
    hdr = (mca_ptl_base_header_t *) &sendfrag->ib_buf.buf[0];

    /* Fill up the header for PML to make a match */
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

        header_length = sizeof(mca_ptl_base_match_header_t);

    } else {
        hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_FRAG;
        hdr->hdr_common.hdr_flags = flags;
        hdr->hdr_common.hdr_size = sizeof(mca_ptl_base_frag_header_t);
        hdr->hdr_frag.hdr_frag_offset = offset; 
        hdr->hdr_frag.hdr_frag_seq = 0;
        hdr->hdr_frag.hdr_src_ptr.lval = 0; /* for VALGRIND/PURIFY - REPLACE WITH MACRO */
        hdr->hdr_frag.hdr_src_ptr.pval = sendfrag;
        hdr->hdr_frag.hdr_dst_ptr = sendreq->req_peer_match;

        header_length = sizeof(mca_ptl_base_frag_header_t);
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

            convertor = &sendfrag->frag_send.frag_base.frag_convertor;
            ompi_convertor_copy(&sendreq->req_convertor, convertor);
            ompi_convertor_init_for_send(
                    convertor,
                    0,
                    sendreq->req_base.req_datatype,
                    sendreq->req_base.req_count,
                    sendreq->req_base.req_addr,
                    offset);
        }


        /* if data is contigous, convertor will return an offset
         * into users buffer - otherwise will return an allocated buffer 
         * that holds the packed data
         */
        iov.iov_base = &sendfrag->ib_buf.buf[header_length];
        iov.iov_len = size_in;

        if((rc = ompi_convertor_pack(convertor, 
                        &iov, 1)) 
                < 0) {

            ompi_output(0, "Unable to pack data");

            return OMPI_ERROR;
        }

        /* adjust size and request offset to reflect actual 
         * number of bytes packed by convertor */
        size_out = iov.iov_len;
        IB_PREPARE_SEND_DESC((&sendfrag->ib_buf), 0, 
                (header_length + iov.iov_len));
    } else {
        size_out = size_in;
        IB_PREPARE_SEND_DESC((&sendfrag->ib_buf), 0, 
                (header_length + size_in));
    }

    hdr->hdr_frag.hdr_frag_length = size_out;

    /* fragment state */
    sendfrag->frag_send.frag_base.frag_owner = 
        &ptl_peer->peer_module->super;
    sendfrag->frag_send.frag_request = sendreq;

    sendfrag->frag_send.frag_base.frag_addr = iov.iov_base;
    sendfrag->frag_send.frag_base.frag_size = size_out;

    sendfrag->frag_send.frag_base.frag_peer = ptl_peer;
    sendfrag->frag_progressed = 0;

    *size = size_out;

    return OMPI_SUCCESS;
}

/*
 * Allocate a IB send descriptor
 *
 */
mca_ptl_ib_send_frag_t* mca_ptl_ib_alloc_send_frag(
        mca_ptl_base_module_t* ptl,
        mca_pml_base_send_request_t* request)
{
    ompi_free_list_t *flist;
    ompi_list_item_t *item;
    mca_ptl_ib_send_frag_t *ib_send_frag;

    flist = &((mca_ptl_ib_module_t *)ptl)->send_free;

    item = ompi_list_remove_first(&((flist)->super));

    while(NULL == item) {
        mca_ptl_tstamp_t tstamp = 0;

        D_PRINT("Gone one NULL descriptor ... trying again");

        ptl->ptl_component->ptlm_progress (tstamp);
        item = ompi_list_remove_first (&((flist)->super));
    }

    ib_send_frag = (mca_ptl_ib_send_frag_t *)item;

    return ib_send_frag;
}


int mca_ptl_ib_register_send_frags(mca_ptl_base_module_t *ptl)
{
    int i, rc, num_send_frags;
    ompi_list_item_t *item;
    ompi_free_list_t *flist;
    ib_buffer_t *ib_buf_ptr;
    mca_ptl_ib_send_frag_t *ib_send_frag;
    mca_ptl_ib_state_t *ib_state;

    flist = &((mca_ptl_ib_module_t *)ptl)->send_free;

    ib_state = ((mca_ptl_ib_module_t *)ptl)->ib_state;

    num_send_frags = ompi_list_get_size(&(flist->super));

    item = ompi_list_get_first(&((flist)->super));

    /* Register the buffers */
    for(i = 0; i < num_send_frags; 
            item = ompi_list_get_next(item), i++) {

        ib_send_frag = (mca_ptl_ib_send_frag_t *) item;

        ib_send_frag->frag_progressed = 0;

        ib_buf_ptr = (ib_buffer_t *) &ib_send_frag->ib_buf;

        rc = mca_ptl_ib_register_mem(ib_state->nic, ib_state->ptag,
                (void*) ib_buf_ptr->buf, 
                4096, &ib_buf_ptr->hndl);
        if(rc != OMPI_SUCCESS) {
            return OMPI_ERROR;
        }

        IB_PREPARE_SEND_DESC(ib_buf_ptr, 0, 4096);
    }

    return OMPI_SUCCESS;
}
