#include "mca/pml/base/pml_base_sendreq.h"
#include "ptl_ib.h"
#include "ptl_ib_peer.h"
#include "ptl_ib_recvfrag.h"
#include "ptl_ib_sendfrag.h"

static void mca_ptl_ib_recv_frag_construct(mca_ptl_ib_recv_frag_t* frag);
static void mca_ptl_ib_recv_frag_destruct(mca_ptl_ib_recv_frag_t* frag);

OBJ_CLASS_INSTANCE(mca_ptl_ib_recv_frag_t, 
        mca_ptl_base_recv_frag_t,
        mca_ptl_ib_recv_frag_construct, 
        mca_ptl_ib_recv_frag_destruct);

/*
 * IB fragment constructor
 */

static void mca_ptl_ib_recv_frag_construct(mca_ptl_ib_recv_frag_t* frag)
{
}


/*
 * IB fragment destructor
 */

static void mca_ptl_ib_recv_frag_destruct(mca_ptl_ib_recv_frag_t* frag)
{
}

void
mca_ptl_ib_recv_frag_done (mca_ptl_base_header_t *header,
        mca_ptl_base_recv_frag_t* frag,
        mca_pml_base_recv_request_t *request)
{
    D_PRINT("");
    frag->frag_base.frag_owner->ptl_recv_progress (
            frag->frag_base.frag_owner,
            request,
            frag->frag_base.frag_size,
            frag->frag_base.frag_size);

    /* Return recv frag to free list */
    OMPI_FREE_LIST_RETURN(&mca_ptl_ib_component.ib_recv_frags,
            (ompi_list_item_t*)frag);
}

static void mca_ptl_ib_data_frag(mca_ptl_base_module_t *module,
        mca_ptl_base_header_t *header)
{
    bool matched;
    int rc;
    ompi_list_item_t *item;
    mca_ptl_ib_recv_frag_t *recv_frag;

    OMPI_FREE_LIST_GET(&mca_ptl_ib_component.ib_recv_frags,
            item, rc);

    while (OMPI_SUCCESS != rc) {
        /* TODO: progress the recv state machine */
        D_PRINT("Retry to allocate a recv fragment\n");
        OMPI_FREE_LIST_GET (&mca_ptl_ib_component.ib_recv_frags,
                item, rc);
    }

    recv_frag = (mca_ptl_ib_recv_frag_t *) item;
    recv_frag->super.frag_base.frag_owner = module; 

    recv_frag->super.frag_base.frag_peer = NULL; 
    recv_frag->super.frag_request = NULL; 
    recv_frag->super.frag_is_buffered = false;

    /* Copy the header, mca_ptl_base_match()
     * does not do what it claims */
    recv_frag->super.frag_base.frag_header = *header;

    /* Taking the data starting point be
     * default */
    recv_frag->super.frag_base.frag_addr =
        (char *) header + sizeof (mca_ptl_base_header_t);
    recv_frag->super.frag_base.frag_size = header->hdr_frag.hdr_frag_length;

    /* match with preposted
     * requests */
    matched = module->ptl_match(
            recv_frag->super.frag_base.frag_owner,
            &recv_frag->super,
            &recv_frag->super.frag_base.frag_header.hdr_match);

    if (!matched) {
        /* Oh my GOD
         * !!! */
        //ompi_output(0, "Can't match buffer. Mama is unhappy\n");
        memcpy (recv_frag->unex_buf,
                (char *) header + sizeof (mca_ptl_base_header_t),
                header->hdr_frag.hdr_frag_length);
        recv_frag->super.frag_is_buffered = true; 
        recv_frag->super.frag_base.frag_addr = recv_frag->unex_buf;

    } else {
        D_PRINT("Message matched!");
    }
}

static void mca_ptl_ib_ctrl_frag(mca_ptl_base_module_t *module,
        mca_ptl_base_header_t *header)
{
    mca_ptl_ib_send_frag_t *send_frag;
    mca_pml_base_send_request_t *req;
    void *data_ptr;

    send_frag = (mca_ptl_ib_send_frag_t *)
        header->hdr_ack.hdr_src_ptr.pval;
    req = (mca_pml_base_send_request_t *) 
        send_frag->frag_send.frag_request;

    req->req_peer_match = header->hdr_ack.hdr_dst_match;
    req->req_peer_addr = header->hdr_ack.hdr_dst_addr;
    req->req_peer_size = header->hdr_ack.hdr_dst_size;

    /* Locate data in the ACK buffer */
    data_ptr = (void*)
        ((char*) header + sizeof(mca_ptl_base_ack_header_t));

    /* Copy over data to request buffer */
    memcpy(((mca_ptl_ib_send_request_t *) req)->req_buf,
            data_ptr, sizeof(VAPI_rkey_t));

    /* Progress & release fragments */
    mca_ptl_ib_process_send_comp(module, (void*) send_frag);
}

static void mca_ptl_ib_last_frag(mca_ptl_base_module_t *module,
        mca_ptl_base_header_t *hdr)
{
    mca_pml_base_recv_request_t *request;
    request = (mca_pml_base_recv_request_t*) hdr->hdr_frag.hdr_dst_ptr.pval;

    module->ptl_recv_progress (
            module,
            request,
            hdr->hdr_frag.hdr_frag_length,
            hdr->hdr_frag.hdr_frag_length);

}

/*
 * Process incoming receive fragments
 *
 */

void mca_ptl_ib_process_recv(mca_ptl_base_module_t *module, void* addr)
{
    ib_buffer_t *ib_buf;
    mca_ptl_base_header_t *header;

    D_PRINT("");

    ib_buf = (ib_buffer_t *) addr;

    header = (mca_ptl_base_header_t *) &ib_buf->buf[0];

    switch(header->hdr_common.hdr_type) {
        case MCA_PTL_HDR_TYPE_MATCH :
        case MCA_PTL_HDR_TYPE_FRAG :
            mca_ptl_ib_data_frag(module, header);
            break;
        case MCA_PTL_HDR_TYPE_ACK :
            mca_ptl_ib_ctrl_frag(module, header);
            break;
        case MCA_PTL_HDR_TYPE_FIN :
            A_PRINT("Fin");
            mca_ptl_ib_last_frag(module, header);
            break;
        default :
            ompi_output(0, "Unknown fragment type");
            break;
    }
}
