/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * $HEADER$
 */
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "include/constants.h"
#include "event/event.h"
#include "util/argv.h"
#include "util/output.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "mca/base/mca_base_param.h"
#include "ptl_self.h"

mca_ptl_t mca_ptl_self = {
    &mca_ptl_self_module.super,
    8, /* ptl_cache_size */
    sizeof(mca_ptl_base_recv_frag_t), /* ptl_cache_bytes */
    0, /* ptl_frag_first_size */
    0, /* ptl_frag_min_size */
    0, /* ptl_frag_max_size */
    65535, /* ptl_exclusivity */
    0, /* ptl_latency */
    0, /* ptl_bandwidth */
    MCA_PTL_PUT, /* ptl flags */
    mca_ptl_self_add_proc,
    mca_ptl_self_del_proc,
    mca_ptl_self_finalize,
    mca_ptl_self_send,  /* put */
    mca_ptl_self_send,  /* put */
    NULL, /* get */
    mca_ptl_self_matched, /* matched */
    mca_ptl_self_request_init,
    mca_ptl_self_request_fini,
    NULL, /* match */
    NULL,
    NULL
};

extern mca_ptl_self_module_1_0_0_t mca_ptl_self_module ;

int mca_ptl_self_add_proc(struct mca_ptl_t* ptl, size_t nprocs, struct ompi_proc_t **ompi_proc, struct mca_ptl_base_peer_t** peer_ret, ompi_bitmap_t* reachable)
{
    int i, count;

    mca_ptl_self_module.self_local = ompi_proc_local();

    for( i = 0, count = 0; i < nprocs; i++ ) {
        if( ompi_proc[i] == mca_ptl_self_module.self_local ) {
            ompi_bitmap_set_bit( reachable, i );
            count++;
        }
    }
    return OMPI_SUCCESS;
}

int mca_ptl_self_del_proc(struct mca_ptl_t* ptl, size_t nprocs, struct ompi_proc_t **proc, struct mca_ptl_base_peer_t** ptl_peer)
{
    return OMPI_SUCCESS;
}

/* before the module is unloaded (called once)*/
int mca_ptl_self_finalize(struct mca_ptl_t* ptl)
{
    return OMPI_SUCCESS;
}

int mca_ptl_self_request_init(struct mca_ptl_t* ptl, mca_pml_base_send_request_t* request)
{
    OBJ_CONSTRUCT(request+1, mca_ptl_base_recv_frag_t);
    return OMPI_SUCCESS;
}

void mca_ptl_self_request_fini(struct mca_ptl_t* ptl, mca_pml_base_send_request_t* request)
{
    OBJ_DESTRUCT(request+1);
}

/*
 *  Initiate a send. If this is the first fragment, use the fragment
 *  descriptor allocated with the send requests, otherwise obtain
 *  one from the free list. Initialize the fragment and foward
 *  on to the peer.
 */

int mca_ptl_self_send(
                      struct mca_ptl_t* ptl,
                      struct mca_ptl_base_peer_t* ptl_base_peer,
                      struct mca_pml_base_send_request_t* request,
                      size_t offset,
                      size_t size,
                      int flags )
{
    mca_ptl_self_send_request_t* req = (mca_ptl_self_send_request_t*)request;
    mca_ptl_base_header_t* hdr = &(req->req_frag.frag_base.frag_header);

    hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_MATCH;
    hdr->hdr_common.hdr_flags = flags;
    hdr->hdr_common.hdr_size = sizeof(mca_ptl_base_match_header_t);
    hdr->hdr_frag.hdr_frag_offset = offset;
    hdr->hdr_frag.hdr_frag_seq = 0;
    hdr->hdr_match.hdr_contextid = request->req_base.req_comm->c_contextid;
    hdr->hdr_match.hdr_src = request->req_base.req_comm->c_my_rank;
    hdr->hdr_match.hdr_dst = request->req_base.req_peer;
    hdr->hdr_match.hdr_tag = request->req_base.req_tag;
    hdr->hdr_match.hdr_msg_length = request->req_bytes_packed;
    hdr->hdr_match.hdr_msg_seq = request->req_base.req_sequence;
    hdr->hdr_frag.hdr_frag_length = request->req_bytes_packed;
    hdr->hdr_frag.hdr_frag_offset = 0 ;
    hdr->hdr_frag.hdr_src_ptr.pval = (void*)req;
    req->req_frag.frag_base.frag_peer = ptl_base_peer;
    req->req_frag.frag_base.frag_size = request->req_bytes_packed;
    req->req_frag.frag_base.frag_owner = &mca_ptl_self;
    req->req_frag.frag_request = NULL;
    req->req_frag.frag_is_buffered = 0;
    ptl->ptl_match( ptl, &(req->req_frag), &(hdr->hdr_match) );
    return OMPI_SUCCESS;
}


/*
 *  A posted receive has been matched - if required send an
 *  ack back to the peer and process the fragment.
 */
void mca_ptl_self_matched( mca_ptl_t* ptl,
                           mca_ptl_base_recv_frag_t* frag)
{
    mca_ptl_self_send_request_t* sendreq = (mca_ptl_self_send_request_t*)
        frag->frag_base.frag_header.hdr_frag.hdr_src_ptr.pval;
    mca_pml_base_recv_request_t* recvreq = frag->frag_request;

    /* Did you have the same datatype or not ? If yes we can use an optimized version
     * for the copy function, if not we have to use a temporary buffer to pack/unpack
     * 
     * Note that if this is a buffered send - the data has already been packed into
     * a contigous buffer and the convertor on the send request initialized to point
     * into this buffer.
     */
    if( sendreq->req_send.req_base.req_datatype == recvreq->req_base.req_datatype &&
        sendreq->req_send.req_send_mode != MCA_PML_BASE_SEND_BUFFERED) {
        ompi_ddt_copy_content_same_ddt( 
            recvreq->req_base.req_datatype, 
            recvreq->req_base.req_count,
            recvreq->req_base.req_addr, 
            sendreq->req_send.req_base.req_addr );
    } else {
        ompi_convertor_t *pSendConvertor, *pRecvConvertor;
        struct iovec iov[1];
        int completed, iov_count, length;
        char* buf;
        
        /* We use a temporary buffer as it look to be faster on much architectures */
        length = 64 * 1024;
        buf = malloc( length * sizeof(char) );

        ompi_convertor_init_for_recv( 
            &frag->frag_base.frag_convertor, 
            0, 
            recvreq->req_base.req_datatype, 
            recvreq->req_base.req_count, 
            recvreq->req_base.req_addr, 0 );
        pSendConvertor = &(sendreq->req_send.req_convertor);
        pRecvConvertor = &(frag->frag_base.frag_convertor);
        completed = 0;
        while( !completed ) {
            iov[0].iov_base = buf;
            iov[0].iov_len = length;
            iov_count = 1;
            completed |= ompi_convertor_pack( pSendConvertor, iov, iov_count );
            /*assert( freeAfter == 0 );*/
            completed |= ompi_convertor_unpack( pRecvConvertor, iov, iov_count );
            /*assert( freeAfter == 0 );*/
        }
	free( buf );
    }
    ptl->ptl_send_progress( ptl, &sendreq->req_send, sendreq->req_send.req_bytes_packed );
    ptl->ptl_recv_progress( ptl, 
        recvreq, 
        frag->frag_base.frag_header.hdr_frag.hdr_frag_length,
        frag->frag_base.frag_size );
}

