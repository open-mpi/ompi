/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_TCP_SEND_FRAG_H
#define MCA_PTL_TCP_SEND_FRAG_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "lam_config.h"
#include "mca/ptl/base/ptl_base_sendreq.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "ptl_tcp.h"
#include "ptl_tcp_recvfrag.h"

extern lam_class_t mca_ptl_tcp_send_frag_t_class;
struct mca_ptl_base_peer_t;


/**
 * TCP send fragment derived type.
 */
struct mca_ptl_tcp_send_frag_t {
   mca_ptl_base_send_frag_t super;  /**< base send fragment descriptor */
   struct iovec *frag_vec_ptr;      /**< pointer into iovec array */
   size_t frag_vec_cnt;             /**< number of iovec structs left to process */
   struct iovec frag_vec[2];        /**< array of iovecs for send */
};
typedef struct mca_ptl_tcp_send_frag_t mca_ptl_tcp_send_frag_t;


static inline mca_ptl_tcp_send_frag_t* mca_ptl_tcp_send_frag_alloc(int* rc) 
{
    return (mca_ptl_tcp_send_frag_t*)lam_free_list_get(&mca_ptl_tcp_module.tcp_send_frags, rc);
}


bool mca_ptl_tcp_send_frag_handler(mca_ptl_tcp_send_frag_t*, int sd);


int mca_ptl_tcp_send_frag_init(
    mca_ptl_tcp_send_frag_t*, 
    struct mca_ptl_base_peer_t*, 
    struct mca_ptl_base_send_request_t*, 
    size_t size,
    int flags);


static inline void mca_ptl_tcp_send_frag_progress(mca_ptl_tcp_send_frag_t* frag)
{
    mca_ptl_base_send_request_t* request = frag->super.frag_request;

    /* if this is an ack - simply return to pool */
    if(request == NULL) {
        mca_ptl_tcp_send_frag_return(frag->super.super.frag_owner, frag);

    /* otherwise, if an ack is not required or has already been received, update request status */
    } else if ((frag->super.super.frag_header.hdr_common.hdr_flags & MCA_PTL_FLAGS_ACK_MATCHED) == 0 ||
               mca_ptl_base_send_request_matched(request) == true) {
        frag->super.super.frag_owner->ptl_send_progress(request, &frag->super);

        /* the first fragment is allocated with the request, 
         * all others need to be returned to free list 
         */
        if(frag->super.super.frag_header.hdr_frag.hdr_frag_offset != 0)
            mca_ptl_tcp_send_frag_return(frag->super.super.frag_owner, frag);
    }
}


static inline void mca_ptl_tcp_send_frag_init_ack(
    mca_ptl_tcp_send_frag_t* ack,
    struct mca_ptl_t* ptl,
    struct mca_ptl_base_peer_t* ptl_peer,
    mca_ptl_tcp_recv_frag_t* frag)
{
    mca_ptl_base_header_t* hdr = &ack->super.super.frag_header;
    hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_ACK;
    hdr->hdr_common.hdr_flags = 0;
    hdr->hdr_common.hdr_size = sizeof(mca_ptl_base_ack_header_t);
    hdr->hdr_ack.hdr_src_ptr = frag->super.super.frag_header.hdr_frag.hdr_src_ptr;
    hdr->hdr_ack.hdr_dst_ptr.lval = 0; /* for VALGRIND/PURIFY - REPLACE WITH MACRO */
    hdr->hdr_ack.hdr_dst_ptr.pval = frag->super.frag_request;
    ack->super.frag_request = 0;
    ack->super.super.frag_peer = ptl_peer;
    ack->super.super.frag_owner = ptl;
    ack->super.super.frag_addr = NULL;
    ack->super.super.frag_size = 0;
    ack->frag_vec_ptr = ack->frag_vec;
    ack->frag_vec[0].iov_base = (lam_iov_base_ptr_t)hdr;
    ack->frag_vec[0].iov_len = sizeof(mca_ptl_base_header_t);
    ack->frag_vec_cnt = 1;
}


#endif

