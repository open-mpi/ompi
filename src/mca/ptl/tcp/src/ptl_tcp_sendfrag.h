/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_TCP_SEND_FRAG_H
#define MCA_PTL_TCP_SEND_FRAG_H

#include "ompi_config.h"
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#include "include/sys/atomic.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "ptl_tcp.h"
#include "ptl_tcp_recvfrag.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
extern ompi_class_t mca_ptl_tcp_send_frag_t_class;
struct mca_ptl_base_peer_t;


/**
 * TCP send fragment derived type.
 */
struct mca_ptl_tcp_send_frag_t {
   mca_ptl_base_send_frag_t frag_send;  /**< base send fragment descriptor */
   int32_t free_after;                  /**< keep trace of which vectors we have to free */
   struct iovec *frag_vec_ptr;          /**< pointer into iovec array */
   size_t frag_vec_cnt;                 /**< number of iovec structs left to process */
   struct iovec frag_vec[2];            /**< array of iovecs for send */
   struct iovec  frag_saved_vec;         /**< save the initial values from the current iovec */
   volatile int frag_progressed;        /**< for threaded case - has request status been updated */
};
typedef struct mca_ptl_tcp_send_frag_t mca_ptl_tcp_send_frag_t;


#define MCA_PTL_TCP_SEND_FRAG_ALLOC(item, rc)  \
    OMPI_FREE_LIST_GET(&mca_ptl_tcp_component.tcp_send_frags, item, rc);


bool mca_ptl_tcp_send_frag_handler(mca_ptl_tcp_send_frag_t*, int sd);


/**
 * Initialize a fragment descriptor.
 *
 * frag (IN)      Fragment
 * peer (IN)      PTL peer addressing information 
 * request (IN)   Send request
 * offset (IN)    Current offset into packed buffer
 * size (IN/OUT)  Requested size / actual size returned
 * flags (IN)
 */

int mca_ptl_tcp_send_frag_init(
    mca_ptl_tcp_send_frag_t*, 
    struct mca_ptl_base_peer_t*, 
    struct mca_pml_base_send_request_t*, 
    size_t offset,
    size_t* size,
    int flags);


/**
 * For fragments that require an acknowledgment, this routine will be called
 * twice, once when the send completes, and again when the acknowledgment is 
 * returned. Only the last caller should update the request status, so we
 * add a lock w/ the frag_progressed flag.
 */

static inline void mca_ptl_tcp_send_frag_progress(mca_ptl_tcp_send_frag_t* frag) 
{ 
    mca_pml_base_send_request_t* request = frag->frag_send.frag_request; 

    /* if this is an ack - simply return to pool */ 
    if(request == NULL) { 
        mca_ptl_tcp_send_frag_return(frag->frag_send.frag_base.frag_owner, frag); 

    /* otherwise, if the message has been sent, and an ack has already been 
     * received, go ahead and update the request status 
     */ 
    } else if (frag->frag_vec_cnt == 0 &&  
         ((frag->frag_send.frag_base.frag_header.hdr_common.hdr_flags & MCA_PTL_FLAGS_ACK_MATCHED) == 0 || 
          mca_pml_base_send_request_matched(request))) { 

        /* make sure this only happens once in threaded case */ 
        if(ompi_atomic_fetch_and_set_int(&frag->frag_progressed,1) == 0) {

            /* update request status */ 
            frag->frag_send.frag_base.frag_owner->ptl_send_progress(
                frag->frag_send.frag_base.frag_owner, 
                request, 
                frag->frag_send.frag_base.frag_size); 

            /* the first fragment is allocated with the request, 
             * all others need to be returned to free list  
             */ 
            if(request->req_cached == false || 
               frag->frag_send.frag_base.frag_header.hdr_frag.hdr_frag_offset != 0) {
                mca_ptl_tcp_send_frag_return(frag->frag_send.frag_base.frag_owner, frag); 
            }
        } 
    }
} 


static inline void mca_ptl_tcp_send_frag_init_ack(
    mca_ptl_tcp_send_frag_t* ack,
    struct mca_ptl_base_module_t* ptl,
    struct mca_ptl_base_peer_t* ptl_peer,
    mca_ptl_tcp_recv_frag_t* frag)
{
    mca_ptl_base_header_t* hdr = &ack->frag_send.frag_base.frag_header;
    mca_pml_base_recv_request_t* request = frag->frag_recv.frag_request;
    hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_ACK;
    hdr->hdr_common.hdr_flags = 0;
    hdr->hdr_common.hdr_size = sizeof(mca_ptl_base_ack_header_t);
    hdr->hdr_ack.hdr_src_ptr = frag->frag_recv.frag_base.frag_header.hdr_frag.hdr_src_ptr;
    hdr->hdr_ack.hdr_dst_match.lval = 0; /* for VALGRIND/PURIFY - REPLACE WITH MACRO */
    hdr->hdr_ack.hdr_dst_match.pval = request;
    hdr->hdr_ack.hdr_dst_addr.lval = 0; /* for VALGRIND/PURIFY - REPLACE WITH MACRO */
    hdr->hdr_ack.hdr_dst_addr.pval = request->req_base.req_addr;
    hdr->hdr_ack.hdr_dst_size = request->req_bytes_packed;
    ack->frag_send.frag_request = 0;
    ack->frag_send.frag_base.frag_peer = ptl_peer;
    ack->frag_send.frag_base.frag_owner = ptl;
    ack->frag_send.frag_base.frag_addr = NULL;
    ack->frag_send.frag_base.frag_size = 0;
    ack->frag_vec_ptr = ack->frag_vec;
    ack->frag_vec[0].iov_base = (ompi_iov_base_ptr_t)hdr;
    ack->frag_vec[0].iov_len = sizeof(mca_ptl_base_header_t);
    ack->frag_vec_cnt = 1;
    ack->free_after = 0;
}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

