/* 
 * $HEADER$
 */
/**
 * @file
 */

#ifndef MCA_PTL_TCP_RECV_FRAG_H
#define MCA_PTL_TCP_RECV_FRAG_H

#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "os/atomic.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "ptl_tcp.h"


extern lam_class_t mca_ptl_tcp_recv_frag_t_class;


/**
 *  TCP received fragment derived type.
 */
struct mca_ptl_tcp_recv_frag_t {
    mca_ptl_base_recv_frag_t super; /**< base receive fragment descriptor */
    size_t frag_hdr_cnt;            /**< number of header bytes received */
    size_t frag_msg_cnt;            /**< number of message bytes received */
    bool frag_ack_pending;          /**< is an ack pending for this fragment */
    volatile int frag_progressed;   /**< flag used to atomically progress fragment */
};
typedef struct mca_ptl_tcp_recv_frag_t mca_ptl_tcp_recv_frag_t;


#define MCA_PTL_TCP_RECV_FRAG_ALLOC(frag, rc) \
    { \
    lam_list_item_t* item; \
    LAM_FREE_LIST_GET(&mca_ptl_tcp_module.tcp_recv_frags, item, rc); \
    frag = (mca_ptl_tcp_recv_frag_t*)item; \
    }


bool mca_ptl_tcp_recv_frag_handler(mca_ptl_tcp_recv_frag_t*, int sd);
void mca_ptl_tcp_recv_frag_init(mca_ptl_tcp_recv_frag_t* frag, struct mca_ptl_base_peer_t* peer);
bool mca_ptl_tcp_recv_frag_send_ack(mca_ptl_tcp_recv_frag_t* frag);



static inline void mca_ptl_tcp_recv_frag_matched(mca_ptl_tcp_recv_frag_t* frag)
{
    mca_ptl_base_recv_request_t* request = frag->super.frag_request;
    mca_ptl_base_frag_header_t* header = &frag->super.super.frag_header.hdr_frag;
  
    /* if there is data associated with the fragment -- setup to receive */
    if(header->hdr_frag_length > 0) {
                                                                                                                           
        /* initialize receive convertor */
        struct iovec iov;
        lam_proc_t *proc =
            lam_comm_peer_lookup(request->super.req_comm, request->super.req_peer);
        lam_convertor_copy(proc->proc_convertor, &frag->super.super.frag_convertor);
        lam_convertor_init_for_recv(
            &frag->super.super.frag_convertor,  /* convertor */
            0,                            /* flags */
            request->super.req_datatype,  /* datatype */
            request->super.req_count,     /* count elements */
            request->super.req_addr,      /* users buffer */
            header->hdr_frag_offset);  /* offset in bytes into packed buffer */
                                                                                                                           
        /*
         * determine offset into users buffer (for contigous data) -
         * or allocate a buffer for the receive if required.
        */
        iov.iov_base = NULL;
        iov.iov_len = header->hdr_frag_length;
        lam_convertor_unpack(&frag->super.super.frag_convertor, &iov, 1);
                                                                                                                           
        /* non-contiguous - allocate buffer for receive */
        if(NULL == iov.iov_base) {
                frag->super.super.frag_addr = malloc(iov.iov_len);
                frag->super.super.frag_size = header->hdr_frag_length;
                frag->super.frag_is_buffered = true;
        /* we now have correct offset into users buffer */
        } else {
                frag->super.super.frag_addr = iov.iov_base;
                frag->super.super.frag_size = iov.iov_len;
        }
    }
}


static inline void mca_ptl_tcp_recv_frag_progress(mca_ptl_tcp_recv_frag_t* frag) 
{ 
    if((frag)->frag_msg_cnt >= (frag)->super.super.frag_header.hdr_frag.hdr_frag_length) { 
        mca_ptl_base_recv_request_t* request = (frag)->super.frag_request; 
 
        /* make sure this only happens once for threaded case */ 
        if(lam_using_threads()) { 
            if(fetchNset(&(frag)->frag_progressed, 1) == 1) 
                return;
        } else { 
            if((frag)->frag_progressed == 1) 
                return;
            (frag)->frag_progressed = 1; 
        } 
 
        if((frag)->super.frag_is_buffered) { 
            mca_ptl_base_match_header_t* header = &(frag)->super.super.frag_header.hdr_match; 
 
            /* 
             * Initialize convertor and use it to unpack data  
             */ 
            struct iovec iov; 
            lam_proc_t *proc = 
                    lam_comm_peer_lookup(request->super.req_comm, request->super.req_peer); 
            lam_convertor_copy(proc->proc_convertor, &(frag)->super.super.frag_convertor); 
            lam_convertor_init_for_recv( 
                    &(frag)->super.super.frag_convertor,  /* convertor */ 
                    0,                                  /* flags */ 
                    request->super.req_datatype,        /* datatype */ 
                    request->super.req_count,           /* count elements */ 
                    request->super.req_addr,            /* users buffer */ 
                    header->hdr_frag.hdr_frag_offset);  /* offset in bytes into packed buffer */ 
 
            iov.iov_base = (frag)->super.super.frag_addr; 
            iov.iov_len = (frag)->super.super.frag_size; 
            lam_convertor_unpack(&(frag)->super.super.frag_convertor, &iov, 1); 
        } 

        /* progress the request */ 
        (frag)->super.super.frag_owner->ptl_recv_progress(request, &(frag)->super); 
        if((frag)->frag_ack_pending == false) { 
            mca_ptl_tcp_recv_frag_return((frag)->super.super.frag_owner, (frag)); 
        }  
    } 
}

#endif

