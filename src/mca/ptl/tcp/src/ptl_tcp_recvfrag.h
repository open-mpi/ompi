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
#include "ompi_config.h"
#include "include/sys/atomic.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "ptl_tcp_peer.h"
#include "ptl_tcp.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
extern ompi_class_t mca_ptl_tcp_recv_frag_t_class;

/**
 *  TCP received fragment derived type.
 */
struct mca_ptl_tcp_recv_frag_t {
    mca_ptl_base_recv_frag_t frag_recv;  /**< base receive fragment descriptor */
    size_t frag_hdr_cnt;                 /**< number of header bytes received */
    size_t frag_msg_cnt;                 /**< number of message bytes received */
    bool frag_ack_pending;               /**< is an ack pending for this fragment */
    volatile int frag_progressed;        /**< flag used to atomically progress fragment */
};
typedef struct mca_ptl_tcp_recv_frag_t mca_ptl_tcp_recv_frag_t;


#define MCA_PTL_TCP_RECV_FRAG_ALLOC(frag, rc) \
    { \
    ompi_list_item_t* item; \
    OMPI_FREE_LIST_GET(&mca_ptl_tcp_component.tcp_recv_frags, item, rc); \
    frag = (mca_ptl_tcp_recv_frag_t*)item; \
    }

bool mca_ptl_tcp_recv_frag_handler(mca_ptl_tcp_recv_frag_t*, int sd);

/*
 * Initialize a TCP receive fragment for a specific peer.
 */
static inline void mca_ptl_tcp_recv_frag_init(mca_ptl_tcp_recv_frag_t* frag, struct mca_ptl_base_peer_t* peer)
{
    frag->frag_recv.frag_base.frag_owner = &(peer->peer_ptl->super);
    frag->frag_recv.frag_base.frag_addr = NULL;
    frag->frag_recv.frag_base.frag_size = 0;
    frag->frag_recv.frag_base.frag_peer = peer;
    frag->frag_recv.frag_request = 0;
    frag->frag_recv.frag_is_buffered = false;
    frag->frag_hdr_cnt = 0;
    frag->frag_msg_cnt = 0;
    frag->frag_ack_pending = false;
    frag->frag_progressed = 0;
}
                                                                                                                
bool mca_ptl_tcp_recv_frag_send_ack(mca_ptl_tcp_recv_frag_t* frag);

extern void* ptl_tcp_memalloc( unsigned int* length );

static inline void mca_ptl_tcp_recv_frag_matched(mca_ptl_tcp_recv_frag_t* frag)
{
    mca_pml_base_recv_request_t* request = frag->frag_recv.frag_request;
    mca_ptl_base_frag_header_t* header = &frag->frag_recv.frag_base.frag_header.hdr_frag;
  
    /* if there is data associated with the fragment -- setup to receive */
    if(header->hdr_frag_length > 0) {
        /* initialize receive convertor */
        ompi_proc_t *proc =
            ompi_comm_peer_lookup(request->req_base.req_comm, 
                                  request->req_base.req_ompi.req_status.MPI_SOURCE);
        ompi_convertor_copy(proc->proc_convertor, &frag->frag_recv.frag_base.frag_convertor);
        ompi_convertor_init_for_recv(
            &frag->frag_recv.frag_base.frag_convertor,  /* convertor */
            0,                              /* flags */
            request->req_base.req_datatype, /* datatype */
            request->req_base.req_count,    /* count elements */
            request->req_base.req_addr,     /* users buffer */
            header->hdr_frag_offset,        /* offset in bytes into packed buffer */
            ptl_tcp_memalloc );             /* not allocating memory */

        /* non-contiguous - allocate buffer for receive */
        if( 1 == ompi_convertor_need_buffers( &frag->frag_recv.frag_base.frag_convertor ) ) {
            frag->frag_recv.frag_base.frag_addr = malloc(header->hdr_frag_length);
            frag->frag_recv.frag_is_buffered = true;
	    /* determine offset into users buffer */
        } else {
            frag->frag_recv.frag_base.frag_addr = ((unsigned char*)request->req_base.req_addr) + 
                header->hdr_frag_offset;
        }
	    frag->frag_recv.frag_base.frag_size = header->hdr_frag_length;
        if(header->hdr_frag_offset + frag->frag_recv.frag_base.frag_size > request->req_bytes_packed) {
            if(header->hdr_frag_offset > request->req_bytes_packed)
                frag->frag_recv.frag_base.frag_size = 0;
            else
                frag->frag_recv.frag_base.frag_size = request->req_bytes_packed - header->hdr_frag_offset;
        }
    }
}


static inline void mca_ptl_tcp_recv_frag_progress(mca_ptl_tcp_recv_frag_t* frag) 
{
    unsigned int iov_count, max_data;
    int freeAfter;

    if((frag)->frag_msg_cnt >= frag->frag_recv.frag_base.frag_header.hdr_frag.hdr_frag_length) { 
        /* make sure this only happens once for threaded case */ 
        if(ompi_atomic_fetch_and_set_int(&frag->frag_progressed, 1) == 0) {
            mca_pml_base_recv_request_t* request = frag->frag_recv.frag_request; 
            if(frag->frag_recv.frag_is_buffered) { 
                mca_ptl_base_match_header_t* header = &(frag)->frag_recv.frag_base.frag_header.hdr_match; 
 
                /* 
                 * Initialize convertor and use it to unpack data  
                 */ 
                struct iovec iov; 
                ompi_proc_t *proc = 
                        ompi_comm_peer_lookup(request->req_base.req_comm, 
                                              request->req_base.req_ompi.req_status.MPI_SOURCE); 
                ompi_convertor_copy(proc->proc_convertor, &frag->frag_recv.frag_base.frag_convertor); 
                ompi_convertor_init_for_recv( 
                        &frag->frag_recv.frag_base.frag_convertor,  /* convertor */ 
                        0,                                 /* flags */ 
                        request->req_base.req_datatype,    /* datatype */ 
                        request->req_base.req_count,       /* count elements */ 
                        request->req_base.req_addr,        /* users buffer */ 
                        header->hdr_frag.hdr_frag_offset,  /* offset in bytes into packed buffer */ 
                        NULL );                            /* dont allocate memory */
		
                iov.iov_base = frag->frag_recv.frag_base.frag_addr; 
                iov.iov_len = frag->frag_recv.frag_base.frag_size;
                iov_count = 1;
                max_data = iov.iov_len;
                ompi_convertor_unpack( &frag->frag_recv.frag_base.frag_convertor,
                                       &iov, &iov_count, &max_data, &freeAfter ); 
            } 

            /* progress the request */ 
            frag->frag_recv.frag_base.frag_owner->ptl_recv_progress(
                frag->frag_recv.frag_base.frag_owner, 
                request, 
                frag->frag_recv.frag_base.frag_header.hdr_frag.hdr_frag_length,
                frag->frag_recv.frag_base.frag_size);

            if((frag)->frag_ack_pending == false) { 
                mca_ptl_tcp_recv_frag_return(frag->frag_recv.frag_base.frag_owner, (frag)); 
            }  
        }
    } 
}
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
