/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_BASE_RECVFRAG_H
#define MCA_PTL_BASE_RECVFRAG_H

#include "mca/ptl/ptl.h"
#include "mca/ptl/base/ptl_base_fragment.h"
#include "mca/ptl/base/ptl_base_recvreq.h"
#include "mca/ptl/base/ptl_base_match.h"

extern lam_class_t mca_ptl_base_recv_frag_t_class;

/**
 * Base type for receive fragment descriptors.
 */
struct mca_ptl_base_recv_frag_t {
    mca_ptl_base_frag_t super; /**< base fragment descriptor */
    mca_ptl_base_recv_request_t *frag_request; /**< matched posted receive */
    bool frag_is_buffered; /**< does fragment need to be unpacked into users buffer */
};
typedef struct mca_ptl_base_recv_frag_t mca_ptl_base_recv_frag_t;


/**
 * Initialize the receive fragment after a match has been made.
 * 
 * @param frag (IN)  Receive fragment descriptor.
 *
 * If a buffer has not already been allocated, determine the
 * offset into the users buffer (if contigous data), or allocate
 * a buffer for the non-contigous case. 
 *
 * TODO: may need to pass in an allocator....
 */
static inline void mca_ptl_base_recv_frag_init(mca_ptl_base_recv_frag_t* frag)
{
    mca_ptl_base_recv_request_t* request = frag->frag_request;
    mca_ptl_base_match_header_t* header = &frag->super.frag_header.hdr_match;

    /* initialize status */
    request->super.req_status.MPI_SOURCE = header->hdr_src;
    request->super.req_status.MPI_TAG = header->hdr_tag;
    request->super.req_status.MPI_ERROR = LAM_SUCCESS;
    request->super.req_status._count = header->hdr_msg_length;

    if(header->hdr_frag.hdr_frag_length > 0) {

        /* initialize receive convertor */
        lam_proc_t *proc = 
            lam_comm_peer_lookup(request->super.req_comm, request->super.req_peer);
        lam_convertor_copy(proc->proc_convertor, &frag->super.frag_convertor);
        lam_convertor_init_for_recv(
            &frag->super.frag_convertor,  /* convertor */
            0,                            /* flags */
            request->super.req_datatype,  /* datatype */
            request->super.req_count,     /* count elements */
            request->super.req_addr,      /* users buffer */
            header->hdr_frag.hdr_frag_offset);  /* offset in bytes into packed buffer */

        /* if buffer has not already been allocated for eager
         * send - go ahead and figure out offset into users 
         * buffer (for contigous data) - or allocate a buffer
         * for the receive if required.
        */
        if(NULL == frag->super.frag_addr) {
            struct iovec iov;
            iov.iov_base = NULL;
            iov.iov_len = header->hdr_frag.hdr_frag_length;
            lam_convertor_unpack(&frag->super.frag_convertor, &iov, 1);

            /* non-contiguous - allocate buffer for receive */
            if(NULL == iov.iov_base) {
                frag->super.frag_addr = malloc(iov.iov_len);
                frag->frag_is_buffered = true;
            /* we now have correct offset into users buffer */
            } else {
                frag->super.frag_addr = iov.iov_base;
                frag->frag_is_buffered = false;
            }
            frag->super.frag_size = header->hdr_frag.hdr_frag_length;
        }
    }
}
                                                                                                                   
/**
 * Called by the PTL to match attempt a match for new fragments.
 * 
 * @param frag (IN)     Receive fragment descriptor.
 * @param header (IN)   Header corresponding to the receive fragment.
 * @return              LAM_SUCCESS or error status on failure.
 */
static inline int mca_ptl_base_recv_frag_match(
    mca_ptl_base_recv_frag_t* frag, 
    mca_ptl_base_match_header_t* header)
{
    bool matched;
    lam_list_t matched_frags;
    int rc;

    OBJ_CONSTRUCT(&matched_frags, lam_list_t);
    if((rc = mca_ptl_base_match(header, frag, &matched, &matched_frags)) != LAM_SUCCESS)
        return rc;

    if(matched) {
        do {
            mca_ptl_t* ptl = frag->super.frag_owner;

            /* initialize current fragment */
            mca_ptl_base_recv_frag_init(frag);
            
            /* notify ptl of match */
            ptl->ptl_recv(ptl, frag);

            /* process any additional fragments that arrived out of order */
            frag = (mca_ptl_base_recv_frag_t*)lam_list_remove_first(&matched_frags);
        } while(NULL != frag);
    }
    return LAM_SUCCESS;
}


#endif

