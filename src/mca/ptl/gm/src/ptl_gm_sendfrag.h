/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_GM_SEND_FRAG_H
#define MCA_PTL_GM_SEND_FRAG_H

#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"

#define MATCH 0
#define FRAG 1
#define ACK 2
#define PUT 3 

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    OBJ_CLASS_DECLARATION (mca_ptl_gm_send_frag_t);
    OBJ_CLASS_DECLARATION (mca_ptl_gm_recv_frag_t);

    /* header definition for intermediary fragments on eager p2p communications */
    struct mca_ptl_gm_eager_header_t {
        mca_ptl_base_common_header_t hdr_common; /**< common attributes */
        ompi_ptr_t hdr_src_ptr;                  /**< pointer to source fragment */
    };
    typedef struct mca_ptl_gm_eager_header_t mca_ptl_gm_eager_header_t;

    /*struct mca_ptl_base_peer_t;*/

    /**
     * GM send fragment derived type.
     */
    struct mca_ptl_gm_send_frag_t {
        mca_ptl_base_send_frag_t send_frag; /**< base send fragment descriptor */
        void * send_buf;
        void * registered_buf;
        struct mca_pml_base_send_request_t *req;
        struct mca_ptl_gm_module_t *ptl;
        struct mca_ptl_gm_peer_t *peer;
    
        uint32_t already_send;  /**< data sended so far */
        int      status;
        int      type;
        int      wait_for_ack; 
        int      put_sent;
        int      send_complete;
    };
    typedef struct mca_ptl_gm_send_frag_t mca_ptl_gm_send_frag_t;

    struct mca_ptl_gm_recv_frag_t {
        mca_ptl_base_recv_frag_t frag_recv;
        size_t frag_hdr_cnt;
        size_t frag_msg_cnt;
        volatile int frag_progressed;
        bool frag_ack_pending;
        void *alloc_recv_buffer;
        void *unex_recv_buffer;
        void * registered_buf;
        struct mca_ptl_gm_module_t *ptl;
        bool matched;
        bool have_allocated_buffer;
    };
    typedef struct mca_ptl_gm_recv_frag_t mca_ptl_gm_recv_frag_t;

    mca_ptl_gm_send_frag_t *
    mca_ptl_gm_alloc_send_frag ( struct mca_ptl_gm_module_t* ptl,
                                 struct mca_pml_base_send_request_t* sendreq );

    int mca_ptl_gm_send_ack_init( struct mca_ptl_gm_send_frag_t* ack,
                                  struct mca_ptl_gm_module_t *ptl,
                                  struct mca_ptl_gm_peer_t* ptl_peer,
                                  struct mca_ptl_gm_recv_frag_t* frag,
                                  char * buffer,
                                  int size );

    int
    mca_ptl_gm_put_frag_init( struct mca_ptl_gm_send_frag_t* sendfrag,
                              struct mca_ptl_gm_peer_t * ptl_peer,
                              struct mca_ptl_gm_module_t *ptl,
                              struct mca_pml_base_send_request_t * sendreq,
                              size_t offset,
                              size_t* size,
                              int flags );

    static inline int
    mca_ptl_gm_init_header_match( struct mca_ptl_gm_send_frag_t* sendfrag,
                                  struct mca_pml_base_send_request_t * sendreq,
                                  int flags )
    {
        mca_ptl_base_header_t *hdr = (mca_ptl_base_header_t *)sendfrag->send_buf;
        
        hdr->hdr_common.hdr_flags = flags;
        hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_MATCH;
        
        hdr->hdr_match.hdr_contextid  = sendreq->req_base.req_comm->c_contextid;
        hdr->hdr_match.hdr_src        = sendreq->req_base.req_comm->c_my_rank;
        hdr->hdr_match.hdr_dst        = sendreq->req_base.req_peer;
        hdr->hdr_match.hdr_tag        = sendreq->req_base.req_tag;
        hdr->hdr_match.hdr_msg_length = sendreq->req_bytes_packed;
        hdr->hdr_match.hdr_msg_seq    = sendreq->req_base.req_sequence;
        sendfrag->type = MATCH;

        return OMPI_SUCCESS;
    }

    static inline int
    mca_ptl_gm_init_header_frag( struct mca_ptl_gm_send_frag_t* sendfrag,
                                 struct mca_ptl_gm_peer_t * ptl_peer,
                                 struct mca_pml_base_send_request_t * sendreq,
                                 size_t offset,
                                 size_t* size,
                                 int flags )

    {
        mca_ptl_base_header_t *hdr = (mca_ptl_base_header_t *)sendfrag->send_buf; 
    
        hdr->hdr_common.hdr_flags = flags;
        hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_FRAG;
        hdr->hdr_frag.hdr_frag_offset  = offset;
        hdr->hdr_frag.hdr_frag_length  = *size;
        hdr->hdr_frag.hdr_src_ptr.lval = 0;
        hdr->hdr_frag.hdr_src_ptr.pval = sendfrag; /* pointer to the frag */
        hdr->hdr_frag.hdr_dst_ptr = sendreq->req_peer_match;
        sendfrag->type = FRAG;

        return OMPI_SUCCESS;
    }

    int mca_ptl_gm_send_frag_done( struct mca_ptl_gm_send_frag_t* frag,
                                   struct mca_pml_base_send_request_t* req);

    mca_ptl_gm_recv_frag_t *
    mca_ptl_gm_alloc_recv_frag( struct mca_ptl_base_module_t *ptl );

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
