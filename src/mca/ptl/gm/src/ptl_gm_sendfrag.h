/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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

    /* specific header for GM rendezvous protocol. It will be filled up by the sender
     * and should be ab;e to hold a pointer to the last registered memory location.
     */
    struct mca_ptl_gm_rdv_header_t {
        mca_ptl_base_frag_header_t hdr_frag;
        ompi_ptr_t                 registered_memory;
    };
    typedef struct mca_ptl_gm_rdv_header_t mca_ptl_gm_rdv_header_t;

    struct mca_ptl_gm_peer_t;

    /**
     * GM send fragment derived type.
     */
    struct mca_ptl_gm_send_frag_t {
        mca_ptl_base_send_frag_t send_frag; /**< base send fragment descriptor */
        void* send_buf;
        ompi_ptr_t* registered_buf;
	
        uint64_t frag_bytes_processed;  /**< data sended so far */
	uint64_t frag_bytes_validated;  /**< amount of data for which we receive an ack */
	uint64_t frag_offset;           /**< initial offset of the fragment as specified by the upper level */
        int      status;
        int      type;
        int      wait_for_ack; 
        int      put_sent;
    };
    typedef struct mca_ptl_gm_send_frag_t mca_ptl_gm_send_frag_t;
    
    struct mca_ptl_gm_recv_frag_t {
        mca_ptl_base_recv_frag_t frag_recv;
        uint64_t     frag_bytes_processed;
	uint64_t     frag_bytes_validated;  /**< amount of data for which we receive an ack */
	uint64_t     frag_offset;
        volatile int frag_progressed;
        void*        alloc_recv_buffer;
        void*        registered_buf;
        bool         frag_ack_pending;
        bool         matched;
        bool         have_allocated_buffer;
        bool         have_registered_buffer;
        ompi_ptr_t   remote_registered_memory;
    };
    typedef struct mca_ptl_gm_recv_frag_t mca_ptl_gm_recv_frag_t;

    mca_ptl_gm_send_frag_t *
    mca_ptl_gm_alloc_send_frag ( struct mca_ptl_gm_module_t* ptl,
                                 struct mca_pml_base_send_request_t* sendreq );
    
    int mca_ptl_gm_send_ack_init( struct mca_ptl_gm_send_frag_t* ack,
                                  struct mca_ptl_gm_module_t *ptl,
                                  struct mca_ptl_gm_peer_t* ptl_peer,
                                  struct mca_ptl_gm_recv_frag_t* frag,
                                  char* buffer,
                                  int size );

    int
    mca_ptl_gm_put_frag_init( struct mca_ptl_gm_send_frag_t* sendfrag,
                              struct mca_ptl_gm_peer_t * ptl_peer,
                              struct mca_ptl_gm_module_t *ptl,
                              struct mca_pml_base_send_request_t * sendreq,
                              size_t offset,
                              size_t* size,
                              int flags );

#define OMPI_FREE_LIST_TRY_GET(fl, item) \
{ \
    item = NULL; \
    if(ompi_using_threads()) { \
        if( ompi_mutex_trylock( &((fl)->fl_lock)) ) { \
            /* We get the lock. Now let's remove one of the elements */ \
            item = ompi_list_remove_first(&((fl)->super)); \
            ompi_mutex_unlock(&((fl)->fl_lock)); \
        } \
    } else { \
        item = ompi_list_remove_first(&((fl)->super)); \
    }  \
}

    static inline int
    mca_ptl_gm_init_header_match( mca_ptl_base_header_t *hdr,
                                  struct mca_pml_base_send_request_t * sendreq,
                                  int flags )
    {
        hdr->hdr_common.hdr_flags = flags;
        hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_MATCH;
        
        hdr->hdr_match.hdr_contextid  = sendreq->req_base.req_comm->c_contextid;
        hdr->hdr_match.hdr_src        = sendreq->req_base.req_comm->c_my_rank;
        hdr->hdr_match.hdr_dst        = sendreq->req_base.req_peer;
        hdr->hdr_match.hdr_tag        = sendreq->req_base.req_tag;
        hdr->hdr_match.hdr_msg_length = sendreq->req_bytes_packed;
        hdr->hdr_match.hdr_msg_seq    = sendreq->req_base.req_sequence;

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

    static inline mca_ptl_gm_recv_frag_t*
    mca_ptl_gm_alloc_recv_frag( struct mca_ptl_base_module_t *ptl )
    {
        int rc;
        ompi_list_item_t* item;
        mca_ptl_gm_recv_frag_t* frag;
        
        OMPI_FREE_LIST_GET( &(((mca_ptl_gm_module_t *)ptl)->gm_recv_frags_free), item, rc );
        
        frag = (mca_ptl_gm_recv_frag_t*)item;
        frag->frag_recv.frag_base.frag_owner = (struct mca_ptl_base_module_t*)ptl;
        frag->frag_bytes_processed = 0;
        frag->frag_bytes_validated = 0;
	frag->frag_offset          = 0;
        return frag;
    }

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
