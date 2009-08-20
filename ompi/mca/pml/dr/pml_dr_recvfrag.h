/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 *  @file
 */
                                                                                                                                                 
#ifndef MCA_PML_DR_RECVFRAG_H
#define MCA_PML_DR_RECVFRAG_H

#include "ompi/mca/btl/btl.h"
#include "ompi/mca/bml/bml.h" 
#include "pml_dr_hdr.h"

BEGIN_C_DECLS

struct mca_pml_dr_buffer_t {
    ompi_free_list_item_t super;
    size_t len;
    unsigned char addr[1];
};
typedef struct mca_pml_dr_buffer_t mca_pml_dr_buffer_t;

OBJ_CLASS_DECLARATION(mca_pml_dr_buffer_t);

struct mca_pml_dr_recv_frag_t {
    ompi_free_list_item_t super;
    mca_pml_dr_hdr_t hdr;
    struct mca_pml_dr_recv_request_t* request;
    size_t num_segments;
    uint32_t csum;
    mca_btl_base_module_t* btl;
    ompi_proc_t* proc;
    mca_btl_base_segment_t segments[MCA_BTL_DES_MAX_SEGMENTS];
    mca_pml_dr_buffer_t* buffers[MCA_BTL_DES_MAX_SEGMENTS];
};
typedef struct mca_pml_dr_recv_frag_t mca_pml_dr_recv_frag_t;

OBJ_CLASS_DECLARATION(mca_pml_dr_recv_frag_t);


#define MCA_PML_DR_RECV_FRAG_ALLOC(frag,rc)                          \
do {                                                                 \
    ompi_free_list_item_t* item;                                     \
    OMPI_FREE_LIST_WAIT(&mca_pml_dr.recv_frags, item, rc);           \
    frag = (mca_pml_dr_recv_frag_t*)item;                            \
} while(0)


#define MCA_PML_DR_RECV_FRAG_INIT(frag,oproc,hdr,segs,cnt,btl,csum)     \
do {                                                                    \
    size_t i, length = 0;                                               \
    uint32_t ui1 = 0;                                                   \
    size_t ui2 = 0;                                                     \
    mca_pml_dr_buffer_t** buffers = frag->buffers;                      \
    bool do_csum = mca_pml_dr.enable_csum &&                            \
        (btl->btl_flags & MCA_BTL_FLAGS_NEED_CSUM);                     \
    /* init recv_frag */                                                \
    frag->btl = btl;                                                    \
    frag->hdr = *(mca_pml_dr_hdr_t*)hdr;                                \
    frag->num_segments = cnt;                                           \
    frag->proc = oproc;                                                 \
                                                                        \
    csum = OPAL_CSUM_ZERO;                                              \
    /* copy over data */                                                \
    for( i = 0; i < cnt; i++ ) {                                        \
        ompi_free_list_item_t* item;                                    \
        mca_pml_dr_buffer_t* buff;                                      \
        OMPI_FREE_LIST_WAIT(&mca_pml_dr.buffers, item, rc);             \
        buff = (mca_pml_dr_buffer_t*)item;                              \
        buffers[i] = buff;                                              \
        frag->segments[i].seg_addr.pval = buff->addr;                   \
        frag->segments[i].seg_len = segs[i].seg_len;                    \
        if( do_csum ) {                                                 \
            size_t hdr_len = 0;                                         \
            if( 0 == i ) {                                              \
                hdr_len = mca_pml_dr_hdr_size(hdr->hdr_common.hdr_type);\
                memcpy( buff->addr,                                     \
                        segs[i].seg_addr.pval,                          \
                        hdr_len );                                      \
            }                                                           \
            csum = OPAL_CSUM_BCOPY_PARTIAL(                             \
                     ((unsigned char*)segs[i].seg_addr.pval)+hdr_len,   \
                     ((unsigned char*)buff->addr)+hdr_len,              \
                     segs[i].seg_len-hdr_len, segs[i].seg_len-hdr_len,  \
                     &ui1, &ui2);                                       \
            length += segs[i].seg_len - hdr_len;                        \
        } else {                                                        \
            memcpy( buff->addr,                                         \
                    segs[i].seg_addr.pval,                              \
                    segs[i].seg_len );                                  \
        }                                                               \
    }                                                                   \
    frag->csum = csum;                                                  \
} while(0)


#define MCA_PML_DR_RECV_FRAG_RETURN(frag)                       \
do {                                                            \
    size_t i;                                                   \
                                                                \
    /* return buffers */                                        \
    for(i=0; i<frag->num_segments; i++) {                       \
        OMPI_FREE_LIST_RETURN(&mca_pml_dr.buffers,              \
                 (ompi_free_list_item_t*)frag->buffers[i]);     \
    }                                                           \
    frag->num_segments = 0;                                     \
                                                                \
    /* return recv_frag */                                      \
    OMPI_FREE_LIST_RETURN(&mca_pml_dr.recv_frags,               \
        (ompi_free_list_item_t*)frag);                          \
} while(0)


/**
 *  Generate an ack to the peer.
 */

void mca_pml_dr_recv_frag_ack(
                              mca_btl_base_module_t* btl,
                              mca_bml_base_endpoint_t* endpoint,
                              mca_pml_dr_common_hdr_t* hdr,
                              void* src_ptr,
                              uint64_t mask, 
                              uint16_t len); 
    
/**
 *  Callback from BTL on receipt of a recv_frag.
 */

void mca_pml_dr_recv_frag_callback( mca_btl_base_module_t *btl, 
                                    mca_btl_base_tag_t tag,
                                    mca_btl_base_descriptor_t* descriptor,
                                    void* cbdata);

/**
 * Match incoming recv_frags against posted receives.  
 * Supports out of order delivery.
 * 
 * @param frag_header (IN)          Header of received recv_frag.
 * @param frag_desc (IN)            Received recv_frag descriptor.
 * @param match_made (OUT)          Flag indicating wether a match was made.
 * @param additional_matches (OUT)  List of additional matches 
 * @return                          OMPI_SUCCESS or error status on failure.
 */
bool mca_pml_dr_recv_frag_match( mca_pml_dr_comm_t* comm,
                                 mca_pml_dr_comm_proc_t* proc,
                                 mca_btl_base_module_t* btl, 
                                 mca_pml_dr_match_hdr_t *hdr,
                                 mca_btl_base_segment_t* segments,
                                 size_t num_segments);

END_C_DECLS
#endif

