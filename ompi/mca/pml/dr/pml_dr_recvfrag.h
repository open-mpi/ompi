/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
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

struct mca_pml_dr_buffer_t {
    opal_list_item_t super;
    size_t len;
    unsigned char addr[1];
};
typedef struct mca_pml_dr_buffer_t mca_pml_dr_buffer_t;

OBJ_CLASS_DECLARATION(mca_pml_dr_buffer_t);


struct mca_pml_dr_recv_frag_t {
    opal_list_item_t super;
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


void mca_pml_dr_recv_frag_send_ack(ompi_proc_t* ompi_proc, 
                                   mca_pml_dr_common_hdr_t* hdr,
                                   ompi_ptr_t src_ptr,
                                   uint64_t mask); 




OBJ_CLASS_DECLARATION(mca_pml_dr_recv_frag_t);

#define MCA_PML_DR_RECV_FRAG_CHECK_DUP(hdr, dup)                     \
do {                                                                 \
    ompi_communicator_t *comm_ptr;                                   \
    mca_pml_dr_comm_t *comm;                                         \
    mca_pml_dr_comm_proc_t *proc;                                    \
    comm_ptr = ompi_comm_lookup(hdr->hdr_common.hdr_ctx);            \
    comm = (mca_pml_dr_comm_t*) comm_ptr->c_pml_comm;                \
    proc = comm->procs + hdr->hdr_common.hdr_src;                    \
    if(mca_pml_dr_comm_proc_check_acked(proc,                        \
                                        hdr->hdr_common.hdr_vid)) {  \
        mca_pml_dr_recv_frag_send_ack(proc->ompi_proc,               \
                                      &hdr->hdr_common,              \
                                      hdr->hdr_match.hdr_src_ptr,    \
                                      1);                            \
        dup = true;                                                  \
    } else {                                                         \
        dup = false;                                                 \
    }                                                                \
} while (0)

#define MCA_PML_DR_RECV_FRAG_ALLOC(frag,rc)                          \
do {                                                                 \
    opal_list_item_t* item;                                          \
    OMPI_FREE_LIST_WAIT(&mca_pml_dr.recv_frags, item, rc);           \
    frag = (mca_pml_dr_recv_frag_t*)item;                            \
} while(0)


#define MCA_PML_DR_RECV_FRAG_INIT(frag,oproc,hdr,segs,cnt,btl,csum)  \
do {                                                                 \
    size_t i;                                                        \
    uint32_t ui1 = 0;                                                \
    uint32_t ui2 = 0;                                                \
    mca_btl_base_segment_t* macro_segments = frag->segments;         \
    mca_pml_dr_buffer_t** buffers = frag->buffers;                   \
                                                                     \
    /* init recv_frag */                                             \
    frag->btl = btl;                                                 \
    frag->hdr = *(mca_pml_dr_hdr_t*)hdr;                             \
    frag->num_segments = cnt;                                        \
    frag->csum = 0;                                                  \
    frag->proc = oproc;                                              \
                                                                     \
    /* copy over data */                                             \
    for(i=0; i<cnt; i++) {                                           \
        opal_list_item_t* item;                                      \
        mca_pml_dr_buffer_t* buff;                                   \
        OMPI_FREE_LIST_WAIT(&mca_pml_dr.buffers, item, rc);          \
        buff = (mca_pml_dr_buffer_t*)item;                           \
        buffers[i] = buff;                                           \
        macro_segments[i].seg_addr.pval = buff->addr;                \
        macro_segments[i].seg_len = segs[i].seg_len;                 \
        csum += OPAL_CSUM_BCOPY_PARTIAL(                             \
               segs[i].seg_addr.pval,                                \
               buff->addr,                                           \
               segs[i].seg_len,                                      \
               segs[i].seg_len,                                      \
               &ui1,                                                 \
               &ui2);                                                \
    }                                                                \
} while(0)


#define MCA_PML_DR_RECV_FRAG_RETURN(frag)                       \
do {                                                            \
    size_t i;                                                   \
                                                                \
    /* return buffers */                                        \
    for(i=0; i<frag->num_segments; i++) {                       \
        OMPI_FREE_LIST_RETURN(&mca_pml_dr.buffers,              \
           (opal_list_item_t*)frag->buffers[i]);                \
    }                                                           \
    frag->num_segments = 0;                                     \
                                                                \
    /* return recv_frag */                                      \
    OMPI_FREE_LIST_RETURN(&mca_pml_dr.recv_frags,               \
        (opal_list_item_t*)frag);                               \
} while(0)


/**
 *  Callback from BTL on receipt of a recv_frag.
 */

OMPI_DECLSPEC void mca_pml_dr_recv_frag_callback(
    mca_btl_base_module_t *btl, 
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
OMPI_DECLSPEC bool mca_pml_dr_recv_frag_match(
    mca_btl_base_module_t* btl, 
    mca_pml_dr_match_hdr_t *hdr,
    mca_btl_base_segment_t* segments,
    size_t num_segments);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

