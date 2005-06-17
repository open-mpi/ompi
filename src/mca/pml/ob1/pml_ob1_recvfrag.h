/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
                                                                                                                                                 
#ifndef MCA_PML_OB1_RECVFRAG_H
#define MCA_PML_OB1_RECVFRAG_H

#include "mca/bmi/bmi.h"
#include "pml_ob1_hdr.h"

struct mca_pml_ob1_buffer_t {
    ompi_list_item_t super;
    unsigned char addr[1];
};
typedef struct mca_pml_ob1_buffer_t mca_pml_ob1_buffer_t;

OBJ_CLASS_DECLARATION(mca_pml_ob1_buffer_t);


struct mca_pml_ob1_recv_frag_t {
    ompi_list_item_t super;
    mca_bmi_base_module_t* bmi;
    mca_pml_ob1_hdr_t hdr;
    struct mca_pml_ob1_recv_request_t* request;
    size_t num_segments;
    mca_bmi_base_segment_t segments[MCA_BMI_DES_MAX_SEGMENTS];
    mca_pml_ob1_buffer_t* buffers[MCA_BMI_DES_MAX_SEGMENTS];
};
typedef struct mca_pml_ob1_recv_frag_t mca_pml_ob1_recv_frag_t;

OBJ_CLASS_DECLARATION(mca_pml_ob1_recv_frag_t);


#define MCA_PML_OB1_RECV_FRAG_ALLOC(frag,rc)                         \
do {                                                            \
    ompi_list_item_t* item;                                     \
    OMPI_FREE_LIST_WAIT(&mca_pml_ob1.recv_frags, item, rc);      \
    frag = (mca_pml_ob1_recv_frag_t*)item;                       \
} while(0)


#define MCA_PML_OB1_RECV_FRAG_INIT(frag,bmi,hdr,segs,cnt)            \
do {                                                            \
    size_t i;                                                   \
    mca_bmi_base_segment_t* macro_segments = frag->segments;          \
    mca_pml_ob1_buffer_t** buffers = frag->buffers;             \
                                                                \
    /* init recv_frag */                                         \
    frag->bmi = bmi;                                            \
    frag->hdr = *(mca_pml_ob1_hdr_t*)hdr;                       \
    frag->num_segments = cnt;                                   \
                                                                \
    /* copy over data */                                        \
    for(i=0; i<cnt; i++) {                                      \
        ompi_list_item_t* item;                                 \
        mca_pml_ob1_buffer_t* buff;                             \
        OMPI_FREE_LIST_WAIT(&mca_pml_ob1.buffers, item, rc);    \
        buff = (mca_pml_ob1_buffer_t*)item;                     \
        buffers[i] = buff;                                      \
        macro_segments[i].seg_addr.pval = buff->addr;                 \
        macro_segments[i].seg_len = segs[i].seg_len;                  \
        memcpy(buff->addr,                                      \
               segs[i].seg_addr.pval,                           \
               segs[i].seg_len);                                \
    }                                                           \
                                                                \
} while(0)


#define MCA_PML_OB1_RECV_FRAG_RETURN(frag)                           \
do {                                                            \
    size_t i;                                                   \
                                                                \
    /* return buffers */                                        \
    for(i=0; i<frag->num_segments; i++) {                       \
        OMPI_FREE_LIST_RETURN(&mca_pml_ob1.buffers,             \
           (ompi_list_item_t*)frag->buffers[i]);                \
    }                                                           \
    frag->num_segments = 0;                                     \
                                                                \
    /* return recv_frag */                                       \
    OMPI_FREE_LIST_RETURN(&mca_pml_ob1.recv_frags,               \
        (ompi_list_item_t*)frag);                               \
} while(0)


/**
 *  Callback from BMI on receipt of a recv_frag.
 */

OMPI_DECLSPEC void mca_pml_ob1_recv_frag_callback(
    mca_bmi_base_module_t* bmi,
    mca_bmi_base_tag_t tag,
    mca_bmi_base_descriptor_t* descriptor,
    void* cbdata
);
                                                                                                               
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
OMPI_DECLSPEC int mca_pml_ob1_recv_frag_match(
    mca_bmi_base_module_t* bmi,
    mca_pml_ob1_match_hdr_t *hdr,
    mca_bmi_base_segment_t* segments,
    size_t num_segments);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

