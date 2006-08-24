/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
                                                                                                                                                 
#ifndef MCA_PML_OB1_RECVFRAG_H
#define MCA_PML_OB1_RECVFRAG_H

#include "ompi/mca/btl/btl.h"
#include "ompi/mca/bml/bml.h" 
#include "pml_ob1_hdr.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct mca_pml_ob1_buffer_t {
    ompi_free_list_item_t super;
    size_t len;
    unsigned char addr[1];
};
typedef struct mca_pml_ob1_buffer_t mca_pml_ob1_buffer_t;

OBJ_CLASS_DECLARATION(mca_pml_ob1_buffer_t);


struct mca_pml_ob1_recv_frag_t {
    ompi_free_list_item_t super;
    mca_pml_ob1_hdr_t hdr;
    struct mca_pml_ob1_recv_request_t* request;
    size_t num_segments;
    mca_btl_base_module_t* btl;
    mca_btl_base_segment_t segments[MCA_BTL_DES_MAX_SEGMENTS];
    mca_pml_ob1_buffer_t* buffers[MCA_BTL_DES_MAX_SEGMENTS];
};
typedef struct mca_pml_ob1_recv_frag_t mca_pml_ob1_recv_frag_t;

OBJ_CLASS_DECLARATION(mca_pml_ob1_recv_frag_t);


#define MCA_PML_OB1_RECV_FRAG_ALLOC(frag,rc)                    \
do {                                                            \
    ompi_free_list_item_t* item;                                \
    OMPI_FREE_LIST_WAIT(&mca_pml_ob1.recv_frags, item, rc);     \
    frag = (mca_pml_ob1_recv_frag_t*)item;                      \
} while(0)


#define MCA_PML_OB1_RECV_FRAG_INIT(frag, hdr, segs, cnt, btl )  \
do {                                                            \
    size_t i;                                                   \
    mca_btl_base_segment_t* macro_segments = frag->segments;    \
    mca_pml_ob1_buffer_t** buffers = frag->buffers;             \
                                                                \
    /* init recv_frag */                                        \
    frag->btl = btl;                                            \
    frag->hdr = *(mca_pml_ob1_hdr_t*)hdr;                       \
    frag->num_segments = cnt;                                   \
    /* copy over data */                                        \
    for(i=0; i<cnt; i++) {                                      \
        ompi_free_list_item_t* item;                            \
        mca_pml_ob1_buffer_t* buff;                             \
        OMPI_FREE_LIST_WAIT(&mca_pml_ob1.buffers, item, rc);    \
        buff = (mca_pml_ob1_buffer_t*)item;                     \
        buffers[i] = buff;                                      \
        macro_segments[i].seg_addr.pval = buff->addr;           \
        macro_segments[i].seg_len = segs[i].seg_len;            \
        memcpy(buff->addr,                                      \
               segs[i].seg_addr.pval,                           \
               segs[i].seg_len);                                \
    }                                                           \
                                                                \
} while(0)


#define MCA_PML_OB1_RECV_FRAG_RETURN(frag)                      \
do {                                                            \
    size_t i;                                                   \
                                                                \
    /* return buffers */                                        \
    for(i=0; i<frag->num_segments; i++) {                       \
        OMPI_FREE_LIST_RETURN(&mca_pml_ob1.buffers,             \
           (ompi_free_list_item_t*)frag->buffers[i]);           \
    }                                                           \
    frag->num_segments = 0;                                     \
                                                                \
    /* return recv_frag */                                      \
    OMPI_FREE_LIST_RETURN(&mca_pml_ob1.recv_frags,              \
        (ompi_free_list_item_t*)frag);                          \
} while(0)


/**
 *  Callback from BTL on receipt of a recv_frag.
 */

OMPI_DECLSPEC void mca_pml_ob1_recv_frag_callback(
                                                  mca_btl_base_module_t *btl, 
                                                  mca_btl_base_tag_t tag,
                                                  mca_btl_base_descriptor_t* descriptor,
                                                  void* cbdata
                                                  );
                                                                                                               
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

