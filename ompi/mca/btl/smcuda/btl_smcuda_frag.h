/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_BTL_SMCUDA_SEND_FRAG_H
#define MCA_BTL_SMCUDA_SEND_FRAG_H

#include "ompi_config.h"
#include "btl_smcuda.h"


#define MCA_BTL_SMCUDA_FRAG_TYPE_MASK ((uintptr_t)0x3)
#define MCA_BTL_SMCUDA_FRAG_SEND ((uintptr_t)0x0)
#define MCA_BTL_SMCUDA_FRAG_ACK ((uintptr_t)0x1)
#define MCA_BTL_SMCUDA_FRAG_PUT ((uintptr_t)0x2)
#define MCA_BTL_SMCUDA_FRAG_GET ((uintptr_t)0x3)

#define MCA_BTL_SMCUDA_FRAG_STATUS_MASK ((uintptr_t)0x4)

struct mca_btl_smcuda_frag_t;

struct mca_btl_smcuda_hdr_t {
    struct mca_btl_smcuda_frag_t *frag;
    size_t len;
    int my_smp_rank;
    mca_btl_base_tag_t tag;
};
typedef struct mca_btl_smcuda_hdr_t mca_btl_smcuda_hdr_t;

struct mca_btl_smcuda_segment_t {
    mca_btl_base_segment_t base;
#if OPAL_CUDA_SUPPORT
    uint8_t key[128]; /* 64 bytes for CUDA mem handle, 64 bytes for CUDA event handle */
    /** Address of the entire memory handle */
    ompi_ptr_t memh_seg_addr;        
     /** Length in bytes of entire memory handle */
    uint32_t memh_seg_len;           
#endif /* OPAL_CUDA_SUPPORT */
};
typedef struct mca_btl_smcuda_segment_t mca_btl_smcuda_segment_t;

/**
 * shared memory send fragment derived type.
 */
struct mca_btl_smcuda_frag_t {
    mca_btl_base_descriptor_t base;
    mca_btl_smcuda_segment_t segment;
    struct mca_btl_base_endpoint_t *endpoint;
#if OPAL_CUDA_SUPPORT
    struct mca_mpool_base_registration_t *registration;
#endif /* OPAL_CUDA_SUPPORT */
    size_t size;
    /* pointer written to the FIFO, this is the base of the shared memory region */
    mca_btl_smcuda_hdr_t *hdr;
    ompi_free_list_t* my_list;
};
typedef struct mca_btl_smcuda_frag_t mca_btl_smcuda_frag_t;
typedef struct mca_btl_smcuda_frag_t mca_btl_smcuda_frag1_t;
typedef struct mca_btl_smcuda_frag_t mca_btl_smcuda_frag2_t;
typedef struct mca_btl_smcuda_frag_t mca_btl_smcuda_user_t;


OBJ_CLASS_DECLARATION(mca_btl_smcuda_frag_t);
OBJ_CLASS_DECLARATION(mca_btl_smcuda_frag1_t);
OBJ_CLASS_DECLARATION(mca_btl_smcuda_frag2_t);
OBJ_CLASS_DECLARATION(mca_btl_smcuda_user_t);

#define MCA_BTL_SMCUDA_FRAG_ALLOC_EAGER(frag)                               \
{                                                                       \
    ompi_free_list_item_t* item;                                        \
    OMPI_FREE_LIST_GET_MT(&mca_btl_smcuda_component.sm_frags_eager, item);     \
    frag = (mca_btl_smcuda_frag_t*)item;                                    \
}

#define MCA_BTL_SMCUDA_FRAG_ALLOC_MAX(frag)                                 \
{                                                                       \
    ompi_free_list_item_t* item;                                        \
    OMPI_FREE_LIST_GET_MT(&mca_btl_smcuda_component.sm_frags_max, item);       \
    frag = (mca_btl_smcuda_frag_t*)item;                                    \
}

#define MCA_BTL_SMCUDA_FRAG_ALLOC_USER(frag)                                \
{                                                                       \
	ompi_free_list_item_t* item;                                        \
	OMPI_FREE_LIST_GET_MT(&mca_btl_smcuda_component.sm_frags_user, item);      \
	frag = (mca_btl_smcuda_frag_t*)item;                                    \
}


#define MCA_BTL_SMCUDA_FRAG_RETURN(frag)                                      \
{                                                                         \
    OMPI_FREE_LIST_RETURN_MT(frag->my_list, (ompi_free_list_item_t*)(frag)); \
}
#endif
