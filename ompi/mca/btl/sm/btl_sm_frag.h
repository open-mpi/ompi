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
 * @file
 */
#ifndef MCA_BTL_SM_SEND_FRAG_H
#define MCA_BTL_SM_SEND_FRAG_H

#include <sys/types.h>
#include "ompi_config.h"
#include "ompi/class/ompi_free_list.h"
#include "btl_sm.h"


typedef enum {
    MCA_BTL_SM_FRAG_SEND,
    MCA_BTL_SM_FRAG_PUT,
    MCA_BTL_SM_FRAG_GET,
    MCA_BTL_SM_FRAG_ACK
} mca_btl_sm_frag_type_t;


/**
 * shared memory send fragment derived type.
 */
struct mca_btl_sm_frag_t {
    mca_btl_base_descriptor_t base;
    mca_btl_base_segment_t segment;
    struct mca_btl_base_endpoint_t *endpoint;
    mca_btl_sm_frag_type_t type;
    mca_btl_base_tag_t tag;
    size_t size;
    int rc;
    ompi_free_list_t* my_list;
};
typedef struct mca_btl_sm_frag_t mca_btl_sm_frag_t;
typedef struct mca_btl_sm_frag_t mca_btl_sm_frag1_t;
typedef struct mca_btl_sm_frag_t mca_btl_sm_frag2_t;

OBJ_CLASS_DECLARATION(mca_btl_sm_frag_t);
OBJ_CLASS_DECLARATION(mca_btl_sm_frag1_t);
OBJ_CLASS_DECLARATION(mca_btl_sm_frag2_t);

#define MCA_BTL_SM_FRAG_ALLOC1(frag, rc)                                \
{                                                                       \
    ompi_free_list_item_t* item;                                        \
    OMPI_FREE_LIST_GET(&mca_btl_sm_component.sm_frags1, item, rc);      \
    frag = (mca_btl_sm_frag_t*)item;                                    \
}

#define MCA_BTL_SM_FRAG_ALLOC2(frag, rc)                                \
{                                                                       \
    ompi_free_list_item_t* item;                                        \
    OMPI_FREE_LIST_GET(&mca_btl_sm_component.sm_frags2, item, rc);      \
    frag = (mca_btl_sm_frag_t*)item;                                    \
}

#define MCA_BTL_SM_FRAG_RETURN(frag)                                      \
{                                                                         \
    OMPI_FREE_LIST_RETURN(frag->my_list, (ompi_free_list_item_t*)(frag)); \
}
#endif
