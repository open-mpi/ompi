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
 * @file
 */
#ifndef MCA_PTL_BASE_FRAGMENT_H
#define MCA_PTL_BASE_FRAGMENT_H

#include "opal/class/opal_list.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "datatype/datatype.h"
#include "datatype/convertor.h"
#include "mca/ptl/base/ptl_base_header.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * Type of fragment
 */

typedef enum {
    MCA_PTL_FRAGMENT_SEND,
    MCA_PTL_FRAGMENT_RECV
} mca_ptl_base_frag_type_t;

/**
 * Base type for fragment descriptors. 
 */
struct mca_ptl_base_frag_t {
    opal_list_item_t super; /**< allow the fragment to be placed on a list */
    mca_ptl_base_header_t frag_header; /**< header used for fragment matching */
    struct mca_ptl_base_module_t* frag_owner; /**< PTL that allocated this fragment */
    struct mca_ptl_base_peer_t* frag_peer; /**< PTL specific addressing info */
    void  *frag_addr; /**< pointer into request buffer at fragment offset */
    size_t frag_size; /**< number of bytes available in request buffer */
    mca_ptl_base_frag_type_t frag_type; /**< fragment derived type */
    ompi_convertor_t frag_convertor; /**< datatype convertor for fragment packing/unpacking */
};
typedef struct mca_ptl_base_frag_t mca_ptl_base_frag_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_ptl_base_frag_t);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

