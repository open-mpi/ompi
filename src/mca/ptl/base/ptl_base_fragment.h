/*
 * $HEADER$
 */
/** 
 * @file
 */
#ifndef MCA_PML_BASE_FRAGMENT_H
#define MCA_PML_BASE_FRAGMENT_H

#include "class/ompi_list.h"
#include "mca/ptl/ptl.h"
#include "datatype/datatype.h"
#include "mca/ptl/base/ptl_base_header.h"

extern ompi_class_t mca_ptl_base_frag_t_class;

/**
 * Base type for fragment descriptors. 
 */
struct mca_ptl_base_frag_t {
    ompi_list_item_t super; /**< allow the fragment to be placed on a list */
    mca_ptl_base_header_t frag_header; /**< header used for fragment matching */
    struct mca_ptl_base_module_t* frag_owner; /**< PTL that allocated this fragment */
    struct mca_ptl_base_peer_t* frag_peer; /**< PTL specific addressing info */
    void  *frag_addr; /**< pointer into request buffer at fragment offset */
    size_t frag_size; /**< number of bytes available in request buffer */
    ompi_convertor_t frag_convertor; /**< datatype convertor for fragment packing/unpacking */
};
typedef struct mca_ptl_base_frag_t mca_ptl_base_frag_t;


#endif

