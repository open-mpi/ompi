/*
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PML_EXAMPLE_RECVFRAG_H_HAS_BEEN_INCLUDED
#define PML_EXAMPLE_RECVFRAG_H_HAS_BEEN_INCLUDED

#include "mca/ptl/base/ptl_base_recvfrag.h"

bool mca_pml_example_recv_frag_match( mca_ptl_base_module_t* ptl,
                                  mca_ptl_base_recv_frag_t* frag,
                                  mca_ptl_base_match_header_t* header );

#endif  /* PML_EXAMPLE_RECVFRAG_H_HAS_BEEN_INCLUDED */
