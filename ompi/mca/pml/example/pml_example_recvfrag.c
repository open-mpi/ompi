/*
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "pml_example.h"
#include "pml_example_recvfrag.h"

bool mca_pml_example_recv_frag_match( mca_ptl_base_module_t* ptl,
                                  mca_ptl_base_recv_frag_t* frag,
                                  mca_ptl_base_match_header_t* header )
{
    return false;
}
