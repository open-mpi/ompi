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
                                                                                                                                                 
#ifndef MCA_PML_TEG_RECVFRAG_H
#define MCA_PML_TEG_RECVFRAG_H

#include "mca/ptl/ptl.h"
#include "mca/pml/base/pml_base_recvreq.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"

/**
 * Called by the PTL to match attempt a match for new fragments.
 *
 * @param ptl (IN)      The PTL pointer
 * @param frag (IN)     Receive fragment descriptor.
 * @param header (IN)   Header corresponding to the receive fragment.
 * @return              OMPI_SUCCESS or error status on failure.
 */
bool mca_pml_teg_recv_frag_match( 
    mca_ptl_base_module_t* ptl,
    mca_ptl_base_recv_frag_t* frag,
    mca_ptl_base_match_header_t* header
);

#endif

