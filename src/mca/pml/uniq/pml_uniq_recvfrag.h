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
                                                                                                                                                 
#ifndef MCA_PML_UNIQ_RECVFRAG_H
#define MCA_PML_UNIQ_RECVFRAG_H

#include "mca/ptl/ptl.h"
#include "mca/pml/base/pml_base_recvreq.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "datatype/convertor.h"

/**
 * Called by the PTL to match attempt a match for new fragments.
 *
 * @param ptl (IN)      The PTL pointer
 * @param frag (IN)     Receive fragment descriptor.
 * @param header (IN)   Header corresponding to the receive fragment.
 * @return              OMPI_SUCCESS or error status on failure.
 */
bool mca_pml_uniq_recv_frag_match( 
    mca_ptl_base_module_t* ptl,
    mca_ptl_base_recv_frag_t* frag,
    mca_ptl_base_match_header_t* header
);

#define MCA_PML_UNIQ_RECV_MATCHED( ptl, frag )                                                  \
do {                                                                                            \
    mca_pml_base_recv_request_t* _request = (mca_pml_base_recv_request_t*)(frag)->frag_request; \
    /* Now that we have the sender we can create the convertor. Additionally, we know */        \
    /* that the required convertor should start at the position zero as we just match */        \
    /* the first fragment.                                                            */        \
    if( 0 != (_request)->req_bytes_packed ) {                                                   \
        (_request)->req_base.req_proc = ompi_comm_peer_lookup(                                  \
                (_request)->req_base.req_comm,                                                  \
                frag->frag_base.frag_header.hdr_match.hdr_src);                                 \
        ompi_convertor_copy_and_prepare_for_recv(                                               \
                                (_request)->req_base.req_proc->proc_convertor,                  \
                                (_request)->req_base.req_datatype,                              \
                                (_request)->req_base.req_count,                                 \
                                (_request)->req_base.req_addr,                                  \
                                &((_request)->req_convertor) );                                 \
    }                                                                                           \
    ptl->ptl_matched( (ptl), (frag) ); /* notify ptl of match */                                \
} while (0)
#endif

