/*
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PML_EXAMPLE_RECVREQ_H_HAS_BEEN_INCLUDED
#define PML_EXAMPLE_RECVREQ_H_HAS_BEEN_INCLUDED

#include "mca/pml/base/pml_base_recvreq.h"

void mca_pml_example_recv_request_progress( struct mca_ptl_base_module_t* ptl,
                                        mca_pml_base_recv_request_t* req,
                                        size_t bytes_received,
                                        size_t bytes_delivered );

#endif  /* PML_EXAMPLE_RECVREQ_H_HAS_BEEN_INCLUDED */
