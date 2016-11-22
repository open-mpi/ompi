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
#include "pml_example_recvreq.h"

/*
 * Update the recv request status to reflect the number of bytes
 * received and actually delivered to the application.
 */

void mca_pml_example_recv_request_progress( struct mca_ptl_base_module_t* ptl,
                                        mca_pml_base_recv_request_t* req,
                                        size_t bytes_received,
                                        size_t bytes_delivered )
{
}
