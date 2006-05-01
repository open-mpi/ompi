/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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

#include "ompi_config.h"
#include "ompi/request/request.h"
#include "pml_portals.h"

int
ompi_pml_portals_iprobe(int src, int tag,
                        struct ompi_communicator_t *comm,
                        int *matched, ompi_status_public_t * status)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_pml_portals_probe(int src, int tag,
                       struct ompi_communicator_t *comm,
                       ompi_status_public_t * status)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}
