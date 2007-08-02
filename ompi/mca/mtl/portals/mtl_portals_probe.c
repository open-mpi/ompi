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
#include "mtl_portals.h"


int
ompi_mtl_portals_iprobe(struct mca_mtl_base_module_t* mtl,
                        struct ompi_communicator_t *comm,
                        int src,
                        int tag,
                        int *flag,
                        struct ompi_status_public_t *status)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}
