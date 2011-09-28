/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2010 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/communicator/communicator.h"
#include "mtl_portals4.h"
#include "mtl_portals4_request.h"

int
ompi_mtl_portals4_iprobe(struct mca_mtl_base_module_t* mtl,
                         struct ompi_communicator_t *comm,
                         int src,
                         int tag,
                         int *flag,
                         struct ompi_status_public_t *status)
{
    /* BWB: FIX ME: implement */
    opal_output(ompi_mtl_base_output,
                "%s:%d: iprobe failed: not implemented\n",
                __FILE__, __LINE__);
    return OMPI_ERR_NOT_IMPLEMENTED;
}
