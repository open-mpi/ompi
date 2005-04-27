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

#include "ompi_config.h"
#include "portals_config.h"

#include "include/constants.h"
#include "util/output.h"

#include "ptl_portals.h"
#include "ptl_portals_compat.h"


int
mca_ptl_portals_init(mca_ptl_portals_component_t *comp)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}

int
mca_ptl_portals_add_procs_compat(struct mca_ptl_base_module_t* ptl_base,
                                 size_t nprocs, struct ompi_proc_t **procs,
                                 struct mca_ptl_base_peer_t** peers,
                                 ompi_bitmap_t* reachable)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}
