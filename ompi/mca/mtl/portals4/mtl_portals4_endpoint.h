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
 * Copyright (c) 2010      Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_MTL_PORTALS_ENDPOINT_H
#define OMPI_MTL_PORTALS_ENDPOINT_H

#include "ompi/mca/mtl/portals4/mtl_portals4.h"

struct mca_mtl_base_endpoint_t {
    ptl_process_t ptl_proc;
};
typedef struct mca_mtl_base_endpoint_t mca_mtl_base_endpoint_t;

static inline mca_mtl_base_endpoint_t *
ompi_mtl_portals4_get_endpoint (struct mca_mtl_base_module_t* mtl, ompi_proc_t *ompi_proc)
{
    if (OPAL_UNLIKELY(NULL == ompi_proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4])) {
        ompi_mtl_portals4_add_procs (mtl, 1, &ompi_proc);
    }

    return ompi_proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4];
}

#endif
