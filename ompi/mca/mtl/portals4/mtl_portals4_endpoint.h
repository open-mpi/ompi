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
 * Copyright (c) 2010-2020 Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_MTL_PORTALS_ENDPOINT_H
#define OMPI_MTL_PORTALS_ENDPOINT_H

#include "ompi/mca/pml/pml.h"
#include "ompi/mca/mtl/portals4/mtl_portals4.h"

struct mca_mtl_base_endpoint_t {
    ptl_process_t ptl_proc;
};
typedef struct mca_mtl_base_endpoint_t mca_mtl_base_endpoint_t;

static inline mca_mtl_base_endpoint_t *
ompi_mtl_portals4_get_endpoint (struct mca_mtl_base_module_t* mtl, ompi_proc_t *ompi_proc)
{
    if (OPAL_UNLIKELY(NULL == ompi_proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4])) {
        int rc;
        if (OPAL_UNLIKELY(OMPI_SUCCESS != (rc = MCA_PML_CALL(add_procs(&ompi_proc, 1))))) {
            ompi_rte_abort(rc,"ompi_mtl_portals4_get_endpoint(): pml->add_procs() failed.  Aborting.\n");
        }
    }

    return ompi_proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4];
}

#endif
