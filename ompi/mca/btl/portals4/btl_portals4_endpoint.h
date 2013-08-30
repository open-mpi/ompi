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

#ifndef OMPI_BTL_PORTALS4_ENDPOINT_H
#define OMPI_BTL_PORTALS4_ENDPOINT_H

#include "btl_portals4.h"

BEGIN_C_DECLS

/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_btl_base_endpoint_t is associated w/ each process
 * and BTL pair at startup. However, connections to the endpoint
 * are established dynamically on an as-needed basis:
 *
 * The MTL, OSC, and COLL components expect the ptl_process_t to be
 * hanging off the PORTALS4 tag in the proc_endpoints.  That's not
 * entirely convenient for the BTL, since we get an endpoint, not an
 * ompi_proc_t.  So we store the ptl_process_t in both places.  Since
 * the btl_base_endpoint_t is just a ptl_process_t, we use the same
 * storage for both.  During tear-down, it's entirely possible that
 * the MTL is going to free the PORTALS4 memory, so we need to be
 * careful during del_procs.
 */
struct mca_btl_base_endpoint_t {
    ptl_process_t ptl_proc;
};
typedef struct mca_btl_base_endpoint_t mca_btl_base_endpoint_t;

END_C_DECLS

#endif /* MCA_BTL_PORTALS4_ENDPOINT_H */
