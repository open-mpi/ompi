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
 * Copyright (c) 2014      Bull SAS.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_BTL_PORTALS4_ENDPOINT_H
#define OPAL_BTL_PORTALS4_ENDPOINT_H

#include "btl_portals4.h"

BEGIN_C_DECLS

/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_btl_base_endpoint_t is associated w/ each process
 * and BTL pair at startup. However, connections to the endpoint
 * are established dynamically on an as-needed basis:
 */
struct mca_btl_base_endpoint_t {
    ptl_process_t ptl_proc;
};
typedef struct mca_btl_base_endpoint_t mca_btl_base_endpoint_t;

END_C_DECLS

#endif /* MCA_BTL_PORTALS4_ENDPOINT_H */
