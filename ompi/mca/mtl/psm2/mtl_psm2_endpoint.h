/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      QLogic Corporation. All rights reserved.
 * Copyright (c) 2015      Intel, Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_MTL_PSM2_ENDPOINT_H
#define MCA_MTL_PSM2_ENDPOINT_H

#include "opal/class/opal_list.h"
#include "opal/mca/event/event.h"
#include "ompi/mca/mtl/mtl.h"
#include "mtl_psm2.h"

#include "psm2.h"

BEGIN_C_DECLS

OBJ_CLASS_DECLARATION(mca_mtl_psm2_endpoint_t);

/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_mtl_psm2_endpoint_t is associated w/ each process
 * and MTL pair at startup. However, connections to the endpoint
 * are established dynamically on an as-needed basis:
 */

struct mca_mtl_psm2_endpoint_t {
    opal_list_item_t super;

    struct mca_mtl_psm2_module_t* mtl_psm2_module;
    /**< MTL instance that created this connection */

    psm2_epid_t	    peer_epid;
    /**< The unique epid for the opened port */

    psm2_epaddr_t    peer_addr;
    /**< The connected endpoint handle*/
};

typedef struct mca_mtl_psm2_endpoint_t  mca_mtl_psm2_endpoint_t;
OBJ_CLASS_DECLARATION(mca_mtl_psm2_endpoint);

static inline mca_mtl_psm2_endpoint_t *ompi_mtl_psm2_get_endpoint (struct mca_mtl_base_module_t* mtl, ompi_proc_t *ompi_proc)
{
    if (OPAL_UNLIKELY(NULL == ompi_proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL])) {
	ompi_mtl_psm2_add_procs (mtl, 1, &ompi_proc);
    }

    return ompi_proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL];
}

END_C_DECLS
#endif
