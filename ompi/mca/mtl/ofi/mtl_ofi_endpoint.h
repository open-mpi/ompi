/*
 * Copyright (c) 2013-2016 Intel, Inc. All rights reserved
 * Copyright (c) 2020      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#ifndef OMPI_MTL_OFI_ENDPOINT_H
#define OMPI_MTL_OFI_ENDPOINT_H

#include "ompi/mca/pml/pml.h"

BEGIN_C_DECLS

OBJ_CLASS_DECLARATION(mca_mtl_ofi_endpoint_t);

/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_mtl_ofi_endpoint_t is associated with each process
 * and MTL pair at startup. However, connections to the endpoint
 * are established dynamically on an as-needed basis:
 */

struct mca_mtl_ofi_endpoint_t {
    opal_list_item_t super;

    /** MTL instance that created this connection */
    struct mca_mtl_ofi_module_t *mtl_ofi_module;

    /** The peer's fi_addr */
    fi_addr_t peer_fiaddr;
};

typedef struct mca_mtl_ofi_endpoint_t  mca_mtl_ofi_endpoint_t;

/**
 * Look up a peer's endpoint, wiring it as a last resort.
 *
 * The PML wires a peer before handing an operation down, so this is
 * normally a plain lookup. Acknowledging a synchronous send is the one
 * path that names a peer the PML never saw; wiring can fail there,
 * usually because the connection info has not arrived, so every caller
 * must be ready for a NULL.
 */
static inline mca_mtl_ofi_endpoint_t *
ompi_mtl_ofi_get_endpoint(struct mca_mtl_base_module_t* mtl,
                          ompi_proc_t *ompi_proc)
{
    if (OPAL_UNLIKELY(NULL == ompi_proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL])) {
        if (OPAL_UNLIKELY(OMPI_SUCCESS != MCA_PML_CALL(add_procs(&ompi_proc, 1)))) {
            return NULL;
        }
    }

    return ompi_proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL];
}

END_C_DECLS
#endif
