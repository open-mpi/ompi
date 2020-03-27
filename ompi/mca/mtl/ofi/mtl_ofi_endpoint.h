/*
 * Copyright (c) 2013-2016 Intel, Inc. All rights reserved
 * Copyright (c) 2020      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
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

static inline mca_mtl_ofi_endpoint_t *
ompi_mtl_ofi_get_endpoint(struct mca_mtl_base_module_t* mtl,
                          ompi_proc_t *ompi_proc)
{
    if (OPAL_UNLIKELY(NULL == ompi_proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL])) {
        if (OPAL_UNLIKELY(OMPI_SUCCESS != MCA_PML_CALL(add_procs(&ompi_proc, 1)))) {
            /* Fatal error. exit() out */
            opal_output(0, "%s:%d: *** The Open MPI OFI MTL is aborting the MPI job (via exit(3)).\n",
                           __FILE__, __LINE__);
            fflush(stderr);
            exit(1);
        }
    }

    return ompi_proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL];
}

END_C_DECLS
#endif
