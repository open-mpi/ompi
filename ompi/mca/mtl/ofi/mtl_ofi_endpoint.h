/*
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_MTL_OFI_ENDPOINT_H
#define OMPI_MTL_OFI_ENDPOINT_H

#include "opal/class/opal_list.h"
#include "opal/mca/event/event.h"
#include "ompi/mca/mtl/mtl.h"
#include "mtl_ofi.h"

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
OBJ_CLASS_DECLARATION(mca_mtl_ofi_endpoint);

END_C_DECLS
#endif
