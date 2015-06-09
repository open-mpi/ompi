/*
 * Copyright (c) 2013-2015 Intel, Inc. All rights reserved
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "mtl_ofi.h"

/**
 * Initialize state of the endpoint instance.
 */

static void mca_mtl_ofi_endpoint_construct(mca_mtl_ofi_endpoint_t *endpoint)
{
    endpoint->mtl_ofi_module = NULL;
}

/**
 * Destroy an endpoint
 */
static void mca_mtl_ofi_endpoint_destruct(mca_mtl_ofi_endpoint_t *endpoint)
{
}


OBJ_CLASS_INSTANCE(
    mca_mtl_ofi_endpoint_t,
    opal_list_item_t,
    mca_mtl_ofi_endpoint_construct,
    mca_mtl_ofi_endpoint_destruct
);
