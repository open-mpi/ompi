/*
 * Copyright (c) 2008 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2018      Los Alamos National Security, LLC. All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "btl_iwarp.h"
#include "btl_iwarp_endpoint.h"
#include "connect/connect.h"

static void empty_component_register(void);
static int empty_component_init(void);
static int empty_component_query(mca_btl_iwarp_module_t *btl,
                                 opal_btl_iwarp_connect_base_module_t **cpc);

opal_btl_iwarp_connect_base_component_t opal_btl_iwarp_connect_empty = {
    "empty",
    empty_component_register,
    empty_component_init,
    empty_component_query,
    NULL
};

static void empty_component_register(void)
{
    /* Nothing to do */
}

static int empty_component_init(void)
{
    /* Never let this CPC run */
    return OPAL_ERR_NOT_SUPPORTED;
}

static int empty_component_query(mca_btl_iwarp_module_t *btl,
                                 opal_btl_iwarp_connect_base_module_t **cpc)
{
    /* Never let this CPC run */
    return OPAL_ERR_NOT_SUPPORTED;
}
