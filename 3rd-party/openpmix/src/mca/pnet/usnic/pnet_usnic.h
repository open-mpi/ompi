/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_PNET_USNIC_H
#define PMIX_PNET_USNIC_H

#include "src/include/pmix_config.h"

#include "src/mca/pnet/pnet.h"

BEGIN_C_DECLS

typedef struct {
    pmix_pnet_base_component_t super;
    char *incparms;
    char *excparms;
    char **include;
    char **exclude;
    char **tcp_static_ports;
    char **tcp6_static_ports;
} pmix_pnet_usnic_component_t;

/* the component must be visible data for the linker to find it */
PMIX_EXPORT extern pmix_pnet_usnic_component_t pmix_mca_pnet_usnic_component;
extern pmix_pnet_module_t pmix_pnet_usnic_module;

/* define a key for any blob we need to send in a launch msg */
#define PMIX_PNET_USNIC_BLOB "pmix.pnet.usnic.blob"

/* define an inventory key */
#define PMIX_PNET_USNIC_INVENTORY_KEY "pmix.pnet.usnic.inventory"

END_C_DECLS

#endif
