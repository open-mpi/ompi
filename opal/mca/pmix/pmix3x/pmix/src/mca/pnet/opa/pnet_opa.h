/*
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_PNET_OPA_H
#define PMIX_PNET_OPA_H

#include <src/include/pmix_config.h>


#include "src/mca/pnet/pnet.h"

BEGIN_C_DECLS

typedef struct {
    pmix_pnet_base_component_t super;
    char *incparms;
    char *excparms;
    char **include;
    char **exclude;
} pmix_pnet_opa_component_t;

/* the component must be visible data for the linker to find it */
PMIX_EXPORT extern pmix_pnet_opa_component_t mca_pnet_opa_component;
extern pmix_pnet_module_t pmix_opa_module;

/* define a key for any blob we need to send in a launch msg */
#define PMIX_PNET_OPA_BLOB  "pmix.pnet.opa.blob"

/* define an inventory key */
#define PMIX_OPA_INVENTORY_KEY  "pmix.opa.inventory"

END_C_DECLS

#endif
