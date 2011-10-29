/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTE_RMAPS_PPR_H
#define ORTE_RMAPS_PPR_H

#include "orte_config.h"

#include "opal/mca/hwloc/hwloc.h"

#include "orte/mca/rmaps/rmaps.h"

BEGIN_C_DECLS

struct orte_rmaps_ppr_component_t {
    orte_rmaps_base_component_t super;
    char *given_ppr;
    bool selected;
    bool pruning_reqd;
    int ppr[OPAL_HWLOC_HWTHREAD_LEVEL];
    opal_hwloc_level_t start;
};
typedef struct orte_rmaps_ppr_component_t orte_rmaps_ppr_component_t;

ORTE_MODULE_DECLSPEC extern orte_rmaps_ppr_component_t mca_rmaps_ppr_component;
extern orte_rmaps_base_module_t orte_rmaps_ppr_module;


END_C_DECLS

#endif
