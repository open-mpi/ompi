/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 *
 */
#ifndef ORTE_FDDP_TREND_H
#define ORTE_FDDP_TREND_H

#include "orte_config.h"

#include "orte/mca/fddp/fddp.h"

BEGIN_C_DECLS

struct orte_fddp_trend_component_t {
    orte_fddp_base_component_t super;
    int window_size;
};
typedef struct orte_fddp_trend_component_t orte_fddp_trend_component_t;

ORTE_MODULE_DECLSPEC extern orte_fddp_trend_component_t mca_fddp_trend_component;
extern orte_fddp_base_module_t orte_fddp_trend_module;


END_C_DECLS

#endif
