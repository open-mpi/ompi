/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTE_RMCAST_SPREAD_H
#define ORTE_RMCAST_SPREAD_H

#include "orte_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/mca/rmcast/rmcast.h"

BEGIN_C_DECLS

#define ORTE_RMCAST_SPREAD_MAX_MSG_SIZE  1500

typedef struct {
    orte_rmcast_base_component_t super;
    int max_msg_size;
} orte_rmcast_spread_component_t;


/*
 * Module open / close
 */
int orte_rmcast_spread_component_open(void);
int orte_rmcast_spread_component_close(void);
int orte_rmcast_spread_component_query(mca_base_module_t **module, int *priority);


ORTE_MODULE_DECLSPEC extern orte_rmcast_spread_component_t mca_rmcast_spread_component;
ORTE_DECLSPEC extern orte_rmcast_module_t orte_rmcast_spread_module;

END_C_DECLS

#endif /* ORTE_RMCAST_SPREAD_H */
