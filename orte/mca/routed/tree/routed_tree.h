/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.
 *                         All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_ROUTED_TREE_ROUTED_TREE_H
#define MCA_ROUTED_TREE_ROUTED_TREE_H

#include "orte_config.h"
#include "orte/types.h"

#include "opal/class/opal_list.h"
#include "opal/class/opal_hash_table.h"
#include "opal/threads/condition.h"

#include "opal/dss/dss_types.h"

#include "orte/mca/routed/routed.h"

BEGIN_C_DECLS

struct orte_routed_tree_module_t {
    orte_routed_module_t     super;
    opal_hash_table_t        peer_list;
    opal_hash_table_t        vpid_wildcard_list;
    orte_process_name_t      wildcard_route;
    opal_condition_t         cond;
    opal_mutex_t             lock;
};
typedef struct orte_routed_tree_module_t orte_routed_tree_module_t;


ORTE_MODULE_DECLSPEC extern orte_routed_component_t mca_routed_tree_component;

extern orte_routed_tree_module_t orte_routed_tree_module;

int orte_routed_tree_module_init(void);

int orte_routed_tree_finalize(void);

int orte_routed_tree_update_route(orte_process_name_t *target,
                                   orte_process_name_t *route);

orte_process_name_t orte_routed_tree_get_route(orte_process_name_t *target);

int orte_routed_tree_init_routes(orte_jobid_t job, opal_buffer_t *ndat);

END_C_DECLS

#endif
