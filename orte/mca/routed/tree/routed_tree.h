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

#include "orte/mca/routed/routed.h"

#include "opal/class/opal_list.h"

BEGIN_C_DECLS

struct orte_routed_tree_entry_t {
    opal_list_item_t    super;
    orte_process_name_t target;
    orte_process_name_t route;
};
typedef struct orte_routed_tree_entry_t orte_routed_tree_entry_t;
OBJ_CLASS_DECLARATION(orte_routed_tree_entry_t);


struct orte_routed_tree_module_t {
    orte_routed_module_t     super;
    opal_list_t              peer_list;
    opal_list_t              vpid_wildcard_list;
    opal_list_t              jobid_wildcard_list;
    orte_routed_tree_entry_t full_wildcard_entry;
};
typedef struct orte_routed_tree_module_t orte_routed_tree_module_t;


ORTE_MODULE_DECLSPEC extern orte_routed_component_t mca_routed_tree_component;

extern orte_routed_tree_module_t orte_routed_tree_module;

int orte_routed_tree_finalize(void);

int orte_routed_tree_update_route(orte_process_name_t *target,
                                   orte_process_name_t *route);

orte_process_name_t orte_routed_tree_get_route(orte_process_name_t *target);


END_C_DECLS

#endif
