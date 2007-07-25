/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.
 *                         All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include "routed_tree.h"

#include "opal/util/output.h"
#include "orte/orte_constants.h"
#include "orte/mca/routed/base/base.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/routed/routed.h"


int
orte_routed_tree_update_route(orte_process_name_t *target,
                               orte_process_name_t *route)
{ 
    if (target->jobid == ORTE_JOBID_INVALID ||
        target->vpid == ORTE_VPID_INVALID) {
        return ORTE_ERR_BAD_PARAM;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "routed_tree_update: [%s] --> [%s]",
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(route)));


    /* exact match */
    if (target->jobid != ORTE_JOBID_WILDCARD &&
        target->vpid != ORTE_VPID_WILDCARD) {
        opal_list_item_t *item;
        orte_routed_tree_entry_t *entry;

        for (item = opal_list_get_first(&orte_routed_tree_module.peer_list) ;
             item != opal_list_get_end(&orte_routed_tree_module.peer_list) ;
             item = opal_list_get_next(item)) {
            entry = (orte_routed_tree_entry_t*) item;

            if (0 == orte_ns.compare_fields(ORTE_NS_CMP_ALL, 
                                            target, &entry->target)) {
                entry->route = *route;
                return ORTE_SUCCESS;
            }
        }

        entry = OBJ_NEW(orte_routed_tree_entry_t);
        entry->target = *target;
        entry->route = *route;
        opal_list_append(&orte_routed_tree_module.peer_list, &entry->super);
        return ORTE_SUCCESS;
    }

    /* vpid wildcard */
    if (target->jobid != ORTE_JOBID_WILDCARD &&
        target->vpid == ORTE_VPID_WILDCARD) {
        opal_list_item_t *item;
        orte_routed_tree_entry_t *entry;

        for (item = opal_list_get_first(&orte_routed_tree_module.vpid_wildcard_list) ;
             item != opal_list_get_end(&orte_routed_tree_module.vpid_wildcard_list) ;
             item = opal_list_get_next(item)) {
            entry = (orte_routed_tree_entry_t*) item;

            if (0 == orte_ns.compare_fields(ORTE_NS_CMP_JOBID, 
                                            target, &entry->target)) {
                entry->route = *route;
                return ORTE_SUCCESS;
            }
        }

        entry = OBJ_NEW(orte_routed_tree_entry_t);
        entry->target = *target;
        entry->route = *route;
        opal_list_append(&orte_routed_tree_module.vpid_wildcard_list, &entry->super);
        return ORTE_SUCCESS;
    }

    /* wildcard */
    if (target->jobid == ORTE_JOBID_WILDCARD &&
        target->vpid == ORTE_VPID_WILDCARD) {
        orte_routed_tree_module.full_wildcard_entry.route = *route;
        return ORTE_SUCCESS;
    }

    return ORTE_ERR_NOT_SUPPORTED;
}


orte_process_name_t
orte_routed_tree_get_route(orte_process_name_t *target)
{
    orte_process_name_t ret;
    opal_list_item_t *item;

    /* check exact matches */
    for (item = opal_list_get_first(&orte_routed_tree_module.peer_list) ;
         item != opal_list_get_end(&orte_routed_tree_module.peer_list) ;
         item = opal_list_get_next(item)) {
        orte_routed_tree_entry_t *entry = 
            (orte_routed_tree_entry_t*) item;

        if (0 == orte_ns.compare_fields(ORTE_NS_CMP_ALL, 
                                        target, &entry->target)) {
            ret = entry->route;
            goto found;
        }
    }

    ret = orte_routed_tree_module.full_wildcard_entry.route;

 found:

    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "routed_tree_get([%s]) --> [%s]",
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(&ret)));

    return ret;
}
