/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/util/if.h"
#include "opal/util/show_help.h"

#include "opal/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/plm/plm_types.h"
#include "orte/util/proc_info.h"
#include "orte/util/sys_info.h"
#include "orte/runtime/orte_globals.h"

#include "dash_host.h"


int orte_util_add_dash_host_nodes(opal_list_t *nodes,
                                  bool *override_oversubscribed,
                                  char ** host_argv)
{
    opal_list_item_t* item;
    orte_std_cntr_t i, j, k;
    int rc;
    char **mapped_nodes = NULL, **mini_map;
    orte_node_t *node;

    /* Accumulate all of the host name mappings */
    for (j = 0; j < opal_argv_count(host_argv); ++j) {
        mini_map = opal_argv_split(host_argv[j], ',');
        
        if (mapped_nodes == NULL) {
            mapped_nodes = mini_map;
        } else {
            for (k = 0; NULL != mini_map[k]; ++k) {
                rc = opal_argv_append_nosize(&mapped_nodes, 
                                             mini_map[k]);
                if (OPAL_SUCCESS != rc) {
                    goto cleanup;
                }
            }
            opal_argv_free(mini_map);
        }
    }

    /* Did we find anything? If not, then do nothing */
    if (NULL == mapped_nodes) {
        return ORTE_SUCCESS;
    }
    
    /*  go through the names found and
       add them to the host list. If they're not unique, then
       bump the slots count for each duplicate */
    
    for (i = 0; NULL != mapped_nodes[i]; ++i) {
        for (item = opal_list_get_first(nodes); 
             item != opal_list_get_end(nodes);
             item = opal_list_get_next(item)) {
            node = (orte_node_t*) item;
            if (0 == strcmp(node->name, mapped_nodes[i]) ||
               (0 == strcmp(node->name, orte_system_info.nodename) &&
               (0 == strcmp(mapped_nodes[i], "localhost") || opal_ifislocal(mapped_nodes[i])))) {
                ++node->slots;
                break;
            }
        }
        
        /* If we didn't find it, add it to the list */
        
        if (item == opal_list_get_end(nodes)) {
            node = OBJ_NEW(orte_node_t);
            if (NULL == node) {
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            /* check to see if this is a local name */
            if (0 == strcmp(mapped_nodes[i], "localhost") ||
                opal_ifislocal(mapped_nodes[i])) {
                /* it is local, so use the local nodename to avoid
                 * later confusion
                 */
                node->name = strdup(orte_system_info.nodename);
            } else {
                /* not local - use the given name */
                node->name = strdup(mapped_nodes[i]);
            }
            node->state = ORTE_NODE_STATE_UP;
            node->slots_inuse = 0;
            node->slots_max = 0;
            node->slots = 1;
            /* indicate that ORTE should override any oversubscribed conditions
             * based on local hardware limits since the user (a) might not have
             * provided us any info on the #slots for a node, and (b) the user
             * might have been wrong! If we don't check the number of local physical
             * processors, then we could be too aggressive on our sched_yield setting
             * and cause performance problems.
             */
            *override_oversubscribed = true;
            opal_list_append(nodes, &node->super);
        }
    }
    rc = ORTE_SUCCESS;

cleanup:
    if (NULL != mapped_nodes) {
        opal_argv_free(mapped_nodes);
    }

    return rc;
}


int orte_util_filter_dash_host_nodes(opal_list_t *nodes,
                                     char** host_argv)
{
    opal_list_item_t* item;
    bool found;
    opal_list_item_t *next;
    orte_std_cntr_t i, j, k;
    int rc;
    char **mapped_nodes = NULL, **mini_map;
    orte_node_t *node;
    
    /* if the incoming node list is empty, then there
     * is nothing to filter!
     */
    if (opal_list_is_empty(nodes)) {
        return ORTE_SUCCESS;
    }

    
    /* Accumulate all of the host name mappings */
    for (j = 0; j < opal_argv_count(host_argv); ++j) {
        mini_map = opal_argv_split(host_argv[j], ',');
        
        if (mapped_nodes == NULL) {
            mapped_nodes = mini_map;
        } else {
            for (k = 0; NULL != mini_map[k]; ++k) {
                rc = opal_argv_append_nosize(&mapped_nodes, 
                                             mini_map[k]);
                if (OPAL_SUCCESS != rc) {
                    goto cleanup;
                }
            }
            opal_argv_free(mini_map);
        }
    }
    
    /* Did we find anything? If not, then do nothing */
    if (NULL == mapped_nodes) {
        return ORTE_SUCCESS;
    }
        
    /* we found some info - filter what is on the list...
     * i.e., go through the list and remove any nodes that
     * were -not- included on the -host list
     */
    j=0;
    k = opal_argv_count(mapped_nodes);
    item = opal_list_get_first(nodes);
    while (item != opal_list_get_end(nodes)) {
        /* hang on to next item in case this one gets removed */
        next = opal_list_get_next(item);
        node = (orte_node_t*)item;
        /* search -host list to see if this one is found */
        found = false;
        for (i = 0; NULL != mapped_nodes[i]; ++i) {
            /* we have a match if one of two conditions is met:
            * 1. the node_name and mapped_nodes directly match
            * 2. the node_name is the local system name AND
            *    either the mapped_node is "localhost" OR it
            *    is a local interface as found by opal_ifislocal
            */
            if (0 == strcmp(node->name, mapped_nodes[i]) ||
               (0 == strcmp(node->name, orte_system_info.nodename) &&
               (0 == strcmp(mapped_nodes[i], "localhost") || opal_ifislocal(mapped_nodes[i])))) {
                found = true;  /* found it - leave it alone */
                j++;
                /* keep cycling here in case there are multiple instances
                 * of the node on the mapped_node array - this will
                 * allow us to properly account for them all so we don't
                 * think something was specified but wasn't found
                 */
            }
        }
        if (!found) {
            opal_list_remove_item(nodes, item);
            OBJ_RELEASE(item);
        }
        item = next;    /* move on */
    }
    
    /* was something specified that was -not- found? */
    if (j < k) {
        char *tmp;
        tmp = opal_argv_join(mapped_nodes, ',');
        opal_show_help("help-dash-host.txt", "not-all-mapped-alloc",
                       true, tmp);
        free(tmp);
        rc = ORTE_ERR_SILENT;
        goto cleanup;
    }
    
    rc = ORTE_SUCCESS;
    /* done filtering existing list */

cleanup:
    if (NULL != mapped_nodes) {
        opal_argv_free(mapped_nodes);
    }
    
    return rc;
}

