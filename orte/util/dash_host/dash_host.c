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
 * Copyright (c) 2013      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "orte/constants.h"
#include "orte/types.h"

#include "orte/util/show_help.h"
#include "opal/util/argv.h"
#include "opal/util/if.h"

#include "orte/mca/ras/base/base.h"
#include "orte/mca/plm/plm_types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"

#include "dash_host.h"

/* we can only enter this routine if no other allocation
 * was found, so we only need to know that finding any
 * relative node syntax should generate an immediate error
 */
int orte_util_add_dash_host_nodes(opal_list_t *nodes,
                                  char ** host_argv)
{
    opal_list_item_t *item, *itm;
    orte_std_cntr_t i, j, k;
    int rc;
    char **mapped_nodes = NULL, **mini_map;
    orte_node_t *node, *nd;
    opal_list_t adds;
    bool found;

    OPAL_OUTPUT_VERBOSE((1, orte_ras_base_framework.framework_output,
                         "%s dashhost: parsing args",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    OBJ_CONSTRUCT(&adds, opal_list_t);

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
        rc = ORTE_SUCCESS;
        goto cleanup;
    }
    
    /*  go through the names found and
        add them to the host list. If they're not unique, then
        bump the slots count for each duplicate */
    
    for (i = 0; NULL != mapped_nodes[i]; ++i) {
        /* if the specified node contains a relative node syntax,
         * this is an error
         */
        if ('+' == mapped_nodes[i][0]) {
            orte_show_help("help-dash-host.txt", "dash-host:relative-syntax",
                           true, mapped_nodes[i]);
            rc = ORTE_ERR_SILENT;
            goto cleanup;
        }
        
        OPAL_OUTPUT_VERBOSE((1, orte_ras_base_framework.framework_output,
                             "%s dashhost: working node %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), mapped_nodes[i]));

        /* see if the node is already on the list */
        found = false;
        OPAL_LIST_FOREACH(node, &adds, orte_node_t) {
            if (0 == strcmp(node->name, mapped_nodes[i]) ||
                (0 == strcmp(node->name, orte_process_info.nodename) &&
                 (0 == strcmp(mapped_nodes[i], "localhost") || opal_ifislocal(mapped_nodes[i])))) {
                found = true;
                ++node->slots;
                OPAL_OUTPUT_VERBOSE((1, orte_ras_base_framework.framework_output,
                                     "%s dashhost: node %s already on list - slots %d",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), node->name, node->slots));
                /* the dash-host option presumes definition of num_slots */
                node->slots_given = true;
                break;
            }
        }
        
        /* If we didn't find it, add it to the list */
        if (!found) {
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
                if (orte_show_resolved_nodenames &&
                    0 != strcmp(mapped_nodes[i], orte_process_info.nodename)) {
                    /* add to list of aliases for this node - only add if unique */
                    opal_argv_append_unique_nosize(&node->alias, mapped_nodes[i], false);
                }
                node->name = strdup(orte_process_info.nodename);
            } else {
                /* not local - use the given name */
                node->name = strdup(mapped_nodes[i]);
            }
            OPAL_OUTPUT_VERBOSE((1, orte_ras_base_framework.framework_output,
                                 "%s dashhost: added node %s to list",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), node->name));
            node->state = ORTE_NODE_STATE_UP;
            node->slots_inuse = 0;
            node->slots_max = 0;
            node->slots = 1;
            /* the dash-host option presumes definition of num_slots */
            node->slots_given = true;
            opal_list_append(&adds, &node->super);
        }
    }

    /* transfer across all unique nodes */
    while (NULL != (item = opal_list_remove_first(&adds))) {
        nd = (orte_node_t*)item;
        found = false;
        for (itm = opal_list_get_first(nodes);
             itm != opal_list_get_end(nodes);
             itm = opal_list_get_next(itm)) {
            node = (orte_node_t*)itm;
            if (0 == strcmp(nd->name, node->name)) {
                found = true;
                OPAL_OUTPUT_VERBOSE((1, orte_ras_base_framework.framework_output,
                                     "%s dashhost: found existing node %s on input list - ignoring",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), node->name));
                break;
            }
        }
        if (!found) {
            OPAL_OUTPUT_VERBOSE((1, orte_ras_base_framework.framework_output,
                                 "%s dashhost: adding node %s to final list",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), nd->name));
            opal_list_append(nodes, &nd->super);
        } else {
            OBJ_RELEASE(item);
        }
    }

    rc = ORTE_SUCCESS;

 cleanup:
    if (NULL != mapped_nodes) {
        opal_argv_free(mapped_nodes);
    }
    OPAL_LIST_DESTRUCT(&adds);

    return rc;
}


/* the -host option can always be used in both absolute
 * and relative mode, so we have to check for pre-existing
 * allocations if we are to use relative node syntax
 */
static int parse_dash_host(char ***mapped_nodes, char** host_argv)
{
    orte_std_cntr_t j, k;
    int rc=ORTE_SUCCESS;
    char **mini_map, *cptr;
    int nodeidx;
    orte_node_t *node;

    /* Accumulate all of the host name mappings */
    for (j = 0; j < opal_argv_count(host_argv); ++j) {
        mini_map = opal_argv_split(host_argv[j], ',');
        
        for (k = 0; NULL != mini_map[k]; ++k) {
            if ('+' == mini_map[k][0]) {
                /* see if we specified empty nodes */
                if ('e' == mini_map[k][1] ||
                    'E' == mini_map[k][1]) {
                    /* request for empty nodes - do they want
                     * all of them?
                     */
                    if (NULL != (cptr = strchr(mini_map[k], ':'))) {
                        /* the colon indicates a specific # are requested */
                        *cptr = '*';
                        opal_argv_append_nosize(mapped_nodes, cptr);
                    } else {
                        /* add a marker to the list */
                        opal_argv_append_nosize(mapped_nodes, "*");
                    }
                } else if ('n' == mini_map[k][1] ||
                           'N' == mini_map[k][1]) {
                    /* they want a specific relative node #, so
                     * look it up on global pool
                     */
                    nodeidx = strtol(&mini_map[k][2], NULL, 10);
                    if (nodeidx < 0 ||
                        nodeidx > (int)orte_node_pool->size) {
                        /* this is an error */
                        orte_show_help("help-dash-host.txt", "dash-host:relative-node-out-of-bounds",
                                       true, nodeidx, mini_map[k]);
                        rc = ORTE_ERR_SILENT;
                        goto cleanup;
                    }
                    /* if the HNP is not allocated, then we need to
                     * adjust the index as the node pool is offset
                     * by one
                     */
                    if (!orte_hnp_is_allocated) {
                        nodeidx++;
                    }
                    /* see if that location is filled */
                    
                    if (NULL == (node = (orte_node_t *) opal_pointer_array_get_item(orte_node_pool, nodeidx))) {
                        /* this is an error */
                        orte_show_help("help-dash-host.txt", "dash-host:relative-node-not-found",
                                       true, nodeidx, mini_map[k]);
                        rc = ORTE_ERR_SILENT;
                        goto cleanup;
                    }
                    /* add this node to the list */
                    opal_argv_append_nosize(mapped_nodes, node->name);
                } else {
                    /* invalid relative node syntax */
                    orte_show_help("help-dash-host.txt", "dash-host:invalid-relative-node-syntax",
                                   true, mini_map[k]);
                    rc = ORTE_ERR_SILENT;
                    goto cleanup;
                }
            } else { /* non-relative syntax - add to list */
                if (OPAL_SUCCESS != (rc = opal_argv_append_nosize(mapped_nodes, 
                                                                  mini_map[k]))) {
                    goto cleanup;
                }
            }
        }
        opal_argv_free(mini_map);
        mini_map = NULL;
    }
    
cleanup:
    if (NULL != mini_map) {
        opal_argv_free(mini_map);
    }
    return rc;
}

int orte_util_filter_dash_host_nodes(opal_list_t *nodes,
                                     char** host_argv,
                                     bool remove)
{
    opal_list_item_t* item;
    opal_list_item_t *next;
    orte_std_cntr_t i, j, len_mapped_node=0;
    int rc;
    char **mapped_nodes = NULL;
    orte_node_t *node, *hnp_node;
    int num_empty=0;
    opal_list_t keep;
    bool want_all_empty=false;
    
    /* if the incoming node list is empty, then there
     * is nothing to filter!
     */
    if (opal_list_is_empty(nodes)) {
        return ORTE_SUCCESS;
    }

    if (ORTE_SUCCESS != (rc = parse_dash_host(&mapped_nodes, host_argv))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* Did we find anything? If not, then do nothing */
    if (NULL == mapped_nodes) {
        return ORTE_SUCCESS;
    }
    
    /* NOTE: The following logic is based on knowing that
     * any node can only be included on the incoming
     * nodes list ONCE.
     */

    /* get the hnp node's info */
    hnp_node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, 0);
        
    len_mapped_node = opal_argv_count(mapped_nodes);
    /* setup a working list so we can put the final list
     * of nodes in order. This way, if the user specifies a
     * set of nodes, we will use them in the order in which
     * they were specifed. Note that empty node requests
     * will always be appended to the end
     */
    OBJ_CONSTRUCT(&keep, opal_list_t);
    
    for (i = 0; i < len_mapped_node; ++i) {
        /* check if we are supposed to add some number of empty
         * nodes here
         */
        if ('*' == mapped_nodes[i][0]) {
            /* if there is a number after the '*', then we are
             * to insert a specific # of nodes
             */
            if ('\0' == mapped_nodes[i][1]) {
                /* take all empty nodes from the list */
                num_empty = INT_MAX;
                want_all_empty = true;
            } else {
                /* extract number of nodes to take */
                num_empty = strtol(&mapped_nodes[i][1], NULL, 10);
            }
            /* search for empty nodes and take them */
            item = opal_list_get_first(nodes);
            while (0 < num_empty && item != opal_list_get_end(nodes)) {
                next = opal_list_get_next(item);  /* save this position */
                node = (orte_node_t*)item;
                /* see if this node is empty */
                if (0 == node->slots_inuse) {
                    /* check to see if it is specified later */
                    for (j=i+1; j < len_mapped_node; j++) {
                        if (0 == strcmp(mapped_nodes[j], node->name)) {
                            /* specified later - skip this one */
                            goto skipnode;
                        }
                    }
                    if (remove) {
                        /* remove item from list */
                        opal_list_remove_item(nodes, item);
                        /* xfer to keep list */
                        opal_list_append(&keep, item);
                    } else {
                        /* mark the node as found */
                        node->mapped = true;
                    }
                    --num_empty;
                }
            skipnode:
                item = next;
            }
        } else {
            /* we are looking for a specific node on the list
             * we have a match if one of two conditions is met:
             * 1. the node_name and mapped_nodes directly match
             * 2. the node_name is the local system name AND
             *    either the mapped_node is "localhost" OR it
             *    is a local interface as found by opal_ifislocal
             */
            item = opal_list_get_first(nodes);
            while (item != opal_list_get_end(nodes)) {
                next = opal_list_get_next(item);  /* save this position */
                node = (orte_node_t*)item;
                /* search -host list to see if this one is found */
                if (0 == strcmp(node->name, mapped_nodes[i]) ||
                    (0 == strcmp(node->name, hnp_node->name) &&
                    (0 == strcasecmp(mapped_nodes[i], "localhost") || opal_ifislocal(mapped_nodes[i])))) {
                    if (remove) {
                        /* remove item from list */
                        opal_list_remove_item(nodes, item);
                        /* xfer to keep list */
                        opal_list_append(&keep, item);
                    } else {
                        /* mark the node as found */
                        node->mapped = true;
                    }
                    break;
                }
                item = next;
            }
        }
        /* done with the mapped entry */
        free(mapped_nodes[i]);
        mapped_nodes[i] = NULL;
    }

    /* was something specified that was -not- found? */
    for (i=0; i < len_mapped_node; i++) {
        if (NULL != mapped_nodes[i]) {
            orte_show_help("help-dash-host.txt", "not-all-mapped-alloc",
                           true, mapped_nodes[i]);
            rc = ORTE_ERR_SILENT;
            goto cleanup;
        }
    }
    
    if (!remove) {
        /* all done */
        rc = ORTE_SUCCESS;
        goto cleanup;
    }

    /* clear the rest of the nodes list */
    while (NULL != (item = opal_list_remove_first(nodes))) {
        OBJ_RELEASE(item);
    }
    
    /* the nodes list has been cleared - rebuild it in order */
    while (NULL != (item = opal_list_remove_first(&keep))) {
        opal_list_append(nodes, item);
    }
    
    /* did they ask for more than we could provide */
    if (!want_all_empty && 0 < num_empty) {
        orte_show_help("help-dash-host.txt", "dash-host:not-enough-empty",
                       true, num_empty);
        rc = ORTE_ERR_SILENT;
        goto cleanup;
    }
    
    rc = ORTE_SUCCESS;
    /* done filtering existing list */
    
cleanup:
    for (i=0; i < len_mapped_node; i++) {
        if (NULL != mapped_nodes[i]) {
            free(mapped_nodes[i]);
            mapped_nodes[i] = NULL;
        }
    }
    if (NULL != mapped_nodes) {
        free(mapped_nodes);
    }
    
    return rc;
}

int orte_util_get_ordered_dash_host_list(opal_list_t *nodes,
                                         char ** host_argv)
{
    int rc, i;
    char **mapped_nodes = NULL;
    orte_node_t *node;

    if (ORTE_SUCCESS != (rc = parse_dash_host(&mapped_nodes, host_argv))) {
        ORTE_ERROR_LOG(rc);
    }
    
    /* for each entry, create a node entry on the list */
    for (i=0; NULL != mapped_nodes[i]; i++) {
        node = OBJ_NEW(orte_node_t);
        node->name = strdup(mapped_nodes[i]);
        opal_list_append(nodes, &node->super);
    }
    
    /* cleanup */
    opal_argv_free(mapped_nodes);
    return rc;
}
