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
 * Copyright (c) 2013-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"

#include <ctype.h>
#include <string.h>

#include "constants.h"
#include "types.h"

#include "src/util/pmix_argv.h"
#include "src/util/pmix_if.h"
#include "src/util/pmix_net.h"
#include "src/util/proc_info.h"
#include "src/util/pmix_show_help.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/plm/plm_types.h"
#include "src/mca/ras/base/base.h"
#include "src/runtime/prte_globals.h"

#include "dash_host.h"

static bool quickmatch(prte_node_t *nd, char *name)
{
    int n;

    if (0 == strcmp(nd->name, name)) {
        return true;
    }
    if (0 == strcmp(nd->name, prte_process_info.nodename) &&
        (0 == strcmp(name, "localhost") ||
         0 == strcmp(name, "127.0.0.1"))) {
        return true;
    }
    if (NULL != nd->aliases) {
        for (n=0; NULL != nd->aliases[n]; n++) {
            if (0 == strcmp(nd->aliases[n], name)) {
                return true;
            }
        }
    }
    return false;
}

int prte_util_dash_host_compute_slots(prte_node_t *node, char *hosts)
{
    char **specs, *cptr;
    int slots = 0;
    int n;

    specs = PMIX_ARGV_SPLIT_COMPAT(hosts, ',');

    /* see if this node appears in the list */
    for (n = 0; NULL != specs[n]; n++) {
        /* check if the #slots was specified */
        if (NULL != (cptr = strchr(specs[n], ':'))) {
            *cptr = '\0';
            ++cptr;
        } else {
            cptr = NULL;
        }
        if (quickmatch(node, specs[n])) {
            if (NULL != cptr) {
                if ('*' == *cptr || 0 == strcmp(cptr, "auto")) {
                    slots += node->slots - node->slots_inuse;
                } else {
                    slots += strtol(cptr, NULL, 10);
                }
            } else {
                ++slots;
            }
        }
    }
    PMIX_ARGV_FREE_COMPAT(specs);
    return slots;
}

/* we can only enter this routine if no other allocation
 * was found, so we only need to know that finding any
 * relative node syntax should generate an immediate error
 */
int prte_util_add_dash_host_nodes(pmix_list_t *nodes, char *hosts, bool allocating)
{
    pmix_list_item_t *item;
    int32_t i, j, k;
    int rc, nodeidx;
    char **host_argv = NULL;
    char **mapped_nodes = NULL, **mini_map, *ndname;
    prte_node_t *node, *nd;
    pmix_list_t adds;
    bool needcheck;
    int slots = 0;
    bool slots_given;
    char *cptr;
    char *shortname;
    char *rawname;
    bool add_slots = false;

    PMIX_OUTPUT_VERBOSE((1, prte_ras_base_framework.framework_output,
                         "%s dashhost: parsing args %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                         hosts));

    PMIX_CONSTRUCT(&adds, pmix_list_t);
    host_argv = PMIX_ARGV_SPLIT_COMPAT(hosts, ',');
    if (0 < pmix_list_get_size(nodes)) {
        needcheck = true;
    } else {
        needcheck = false;
    }

    /* Accumulate all of the host name mappings */
    for (j = 0; j < PMIX_ARGV_COUNT_COMPAT(host_argv); ++j) {
        mini_map = PMIX_ARGV_SPLIT_COMPAT(host_argv[j], ',');

        if (mapped_nodes == NULL) {
            mapped_nodes = mini_map;
        } else {
            for (k = 0; NULL != mini_map[k]; ++k) {
                rc = PMIX_ARGV_APPEND_NOSIZE_COMPAT(&mapped_nodes, mini_map[k]);
                if (PRTE_SUCCESS != rc) {
                    PMIX_ARGV_FREE_COMPAT(host_argv);
                    PMIX_ARGV_FREE_COMPAT(mini_map);
                    goto cleanup;
                }
            }
            PMIX_ARGV_FREE_COMPAT(mini_map);
        }
    }
    PMIX_ARGV_FREE_COMPAT(host_argv);
    mini_map = NULL;

    /* Did we find anything? If not, then do nothing */
    if (NULL == mapped_nodes) {
        rc = PRTE_SUCCESS;
        goto cleanup;
    }

    for (i = 0; NULL != mapped_nodes[i]; ++i) {
        /* if the specified node contains a relative node syntax,
         * and we are allocating, then ignore it
         */
        if ('+' == mapped_nodes[i][0]) {
            if (!allocating) {
                if ('e' == mapped_nodes[i][1] || 'E' == mapped_nodes[i][1]) {
                    /* request for empty nodes - do they want
                     * all of them?
                     */
                    if (NULL != (cptr = strchr(mapped_nodes[i], ':'))) {
                        /* the colon indicates a specific # are requested */
                        ++cptr;
                        j = strtoul(cptr, NULL, 10);
                    } else if ('\0' != mapped_nodes[0][2]) {
                        j = strtoul(&mapped_nodes[0][2], NULL, 10);
                    } else {
                        /* add them all */
                        j = prte_node_pool->size;
                    }
                    for (k = 0; 0 < j && k < prte_node_pool->size; k++) {
                        if (NULL
                            != (node = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, k))) {
                            if (0 == node->num_procs) {
                                PMIX_ARGV_APPEND_NOSIZE_COMPAT(&mini_map, node->name);
                                --j;
                            }
                        }
                    }
                } else if ('n' == mapped_nodes[i][1] || 'N' == mapped_nodes[i][1]) {
                    /* they want a specific relative node #, so
                     * look it up on global pool
                     */
                    if ('\0' == mapped_nodes[i][2]) {
                        /* they forgot to tell us the # */
                        pmix_show_help("help-dash-host.txt",
                                       "dash-host:invalid-relative-node-syntax", true,
                                       mapped_nodes[i]);
                        rc = PRTE_ERR_SILENT;
                        goto cleanup;
                    }
                    nodeidx = strtol(&mapped_nodes[i][2], NULL, 10);
                    if (nodeidx < 0 || nodeidx > (int) prte_node_pool->size) {
                        /* this is an error */
                        pmix_show_help("help-dash-host.txt",
                                       "dash-host:relative-node-out-of-bounds", true, nodeidx,
                                       mapped_nodes[i]);
                        rc = PRTE_ERR_SILENT;
                        goto cleanup;
                    }
                    /* if the HNP is not allocated, then we need to
                     * adjust the index as the node pool is offset
                     * by one
                     */
                    if (!prte_hnp_is_allocated) {
                        nodeidx++;
                    }
                    /* see if that location is filled */
                    node = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, nodeidx);
                    if (NULL == node) {
                        /* this is an error */
                        pmix_show_help("help-dash-host.txt", "dash-host:relative-node-not-found",
                                       true, nodeidx, mapped_nodes[i]);
                        rc = PRTE_ERR_SILENT;
                        goto cleanup;
                    }
                    /* add this node to the list */
                    PMIX_ARGV_APPEND_NOSIZE_COMPAT(&mini_map, node->name);
                } else {
                    /* invalid relative node syntax */
                    pmix_show_help("help-dash-host.txt", "dash-host:invalid-relative-node-syntax",
                                   true, mapped_nodes[i]);
                    rc = PRTE_ERR_SILENT;
                    goto cleanup;
                }
            }
        } else {
            /* just one node was given */
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&mini_map, mapped_nodes[i]);
        }
    }
    if (NULL == mini_map) {
        rc = PRTE_SUCCESS;
        goto cleanup;
    }

    /*  go through the names found and
        add them to the host list. If they're not unique, then
        bump the slots count for each duplicate */
    for (i = 0; NULL != mini_map[i]; i++) {
        PMIX_OUTPUT_VERBOSE((1, prte_ras_base_framework.framework_output,
                             "%s dashhost: working node %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                             mini_map[i]));

        /* see if the node contains the number of slots */
        slots_given = false;
        if (NULL != (cptr = strchr(mini_map[i], ':'))) {
            *cptr = '\0';
            ++cptr;
            if ('*' == *cptr || 0 == strcmp(cptr, "auto")) {
                /* auto-detect #slots */
                slots = -1;
                slots_given = false;
            } else {
                slots = strtol(cptr, NULL, 10);
                if ('+' == *cptr || '-' == *cptr) {
                    // mark that we are being asked to increase/decrease #slots
                    add_slots = true;
                }
                slots_given = true;
            }
        }

        /* check for local name and compute non-fqdn name */
        shortname = NULL;
        rawname = NULL;
        if (prte_check_host_is_local(mini_map[i])) {
            ndname = prte_process_info.nodename;
        } else {
            ndname = mini_map[i];
            /* compute the non-fqdn version */
            if (!prte_keep_fqdn_hostnames &&
                !pmix_net_isaddr(ndname)) {
                cptr = strchr(ndname, '.');
                if (NULL != cptr) {
                    rawname = strdup(ndname);
                    *cptr = '\0';
                    shortname = strdup(ndname);
                    *cptr = '.';
                }
            }
        }
        /* see if a node of this name is already on the list */
        node = prte_node_match(&adds, ndname);
        if (NULL == node && NULL != shortname) {
            node = prte_node_match(&adds, shortname);
        }
        if (NULL != node) {
            if (slots_given) {
                node->slots += slots;
                PRTE_FLAG_SET(node, PRTE_NODE_FLAG_SLOTS_GIVEN);
                if (add_slots) {
                    prte_set_attribute(&node->attributes, PRTE_NODE_ADD_SLOTS,
                                       PRTE_ATTR_GLOBAL, NULL, PMIX_BOOL);
                }
            } else if (slots < 0) {
                node->slots = 0;
                PRTE_FLAG_UNSET(node, PRTE_NODE_FLAG_SLOTS_GIVEN);
            } else {
                ++node->slots;
                PRTE_FLAG_SET(node, PRTE_NODE_FLAG_SLOTS_GIVEN);
            }
            PMIX_OUTPUT_VERBOSE((1, prte_ras_base_framework.framework_output,
                                 "%s dashhost: node %s already on list - slots %d",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), node->name, node->slots));
            if (NULL != shortname) {
                free(shortname);
                shortname = NULL;
            }
            if (NULL != rawname) {
                node->rawname = rawname;
                rawname = NULL;
            }
        } else {
            /* if we didn't find it, add it to the list */
            node = PMIX_NEW(prte_node_t);
            if (NULL == node) {
                PMIX_ARGV_FREE_COMPAT(mapped_nodes);
                if (NULL != shortname) {
                    free(shortname);
                }
                if (NULL != rawname) {
                    free(rawname);
                }
                return PRTE_ERR_OUT_OF_RESOURCE;
            }
            if (prte_keep_fqdn_hostnames || NULL == shortname) {
                node->name = strdup(ndname);
            } else {
                node->name = strdup(shortname);
            }
            if (NULL != rawname) {
                node->rawname = rawname;
                rawname = NULL;
            }
            PMIX_OUTPUT_VERBOSE((1, prte_ras_base_framework.framework_output,
                                 "%s dashhost: added node %s to list - slots %d",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), node->name, slots));
            node->state = PRTE_NODE_STATE_UP;
            node->slots_inuse = 0;
            node->slots_max = 0;
            if (slots_given) {
                node->slots = slots;
                PRTE_FLAG_SET(node, PRTE_NODE_FLAG_SLOTS_GIVEN);
                if (add_slots) {
                    prte_set_attribute(&node->attributes, PRTE_NODE_ADD_SLOTS,
                                       PRTE_ATTR_GLOBAL, NULL, PMIX_BOOL);
                }
            } else if (slots < 0) {
                node->slots = 0;
                PRTE_FLAG_UNSET(node, PRTE_NODE_FLAG_SLOTS_GIVEN);
            } else {
                node->slots = 1;
                PRTE_FLAG_SET(node, PRTE_NODE_FLAG_SLOTS_GIVEN);
            }
            pmix_list_append(&adds, &node->super);
        }
        if (0 != strcmp(node->name, mini_map[i])) {
            // add the mini_map name to the list of aliases
            PMIX_ARGV_APPEND_UNIQUE_COMPAT(&node->aliases, mini_map[i]);
        }
        // ensure the non-fqdn version is saved
        if (NULL != shortname && 0 != strcmp(shortname, node->name)) {
            PMIX_ARGV_APPEND_UNIQUE_COMPAT(&node->aliases, shortname);
        }
        if (NULL != shortname) {
            free(shortname);
        }
        if (NULL != rawname) {
            free(rawname);
        }
    }
    PMIX_ARGV_FREE_COMPAT(mini_map);

    /* transfer across all unique nodes */
    while (NULL != (item = pmix_list_remove_first(&adds))) {
        nd = (prte_node_t *) item;
        if (needcheck) {
            node = prte_node_match(nodes, nd->name);
            if (NULL != node) {
                PMIX_OUTPUT_VERBOSE((1, prte_ras_base_framework.framework_output,
                     "%s dashhost: found existing node %s on input list - adding slots",
                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), node->name));
                if (PRTE_FLAG_TEST(nd, PRTE_NODE_FLAG_SLOTS_GIVEN)) {
                    /* transfer across the number of slots */
                    node->slots += nd->slots;
                    PRTE_FLAG_SET(node, PRTE_NODE_FLAG_SLOTS_GIVEN);
                }
                PMIX_RELEASE(item);
           } else {
                PMIX_OUTPUT_VERBOSE((1, prte_ras_base_framework.framework_output,
                                     "%s dashhost: adding node %s with %d slots to final list",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), nd->name, nd->slots));
                pmix_list_append(nodes, &nd->super);
            }
        } else {
            PMIX_OUTPUT_VERBOSE((1, prte_ras_base_framework.framework_output,
                                 "%s dashhost: adding node %s with %d slots to final list",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), nd->name, nd->slots));
            pmix_list_append(nodes, &nd->super);
        }
    }

    if (prte_managed_allocation && !allocating) {
        prte_node_t *node_from_pool = NULL;
        PMIX_LIST_FOREACH(node, nodes, prte_node_t) {
            needcheck = true;
            for (i = 0; i < prte_node_pool->size; i++) {
                node_from_pool = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, i);
                if (NULL == node_from_pool) {
                    continue;
                }
                if (prte_nptr_match(node_from_pool, node)) {
                    needcheck = false;
                    if (node->slots < node_from_pool->slots) {
                        node_from_pool->slots = node->slots;
                    }
                    break;
                }
            }
            if (needcheck) {
                // node in -host was not in allocation - this is not allowed
                pmix_show_help("help-dash-host.txt", "not-all-mapped-alloc",
                               true, node->name);
                rc = PRTE_ERR_SILENT;
                goto cleanup;
            }
        }
    }
    rc = PRTE_SUCCESS;

cleanup:
    if (NULL != mapped_nodes) {
        PMIX_ARGV_FREE_COMPAT(mapped_nodes);
    }
    PMIX_LIST_DESTRUCT(&adds);

    return rc;
}

/* the -host option can always be used in both absolute
 * and relative mode, so we have to check for pre-existing
 * allocations if we are to use relative node syntax
 */
static int parse_dash_host(char ***mapped_nodes, char *hosts)
{
    int32_t j, k;
    int rc = PRTE_SUCCESS;
    char **mini_map = NULL, *cptr;
    int nodeidx;
    prte_node_t *node;
    char **host_argv = NULL;

    host_argv = PMIX_ARGV_SPLIT_COMPAT(hosts, ',');

    /* Accumulate all of the host name mappings */
    for (j = 0; j < PMIX_ARGV_COUNT_COMPAT(host_argv); ++j) {
        mini_map = PMIX_ARGV_SPLIT_COMPAT(host_argv[j], ',');

        for (k = 0; NULL != mini_map[k]; ++k) {
            if ('+' == mini_map[k][0]) {
                /* see if we specified empty nodes */
                if ('e' == mini_map[k][1] || 'E' == mini_map[k][1]) {
                    /* request for empty nodes - do they want
                     * all of them?
                     */
                    if (NULL != (cptr = strchr(mini_map[k], ':'))) {
                        /* the colon indicates a specific # are requested */
                        *cptr = '*';
                        PMIX_ARGV_APPEND_NOSIZE_COMPAT(mapped_nodes, cptr);
                    } else {
                        /* add a marker to the list */
                        PMIX_ARGV_APPEND_NOSIZE_COMPAT(mapped_nodes, "*");
                    }
                } else if ('n' == mini_map[k][1] || 'N' == mini_map[k][1]) {
                    /* they want a specific relative node #, so
                     * look it up on global pool
                     */
                    nodeidx = strtol(&mini_map[k][2], NULL, 10);
                    if (nodeidx < 0 || nodeidx > (int) prte_node_pool->size) {
                        /* this is an error */
                        pmix_show_help("help-dash-host.txt",
                                       "dash-host:relative-node-out-of-bounds", true, nodeidx,
                                       mini_map[k]);
                        rc = PRTE_ERR_SILENT;
                        goto cleanup;
                    }
                    /* if the HNP is not allocated, then we need to
                     * adjust the index as the node pool is offset
                     * by one
                     */
                    if (!prte_hnp_is_allocated) {
                        nodeidx++;
                    }
                    /* see if that location is filled */
                    node = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, nodeidx);
                    if (NULL == node) {
                        /* this is an error */
                        pmix_show_help("help-dash-host.txt", "dash-host:relative-node-not-found",
                                       true, nodeidx, mini_map[k]);
                        rc = PRTE_ERR_SILENT;
                        goto cleanup;
                    }
                    /* add this node to the list */
                    PMIX_ARGV_APPEND_NOSIZE_COMPAT(mapped_nodes, node->name);
                } else {
                    /* invalid relative node syntax */
                    pmix_show_help("help-dash-host.txt", "dash-host:invalid-relative-node-syntax",
                                   true, mini_map[k]);
                    rc = PRTE_ERR_SILENT;
                    goto cleanup;
                }
            } else { /* non-relative syntax - add to list */
                /* remove any modifier */
                if (NULL != (cptr = strchr(mini_map[k], ':'))) {
                    *cptr = '\0';
                }
                /* check for local alias */
                if (prte_check_host_is_local(mini_map[k])) {
                    PMIX_ARGV_APPEND_NOSIZE_COMPAT(mapped_nodes, prte_process_info.nodename);
                } else {
                    PMIX_ARGV_APPEND_NOSIZE_COMPAT(mapped_nodes, mini_map[k]);
                }
            }
        }
        PMIX_ARGV_FREE_COMPAT(mini_map);
        mini_map = NULL;
    }

cleanup:
    if (NULL != host_argv) {
        PMIX_ARGV_FREE_COMPAT(host_argv);
    }
    if (NULL != mini_map) {
        PMIX_ARGV_FREE_COMPAT(mini_map);
    }
    return rc;
}

int prte_util_filter_dash_host_nodes(pmix_list_t *nodes, char *hosts, bool remove)
{
    pmix_list_item_t *item;
    pmix_list_item_t *next;
    int32_t i, j, len_mapped_node = 0;
    int rc, test;
    char **mapped_nodes = NULL;
    prte_node_t *node;
    int num_empty = 0;
    pmix_list_t keep;
    bool want_all_empty = false;
    char *cptr;
    size_t lst, lmn;

    /* if the incoming node list is empty, then there
     * is nothing to filter!
     */
    if (pmix_list_is_empty(nodes)) {
        return PRTE_SUCCESS;
    }

    if (PRTE_SUCCESS != (rc = parse_dash_host(&mapped_nodes, hosts))) {
        PRTE_ERROR_LOG(rc);
        return rc;
    }
    /* Did we find anything? If not, then do nothing */
    if (NULL == mapped_nodes) {
        return PRTE_SUCCESS;
    }

    /* NOTE: The following logic is based on knowing that
     * any node can only be included on the incoming
     * nodes list ONCE.
     */

    len_mapped_node = PMIX_ARGV_COUNT_COMPAT(mapped_nodes);
    /* setup a working list so we can put the final list
     * of nodes in order. This way, if the user specifies a
     * set of nodes, we will use them in the order in which
     * they were specifed. Note that empty node requests
     * will always be appended to the end
     */
    PMIX_CONSTRUCT(&keep, pmix_list_t);

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
            item = pmix_list_get_first(nodes);
            while (0 < num_empty && item != pmix_list_get_end(nodes)) {
                next = pmix_list_get_next(item); /* save this position */
                node = (prte_node_t *) item;
                /* see if this node is empty */
                if (0 == node->slots_inuse) {
                    /* check to see if it is specified later */
                    for (j = i + 1; j < len_mapped_node; j++) {
                        if (0 == strcmp(mapped_nodes[j], node->name)) {
                            /* specified later - skip this one */
                            goto skipnode;
                        }
                    }
                    if (remove) {
                        /* remove item from list */
                        pmix_list_remove_item(nodes, item);
                        /* xfer to keep list */
                        pmix_list_append(&keep, item);
                    } else {
                        /* mark the node as found */
                        PRTE_FLAG_SET(node, PRTE_NODE_FLAG_MAPPED);
                    }
                    --num_empty;
                }
            skipnode:
                item = next;
            }
        } else {
            /* remove any modifier */
            if (NULL != (cptr = strchr(mapped_nodes[i], ':'))) {
                *cptr = '\0';
            }
            /* we are looking for a specific node on the list. */
            cptr = NULL;
            lmn = strtoul(mapped_nodes[i], &cptr, 10);
            item = pmix_list_get_first(nodes);
            while (item != pmix_list_get_end(nodes)) {
                next = pmix_list_get_next(item); /* save this position */
                node = (prte_node_t *) item;
                /* search -host list to see if this one is found */
                if (prte_managed_allocation && (NULL == cptr || 0 == strlen(cptr))) {
                    /* if we are only given a number, then we test the
                     * value against the number in the node name. This allows support for
                     * launch_id-based environments. For example, a hostname
                     * of "nid0015" can be referenced by "--host 15" */
                    for (j = strlen(node->name) - 1; 0 < j; j--) {
                        if (!isdigit(node->name[j])) {
                            j++;
                            break;
                        }
                    }
                    if (j >= (int) (strlen(node->name) - 1)) {
                        test = 0;
                    } else {
                        lst = strtoul(&node->name[j], NULL, 10);
                        test = (lmn == lst) ? 0 : 1;
                    }
                } else {
                    test = (quickmatch(node, mapped_nodes[i])) ? 0 : 1;
                }
                if (0 == test) {
                    if (remove) {
                        /* remove item from list */
                        pmix_list_remove_item(nodes, item);
                        /* xfer to keep list */
                        pmix_list_append(&keep, item);
                    } else {
                        /* mark the node as found */
                        PRTE_FLAG_SET(node, PRTE_NODE_FLAG_MAPPED);
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
    for (i = 0; i < len_mapped_node; i++) {
        if (NULL != mapped_nodes[i]) {
            pmix_show_help("help-dash-host.txt", "not-all-mapped-alloc", true, mapped_nodes[i]);
            rc = PRTE_ERR_SILENT;
            goto cleanup;
        }
    }

    if (!remove) {
        /* all done */
        rc = PRTE_SUCCESS;
        goto cleanup;
    }

    /* clear the rest of the nodes list */
    while (NULL != (item = pmix_list_remove_first(nodes))) {
        PMIX_RELEASE(item);
    }

    /* the nodes list has been cleared - rebuild it in order */
    while (NULL != (item = pmix_list_remove_first(&keep))) {
        pmix_list_append(nodes, item);
    }

    /* did they ask for more than we could provide */
    if (!want_all_empty && 0 < num_empty) {
        pmix_show_help("help-dash-host.txt", "dash-host:not-enough-empty", true, num_empty);
        rc = PRTE_ERR_SILENT;
        goto cleanup;
    }

    rc = PRTE_SUCCESS;
    /* done filtering existing list */

cleanup:
    for (i = 0; i < len_mapped_node; i++) {
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

int prte_util_get_ordered_dash_host_list(pmix_list_t *nodes, char *hosts)
{
    int rc, i;
    char **mapped_nodes = NULL;
    prte_node_t *node;

    if (PRTE_SUCCESS != (rc = parse_dash_host(&mapped_nodes, hosts))) {
        PRTE_ERROR_LOG(rc);
    }

    /* for each entry, create a node entry on the list */
    for (i = 0; NULL != mapped_nodes[i]; i++) {
        node = PMIX_NEW(prte_node_t);
        node->name = strdup(mapped_nodes[i]);
        pmix_list_append(nodes, &node->super);
    }

    /* cleanup */
    PMIX_ARGV_FREE_COMPAT(mapped_nodes);
    return rc;
}
