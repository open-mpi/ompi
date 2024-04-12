/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016-2021 IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <string.h>

#include "src/hwloc/hwloc-internal.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"
#include "src/threads/pmix_tsd.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_if.h"
#include "src/util/pmix_output.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/ess/ess.h"
#include "src/runtime/prte_globals.h"
#include "src/util/dash_host/dash_host.h"
#include "src/util/hostfile/hostfile.h"
#include "src/util/name_fns.h"
#include "src/util/pmix_show_help.h"
#include "types.h"

#include "src/mca/rmaps/base/base.h"
#include "src/mca/rmaps/base/rmaps_private.h"

int prte_rmaps_base_filter_nodes(prte_app_context_t *app, pmix_list_t *nodes, bool remove)
{
    int rc = PRTE_ERR_TAKE_NEXT_OPTION;
    char *hosts;

    /* did the app_context contain a hostfile? */
    hosts = NULL;
    if (prte_get_attribute(&app->attributes, PRTE_APP_HOSTFILE, (void **) &hosts, PMIX_STRING) &&
        NULL != hosts) {
        /* yes - filter the node list through the file, removing
         * any nodes not found in the file
         */
        if (PRTE_SUCCESS != (rc = prte_util_filter_hostfile_nodes(nodes, hosts, remove))) {
            PRTE_ERROR_LOG(rc);
            free(hosts);
            return rc;
        }
        /** check that anything is here */
        if (0 == pmix_list_get_size(nodes)) {
            pmix_show_help("help-prte-rmaps-base.txt", "prte-rmaps-base:no-mapped-node", true,
                           app->app, "-hostfile", hosts);
            free(hosts);
            return PRTE_ERR_SILENT;
        }
        free(hosts);
    }
    /* now filter the list through any -host specification */
    hosts = NULL;
    if (prte_get_attribute(&app->attributes, PRTE_APP_DASH_HOST, (void **) &hosts, PMIX_STRING) &&
        NULL != hosts) {
        if (PRTE_SUCCESS != (rc = prte_util_filter_dash_host_nodes(nodes, hosts, remove))) {
            PRTE_ERROR_LOG(rc);
            free(hosts);
            return rc;
        }
        /** check that anything is left! */
        if (0 == pmix_list_get_size(nodes)) {
            pmix_show_help("help-prte-rmaps-base.txt", "prte-rmaps-base:no-mapped-node", true,
                           app->app, "-host", hosts);
            free(hosts);
            return PRTE_ERR_SILENT;
        }
        free(hosts);
    }

    return rc;
}

/*
 * Query the registry for all nodes allocated to a specified app_context
 */
int prte_rmaps_base_get_target_nodes(pmix_list_t *allocated_nodes,
                                     int32_t *total_num_slots,
                                     prte_job_t *jdata, prte_app_context_t *app,
                                     prte_mapping_policy_t policy,
                                     bool initial_map, bool silent)
{
    pmix_list_item_t *item;
    prte_node_t *node, *nd, *nptr, *next;
    int32_t num_slots;
    int32_t i;
    int rc;
    prte_job_t *daemons;
    bool novm;
    pmix_list_t nodes;
    char *hosts = NULL;
    bool needhosts = false;
    /** set default answer */
    *total_num_slots = 0;

    /* get the daemon job object */
    daemons = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);
    /* see if we have a vm or not */
    novm = prte_get_attribute(&daemons->attributes, PRTE_JOB_NO_VM, NULL, PMIX_BOOL);

    /* if this is NOT a managed allocation, then we use the nodes
     * that were specified for this app - there is no need to collect
     * all available nodes and "filter" them.
     *
     * However, if it is a managed allocation AND the hostfile or the hostlist was
     * provided, those take precedence, so process them and filter as we normally do.
     */
    if (prte_get_attribute(&app->attributes, PRTE_APP_DASH_HOST, (void **) &hosts, PMIX_STRING) ||
        prte_get_attribute(&app->attributes, PRTE_APP_HOSTFILE, (void **) &hosts, PMIX_STRING)) {
        needhosts = true;
    }
    if (!prte_managed_allocation ||
        (prte_managed_allocation && needhosts)) {
        PMIX_CONSTRUCT(&nodes, pmix_list_t);
        /* if the app provided a dash-host, then use those nodes */
        hosts = NULL;
        if (prte_get_attribute(&app->attributes, PRTE_APP_DASH_HOST, (void **) &hosts, PMIX_STRING)) {
            PMIX_OUTPUT_VERBOSE((5, prte_rmaps_base_framework.framework_output,
                                 "%s using dash_host %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                 hosts));
            if (PRTE_SUCCESS != (rc = prte_util_add_dash_host_nodes(&nodes, hosts, false))) {
                PRTE_ERROR_LOG(rc);
                free(hosts);
                return rc;
            }
            free(hosts);
        } else if (prte_get_attribute(&app->attributes, PRTE_APP_HOSTFILE, (void **) &hosts, PMIX_STRING)) {
            /* otherwise, if the app provided a hostfile, then use that */
            PMIX_OUTPUT_VERBOSE((5, prte_rmaps_base_framework.framework_output,
                                 "%s using hostfile %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                 hosts));
            if (PRTE_SUCCESS != (rc = prte_util_add_hostfile_nodes(&nodes, hosts))) {
                free(hosts);
                PRTE_ERROR_LOG(rc);
                return rc;
            }
            free(hosts);
        } else {
            /* if nothing else was specified by the app, then use all known nodes, which
             * will include ourselves
             */
            PMIX_OUTPUT_VERBOSE((5, prte_rmaps_base_framework.framework_output,
                                 "%s using known nodes", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
            goto addknown;
        }
        /** if we still don't have anything */
        if (0 == pmix_list_get_size(&nodes)) {
            if (!silent) {
                pmix_show_help("help-prte-rmaps-base.txt", "prte-rmaps-base:no-available-resources",
                               true);
            }
            PMIX_DESTRUCT(&nodes);
            return PRTE_ERR_SILENT;
        }
        /* find the nodes in our node array and assemble them
         * in list order as that is what the user specified. Note
         * that the prte_node_t objects on the nodes list are not
         * fully filled in - they only contain the user-provided
         * name of the node as a temp object. Thus, we cannot just
         * check to see if the node pointer matches that of a node
         * in the node_pool.
         */
        PMIX_LIST_FOREACH_SAFE(nptr, next, &nodes, prte_node_t)
        {
            for (i = 0; i < prte_node_pool->size; i++) {
                node = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, i);
                if (NULL == node) {
                    continue;
                }
                /* ignore nodes that are non-usable */
                if (PRTE_FLAG_TEST(node, PRTE_NODE_NON_USABLE)) {
                    continue;
                }
                /* ignore nodes that are marked as do-not-use for this mapping */
                if (PRTE_NODE_STATE_DO_NOT_USE == node->state) {
                    PMIX_OUTPUT_VERBOSE((10, prte_rmaps_base_framework.framework_output,
                                         "NODE %s IS MARKED NO_USE", node->name));
                    /* reset the state so it can be used another time */
                    node->state = PRTE_NODE_STATE_UP;
                    continue;
                }
                if (PRTE_NODE_STATE_DOWN == node->state) {
                    PMIX_OUTPUT_VERBOSE((10, prte_rmaps_base_framework.framework_output,
                                         "NODE %s IS DOWN", node->name));
                    continue;
                }
                if (PRTE_NODE_STATE_NOT_INCLUDED == node->state) {
                    PMIX_OUTPUT_VERBOSE((10, prte_rmaps_base_framework.framework_output,
                                         "NODE %s IS MARKED NO_INCLUDE", node->name));
                    /* not to be used */
                    continue;
                }
                /* if this node wasn't included in the vm (e.g., by -host), ignore it,
                 * unless we are mapping prior to launching the vm
                 */
                if (NULL == node->daemon && !novm) {
                    PMIX_OUTPUT_VERBOSE((10, prte_rmaps_base_framework.framework_output,
                                         "NODE %s HAS NO DAEMON", node->name));
                    continue;
                }
                if (!prte_nptr_match(node, nptr)) {
                    PMIX_OUTPUT_VERBOSE((10, prte_rmaps_base_framework.framework_output,
                                         "NODE %s DOESNT MATCH NODE %s", node->name, nptr->name));
                    continue;
                }
                /* retain a copy for our use in case the item gets
                 * destructed along the way
                 */
                PMIX_RETAIN(node);
                if (initial_map) {
                    /* if this is the first app_context we
                     * are getting for an initial map of a job,
                     * then mark all nodes as unmapped
                     */
                    PRTE_FLAG_UNSET(node, PRTE_NODE_FLAG_MAPPED);
                }
                /* the list is ordered as per user direction using -host
                 * or the listing in -hostfile - preserve that ordering */
                pmix_list_append(allocated_nodes, &node->super);
                break;
            }
            /* remove the item from the list as we have allocated it */
            pmix_list_remove_item(&nodes, (pmix_list_item_t *) nptr);
            PMIX_RELEASE(nptr);
        }
        PMIX_DESTRUCT(&nodes);
        /* now prune for usage and compute total slots */
        goto complete;
    }

addknown:
    /* add everything in the node pool that can be used - add them
     * in daemon order, which may be different than the order in the
     * node pool. Since an empty list is passed into us, the list at
     * this point either has the HNP node or nothing, and the HNP
     * node obviously has a daemon on it (us!)
     */
    if (0 == pmix_list_get_size(allocated_nodes)) {
        /* the list is empty - if the HNP is allocated, then add it */
        if (prte_hnp_is_allocated) {
            nd = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, 0);
            if (!PRTE_FLAG_TEST(nd, PRTE_NODE_NON_USABLE)) {
                PMIX_RETAIN(nd);
                pmix_list_append(allocated_nodes, &nd->super);
            } else {
                nd = NULL;
            }
        } else {
            nd = NULL;
        }
    } else {
        nd = (prte_node_t *) pmix_list_get_last(allocated_nodes);
    }
    for (i = 1; i < prte_node_pool->size; i++) {
        if (NULL != (node = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, i))) {
            /* ignore nodes that are non-usable */
            if (PRTE_FLAG_TEST(node, PRTE_NODE_NON_USABLE)) {
                continue;
            }
            /* ignore nodes that are marked as do-not-use for this mapping */
            if (PRTE_NODE_STATE_DO_NOT_USE == node->state) {
                PMIX_OUTPUT_VERBOSE((10, prte_rmaps_base_framework.framework_output,
                                     "NODE %s IS MARKED NO_USE", node->name));
                /* reset the state so it can be used another time */
                node->state = PRTE_NODE_STATE_UP;
                continue;
            }
            if (PRTE_NODE_STATE_DOWN == node->state) {
                PMIX_OUTPUT_VERBOSE((10, prte_rmaps_base_framework.framework_output,
                                     "NODE %s IS MARKED DOWN", node->name));
                continue;
            }
            if (PRTE_NODE_STATE_NOT_INCLUDED == node->state) {
                PMIX_OUTPUT_VERBOSE((10, prte_rmaps_base_framework.framework_output,
                                     "NODE %s IS MARKED NO_INCLUDE", node->name));
                /* not to be used */
                continue;
            }
            /* if this node wasn't included in the vm (e.g., by -host), ignore it,
             * unless we are mapping prior to launching the vm
             */
            if (NULL == node->daemon && !novm) {
                PMIX_OUTPUT_VERBOSE((10, prte_rmaps_base_framework.framework_output,
                                     "NODE %s HAS NO DAEMON", node->name));
                continue;
            }
            /* retain a copy for our use in case the item gets
             * destructed along the way
             */
            PMIX_RETAIN(node);
            if (initial_map) {
                /* if this is the first app_context we
                 * are getting for an initial map of a job,
                 * then mark all nodes as unmapped
                 */
                PRTE_FLAG_UNSET(node, PRTE_NODE_FLAG_MAPPED);
            }
            if (NULL == nd || NULL == nd->daemon ||
                NULL == node->daemon ||
                nd->daemon->name.rank < node->daemon->name.rank) {
                /* just append to end */
                pmix_list_append(allocated_nodes, &node->super);
                nd = node;
            } else {
                /* starting from end, put this node in daemon-vpid order */
                while (node->daemon->name.rank < nd->daemon->name.rank) {
                    if (pmix_list_get_begin(allocated_nodes) == pmix_list_get_prev(&nd->super)) {
                        /* insert at beginning */
                        pmix_list_prepend(allocated_nodes, &node->super);
                        goto moveon;
                    }
                    nd = (prte_node_t *) pmix_list_get_prev(&nd->super);
                }
                item = pmix_list_get_next(&nd->super);
                if (item == pmix_list_get_end(allocated_nodes)) {
                    /* we are at the end - just append */
                    pmix_list_append(allocated_nodes, &node->super);
                } else {
                    nd = (prte_node_t *) item;
                    pmix_list_insert_pos(allocated_nodes, item, &node->super);
                }
            moveon:
                /* reset us back to the end for the next node */
                nd = (prte_node_t *) pmix_list_get_last(allocated_nodes);
            }
        }
    }

    PMIX_OUTPUT_VERBOSE((5, prte_rmaps_base_framework.framework_output,
                         "%s Starting with %d nodes in list", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                         (int) pmix_list_get_size(allocated_nodes)));

    /** check that anything is here */
    if (0 == pmix_list_get_size(allocated_nodes)) {
        if (!silent) {
            pmix_show_help("help-prte-rmaps-base.txt", "prte-rmaps-base:no-available-resources",
                           true);
        }
        return PRTE_ERR_SILENT;
    }

    /* filter the nodes thru any hostfile and dash-host options */
    PMIX_OUTPUT_VERBOSE((5, prte_rmaps_base_framework.framework_output, "%s Filtering thru apps",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));

    if (PRTE_SUCCESS != (rc = prte_rmaps_base_filter_nodes(app, allocated_nodes, true))
        && PRTE_ERR_TAKE_NEXT_OPTION != rc) {
        PRTE_ERROR_LOG(rc);
        return rc;
    }
    PMIX_OUTPUT_VERBOSE((5, prte_rmaps_base_framework.framework_output,
                         "%s Retained %d nodes in list", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                         (int) pmix_list_get_size(allocated_nodes)));

complete:
    /* if we are mapping debuggers, then they don't count against
     * the allocation */
    if (PRTE_FLAG_TEST(app, PRTE_APP_FLAG_TOOL)) {
        num_slots = INT32_MAX;
        PMIX_LIST_FOREACH_SAFE(node, next, allocated_nodes, prte_node_t)
        {
            if (0 == node->index) {
                if (!prte_hnp_is_allocated ||
                    (PRTE_GET_MAPPING_DIRECTIVE(policy) & PRTE_MAPPING_NO_USE_LOCAL)) {
                    pmix_list_remove_item(allocated_nodes, &node->super);
                    PMIX_RELEASE(node); /* "un-retain" it */
                    continue;
                }
            }
            if (NULL == node->topology || NULL == node->topology->topo) {
                /* cannot use this node - should never happen */
                pmix_list_remove_item(allocated_nodes, &node->super);
                PMIX_RELEASE(node);
                continue;
            }
            /* cache the available CPUs for later */
            hwloc_bitmap_copy(node->jobcache, node->available);
        }
    } else {
        num_slots = 0;
        PMIX_LIST_FOREACH_SAFE(node, next, allocated_nodes, prte_node_t)
        {
            if (NULL == node->topology || NULL == node->topology->topo) {
                PMIX_OUTPUT_VERBOSE((5, prte_rmaps_base_framework.framework_output,
                                     "%s node %s lacks topology",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), node->name));
                /* cannot use this node - should never happen */
                pmix_list_remove_item(allocated_nodes, &node->super);
                PMIX_RELEASE(node);
                continue;
            }
            /* if the hnp was not allocated, or flagged not to be used,
             * then remove it here */
            if (!prte_hnp_is_allocated ||
                (PRTE_GET_MAPPING_DIRECTIVE(policy) & PRTE_MAPPING_NO_USE_LOCAL)) {
                if (0 == node->index) {
                    PMIX_OUTPUT_VERBOSE((5, prte_rmaps_base_framework.framework_output,
                                         "%s node %s HNP not allocated or not to be used",
                                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), node->name));
                    pmix_list_remove_item(allocated_nodes, &node->super);
                    PMIX_RELEASE(node); /* "un-retain" it */
                    continue;
                }
            }
            /** check to see if this node is fully used - remove if so */
            if (0 != node->slots_max && node->slots_inuse >= node->slots_max) {
                PMIX_OUTPUT_VERBOSE((5, prte_rmaps_base_framework.framework_output,
                                     "%s Removing node %s: max %d inuse %d",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), node->name,
                                     node->slots_max, node->slots_inuse));
                pmix_list_remove_item(allocated_nodes, &node->super);
                PMIX_RELEASE(node); /* "un-retain" it */
                continue;
            }
            if (node->slots <= node->slots_inuse &&
                (PRTE_MAPPING_NO_OVERSUBSCRIBE & PRTE_GET_MAPPING_DIRECTIVE(policy))) {
                /* remove the node as fully used */
                PMIX_OUTPUT_VERBOSE((5, prte_rmaps_base_framework.framework_output,
                                     "%s Removing node %s slots %d inuse %d",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), node->name, node->slots,
                                     node->slots_inuse));
                pmix_list_remove_item(allocated_nodes, &node->super);
                PMIX_RELEASE(node); /* "un-retain" it */
                continue;
            }
            if (node->slots > node->slots_inuse) {
                int32_t s;
                /* check for any -host allocations */
                if (prte_get_attribute(&app->attributes, PRTE_APP_DASH_HOST, (void **) &hosts,
                                       PMIX_STRING)) {
                    s = prte_util_dash_host_compute_slots(node, hosts);
                } else {
                    s = node->slots - node->slots_inuse;
                }
                node->slots_available = s;
                /* add the available slots */
                PMIX_OUTPUT_VERBOSE((5, prte_rmaps_base_framework.framework_output,
                                     "%s node %s has %d slots available",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), node->name, s));
                num_slots += s;
                /* cache the available CPUs for later */
                hwloc_bitmap_copy(node->jobcache, node->available);
                continue;
            }
            if (!(PRTE_MAPPING_NO_OVERSUBSCRIBE & PRTE_GET_MAPPING_DIRECTIVE(policy))) {
                /* nothing needed to do here - we don't add slots to the
                 * count as we don't have any available. Just let the mapper
                 * do what it needs to do to meet the request
                 */
                PMIX_OUTPUT_VERBOSE((5, prte_rmaps_base_framework.framework_output,
                                     "%s node %s is fully used, but available for oversubscription",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), node->name));
                /* cache the available CPUs for later */
                hwloc_bitmap_copy(node->jobcache, node->available);
            } else {
                PMIX_OUTPUT_VERBOSE((5, prte_rmaps_base_framework.framework_output,
                                     "%s node %s is fully used and not available for oversubscription: SLOTS %d INUSE %d",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), node->name,
                                     (int)node->slots, (int)node->slots_inuse));
                /* if we cannot use it, remove it from list */
                pmix_list_remove_item(allocated_nodes, &node->super);
                PMIX_RELEASE(node); /* "un-retain" it */
                continue;
            }
        }
    }

    /* Sanity check to make sure we have resources available */
    if (0 == pmix_list_get_size(allocated_nodes)) {
        if (silent) {
            /* let the caller know that the resources exist,
             * but are currently busy
             */
            return PRTE_ERR_RESOURCE_BUSY;
        } else {
            pmix_show_help("help-prte-rmaps-base.txt",
                           "prte-rmaps-base:all-available-resources-used", true);
            return PRTE_ERR_SILENT;
        }
    }

    /* pass back the total number of available slots */
    *total_num_slots = num_slots;

    /* check for prior bookmark */
    prte_rmaps_base_get_starting_point(allocated_nodes, jdata);

    if (4 < pmix_output_get_verbosity(prte_rmaps_base_framework.framework_output)) {
        pmix_output(0, "AVAILABLE NODES FOR MAPPING:");
        for (item = pmix_list_get_first(allocated_nodes);
             item != pmix_list_get_end(allocated_nodes); item = pmix_list_get_next(item)) {
            node = (prte_node_t *) item;
            pmix_output(0, "    node: %s daemon: %s slots_available: %d", node->name,
                        (NULL == node->daemon) ? "NULL" : PRTE_VPID_PRINT(node->daemon->name.rank),
                        node->slots_available);
        }
    }

    return PRTE_SUCCESS;
}

prte_proc_t *prte_rmaps_base_setup_proc(prte_job_t *jdata,
                                        prte_app_idx_t idx,
                                        prte_node_t *node,
                                        hwloc_obj_t obj,
                                        prte_rmaps_options_t *options)
{
    prte_proc_t *proc;
    int rc;
    prte_app_context_t *app;

    proc = PMIX_NEW(prte_proc_t);
    /* set the jobid */
    PMIX_LOAD_NSPACE(proc->name.nspace, jdata->nspace);
    /* flag the proc as ready for launch */
    proc->state = PRTE_PROC_STATE_INIT;
    proc->app_idx = idx;
    app = (prte_app_context_t*)pmix_pointer_array_get_item(jdata->apps, idx);
    if (NULL == app) {
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        PMIX_RELEASE(proc);
        return NULL;
    }
    /* mark the proc as UPDATED so it will be included in the launch */
    PRTE_FLAG_SET(proc, PRTE_PROC_FLAG_UPDATED);
    if (NULL == node->daemon) {
        proc->parent = PMIX_RANK_INVALID;
    } else {
        proc->parent = node->daemon->name.rank;
    }

    // point the proc at its node
    proc->node = node;
    PMIX_RETAIN(node); /* maintain accounting on object */

    /* point the proc to its locale */
    proc->obj = obj;

    /* bind the process so we know which cpus have been taken */
    rc = prte_rmaps_base_bind_proc(jdata, proc, node, obj, options);
    if (PRTE_SUCCESS != rc) {
        PMIX_RELEASE(proc); // releases node to maintain accounting
        return NULL;
    }
    if (0 > (rc = pmix_pointer_array_add(node->procs, (void *) proc))) {
        PRTE_ERROR_LOG(rc);
        PMIX_RELEASE(proc); // releases node to maintain accounting
        return NULL;
    }

    /* if this is a tool app, then it doesn't count against
     * available slots - otherwise, it does */
    if (PRTE_FLAG_TEST(app, PRTE_APP_FLAG_TOOL)) {
        proc->local_rank = 0;
        proc->node_rank = UINT16_MAX;
    } else {
        proc->node_rank = node->num_procs;
        node->num_procs++;
        ++node->slots_inuse;
    }

    /* retain the proc struct so that we correctly track its release */
    PMIX_RETAIN(proc);

    return proc;
}

/*
 * determine the proper starting point for the next mapping operation
 */
void prte_rmaps_base_get_starting_point(pmix_list_t *node_list, prte_job_t *jdata)
{
    prte_node_t *node, *nd1;
    bool first = true;

    /* if a bookmark exists from some prior mapping, set us to start there */
    node = NULL;
    if (NULL != jdata->bookmark) {
        /* find this node on the list */
        PMIX_LIST_FOREACH(nd1, node_list, prte_node_t) {
            if (nd1->index == jdata->bookmark->index) {
                node = nd1;
                break;
            }
            first = false;
        }
    }
    if (NULL == node || first) {
        return;
    }

    PMIX_OUTPUT_VERBOSE((5, prte_rmaps_base_framework.framework_output,
                         "%s Starting bookmark at node %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                         node->name));

    /* put this node at the front of the list */
    pmix_list_remove_item(node_list, &node->super);
    pmix_list_prepend(node_list, &node->super);

    return;
}

int prte_rmaps_base_get_ncpus(prte_node_t *node,
                              hwloc_obj_t obj,
                              prte_rmaps_options_t *options)
{
    int ncpus;

    if (NULL == options->job_cpuset) {
        hwloc_bitmap_copy(prte_rmaps_base.available, node->available);
    } else {
        hwloc_bitmap_and(prte_rmaps_base.available, node->available, options->job_cpuset);
    }
    if (NULL != obj) {
#if HWLOC_API_VERSION < 0x20000
        hwloc_bitmap_and(prte_rmaps_base.available, prte_rmaps_base.available, obj->allowed_cpuset);
#else
        hwloc_bitmap_and(prte_rmaps_base.available, prte_rmaps_base.available, obj->cpuset);
#endif
    }
    if (options->use_hwthreads) {
        ncpus = hwloc_bitmap_weight(prte_rmaps_base.available);
    } else {
        /* if we are treating cores as cpus, then we really
         * want to know how many cores are in this object.
         * hwloc sets a bit for each "pu", so we can't just
         * count bits in this case as there may be more than
         * one hwthread/core. Instead, find the number of cores
         * under the object
         */
        ncpus = hwloc_get_nbobjs_inside_cpuset_by_type(node->topology->topo, prte_rmaps_base.available, HWLOC_OBJ_CORE);
    }

    return ncpus;
}

bool prte_rmaps_base_check_avail(prte_job_t *jdata,
                                 prte_app_context_t *app,
                                 prte_node_t *node,
                                 pmix_list_t *node_list,
                                 hwloc_obj_t obj,
                                 prte_rmaps_options_t *options)
{
    int nprocs;
    bool avail = false;

    pmix_output_verbose(10, prte_rmaps_base_framework.framework_output,
                        "%s get_avail_ncpus: node %s has %d procs on it",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), node->name, node->num_procs);

    if (PRTE_FLAG_TEST(app, PRTE_APP_FLAG_TOOL)) {
        avail = true;
        goto done;
    }

    if (!options->oversubscribe) {
        if (node->slots <= node->slots_inuse) {
            pmix_output_verbose(2, prte_rmaps_base_framework.framework_output,
                                "mca:rmaps: node %s is full - skipping",
                                node->name);
            goto done;
        }
    }
    if (0 != node->slots_max &&
        node->slots_max <= node->slots_inuse) {
        /* cannot use this node - already at max_slots */
        pmix_list_remove_item(node_list, &node->super);
        PMIX_RELEASE(node);
        goto done;
    }

    if (PRTE_BIND_TO_NONE == options->bind) {
        if (NULL != options->job_cpuset) {
            options->target = hwloc_bitmap_dup(options->job_cpuset);
        } else {
            options->target = NULL;
        }
        avail = true;
        goto done;
    }

    options->ncpus = prte_rmaps_base_get_ncpus(node, obj, options);
    /* the available cpus are in the scratch location */
    options->target = hwloc_bitmap_dup(prte_rmaps_base.available);

    nprocs = options->ncpus / options->cpus_per_rank;
    if (options->nprocs < nprocs) {
        avail = true;
    } else if (options->overload) {
        /* doesn't matter how many cpus are in use */
        avail = true;
    } else if (0 < nprocs) {
        options->nprocs = nprocs;
        avail = true;
    }

done:
    /* add this node to the map - do it only once */
    if (avail && !PRTE_FLAG_TEST(node, PRTE_NODE_FLAG_MAPPED)) {
        PRTE_FLAG_SET(node, PRTE_NODE_FLAG_MAPPED);
        PMIX_RETAIN(node);
        pmix_pointer_array_add(jdata->map->nodes, node);
        ++(jdata->map->num_nodes);
        options->nnodes++;  // track #nodes for this app
    }

    return avail;
}

void prte_rmaps_base_get_cpuset(prte_job_t *jdata,
                                prte_node_t *node,
                                prte_rmaps_options_t *options)
{
    PRTE_HIDE_UNUSED_PARAMS(jdata);
    
    if (NULL != options->cpuset) {
        options->job_cpuset = prte_hwloc_base_generate_cpuset(node->topology->topo,
                                                              options->use_hwthreads,
                                                              options->cpuset);
    } else {
        options->job_cpuset = hwloc_bitmap_dup(node->available);
    }
}

int prte_rmaps_base_check_support(prte_job_t *jdata,
                                  prte_node_t *node,
                                  prte_rmaps_options_t *options)
{
    struct hwloc_topology_support *support;

    if (PRTE_FLAG_TEST(jdata, PRTE_JOB_FLAG_TOOL) ||
        PRTE_BIND_TO_NONE == PRTE_GET_BINDING_POLICY(jdata->map->binding)) {
        return PRTE_SUCCESS;
    }

    support = (struct hwloc_topology_support *) hwloc_topology_get_support(node->topology->topo);
    /* check if topology supports cpubind - have to be careful here
     * as Linux doesn't currently support thread-level binding. This
     * may change in the future, though, and it isn't clear how hwloc
     * interprets the current behavior. So check both flags to be sure.
     */
    if (!support->cpubind->set_thisproc_cpubind &&
        !support->cpubind->set_thisthread_cpubind) {
        if (PRTE_BINDING_REQUIRED(jdata->map->binding) &&
            PRTE_BINDING_POLICY_IS_SET(jdata->map->binding)) {
            /* we are required to bind but cannot */
            pmix_show_help("help-prte-rmaps-base.txt", "rmaps:cpubind-not-supported",
                           true, node->name);
            return PRTE_ERR_SILENT;
        }
    }
    /* check if topology supports membind - have to be careful here
     * as hwloc treats this differently than I (at least) would have
     * expected. Per hwloc, Linux memory binding is at the thread,
     * and not process, level. Thus, hwloc sets the "thisproc" flag
     * to "false" on all Linux systems, and uses the "thisthread" flag
     * to indicate binding capability - don't warn if the user didn't
     * specifically request binding
     */
    if (!support->membind->set_thisproc_membind &&
        !support->membind->set_thisthread_membind &&
        PRTE_BINDING_POLICY_IS_SET(jdata->map->binding)) {
        if (PRTE_HWLOC_BASE_MBFA_WARN == prte_hwloc_base_mbfa && !options->membind_warned) {
            pmix_show_help("help-prte-rmaps-base.txt", "rmaps:membind-not-supported", true,
                           node->name);
            options->membind_warned = true;
        } else if (PRTE_HWLOC_BASE_MBFA_ERROR == prte_hwloc_base_mbfa) {
            pmix_show_help("help-prte-rmaps-base.txt", "rmaps:membind-not-supported-fatal",
                           true, node->name);
            return PRTE_ERR_SILENT;
        }
    }
    return PRTE_SUCCESS;
}

int prte_rmaps_base_check_oversubscribed(prte_job_t *jdata,
                                         prte_app_context_t *app,
                                         prte_node_t *node,
                                         prte_rmaps_options_t *options)
{
    if (PRTE_FLAG_TEST(app, PRTE_APP_FLAG_TOOL)) {
        return PRTE_SUCCESS;
    }
    if (!options->oversubscribe &&
        node->slots == node->num_procs) {
        return PRTE_ERR_TAKE_NEXT_OPTION;  // this node is full
    }

    /* not all nodes are equal, so only set oversubscribed for
     * this node if it is in that state
     */
    if (node->slots < (int) node->num_procs) {
        /* flag the node as oversubscribed so that sched-yield gets
         * properly set
         */
        PRTE_FLAG_SET(node, PRTE_NODE_FLAG_OVERSUBSCRIBED);
        PRTE_FLAG_SET(jdata, PRTE_JOB_FLAG_OVERSUBSCRIBED);
        if (options->oversubscribe) {
            return PRTE_SUCCESS;
        }

        /* check for permission */
        if (PRTE_FLAG_TEST(node, PRTE_NODE_FLAG_SLOTS_GIVEN)) {
            /* if we weren't given a directive either way, then we will error out
             * as the #slots were specifically given, either by the host RM or
             * via hostfile/dash-host */
            if (!(PRTE_MAPPING_SUBSCRIBE_GIVEN &
                  PRTE_GET_MAPPING_DIRECTIVE(jdata->map->mapping))) {
                pmix_show_help("help-prte-rmaps-base.txt", "prte-rmaps-base:alloc-error",
                               true, app->num_procs, app->app, prte_process_info.nodename);
                PRTE_UPDATE_EXIT_STATUS(PRTE_ERROR_DEFAULT_EXIT_CODE);
                return PRTE_ERR_SILENT;
            } else if (!options->oversubscribe) {
                /* if we were explicitly told not to oversubscribe, then don't */
                pmix_show_help("help-prte-rmaps-base.txt", "prte-rmaps-base:alloc-error",
                               true, app->num_procs, app->app, prte_process_info.nodename);
                PRTE_UPDATE_EXIT_STATUS(PRTE_ERROR_DEFAULT_EXIT_CODE);
                return PRTE_ERR_SILENT;
            }
        }
    }
    return PRTE_SUCCESS;
}
