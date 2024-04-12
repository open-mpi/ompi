/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2017 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#include <string.h>

#include "src/util/pmix_argv.h"
#include "src/util/pmix_if.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/rmaps/base/base.h"
#include "src/runtime/prte_globals.h"
#include "src/util/name_fns.h"
#include "src/util/pmix_net.h"
#include "src/util/proc_info.h"

#include "src/mca/ras/base/ras_private.h"

/*
 * Add the specified node definitions to the global data store
 * NOTE: this removes all items from the list!
 */
int prte_ras_base_node_insert(pmix_list_t *nodes, prte_job_t *jdata)
{
    pmix_list_item_t *item;
    int32_t num_nodes;
    int rc, i;
    prte_node_t *node, *hnp_node, *nptr;
    bool skiphnp = false;
    prte_attribute_t *kv;
    prte_proc_t *daemon;
    prte_job_t *djob;

    /* get the number of nodes */
    num_nodes = (int32_t) pmix_list_get_size(nodes);
    if (0 == num_nodes) {
        return PRTE_SUCCESS; /* nothing to do */
    }

    PMIX_OUTPUT_VERBOSE((5, prte_ras_base_framework.framework_output,
                         "%s ras:base:node_insert inserting %ld nodes",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), (long) num_nodes));

    /* mark the job as being a large-cluster sim if that was requested */
    if (1 < prte_ras_base.multiplier) {
        prte_set_attribute(&jdata->attributes, PRTE_JOB_MULTI_DAEMON_SIM, PRTE_ATTR_GLOBAL, NULL,
                           PMIX_BOOL);
    }

    /* set the size of the global array - this helps minimize time
     * spent doing realloc's
     */
    if (PRTE_SUCCESS
        != (rc = pmix_pointer_array_set_size(prte_node_pool,
                                             num_nodes * prte_ras_base.multiplier))) {
        PRTE_ERROR_LOG(rc);
        return rc;
    }

    /* if we are not launching, get the daemon job */
    djob = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);

    /* get the hnp node's info */
    hnp_node = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, 0);

    if (prte_ras_base.launch_orted_on_hn) {
        if (NULL != hnp_node) {
            PMIX_LIST_FOREACH(node, nodes, prte_node_t)
            {
                if (prte_check_host_is_local(node->name)) {
                    prte_hnp_is_allocated = true;
                    break;
                }
            }
            if (prte_hnp_is_allocated
                && !(PRTE_GET_MAPPING_DIRECTIVE(prte_rmaps_base.mapping)
                     & PRTE_MAPPING_NO_USE_LOCAL)) {
                if (NULL != hnp_node->name) {
                    free(hnp_node->name);
                }
                hnp_node->name = strdup("prte");
                skiphnp = true;
                PRTE_SET_MAPPING_DIRECTIVE(prte_rmaps_base.mapping, PRTE_MAPPING_NO_USE_LOCAL);
                PRTE_FLAG_SET(hnp_node,
                              PRTE_NODE_NON_USABLE); // leave this node out of mapping operations
            }
        }
    }

    /* cycle through the list */
    while (NULL != (item = pmix_list_remove_first(nodes))) {
        node = (prte_node_t *) item;

        /* the HNP had to already enter its node on the array - that entry is in the
         * first position since it is the first one entered. We need to check to see
         * if this node is the same as the HNP's node so we don't double-enter it
         */
        if (!skiphnp && NULL != hnp_node && prte_check_host_is_local(node->name)) {
            PMIX_OUTPUT_VERBOSE((5, prte_ras_base_framework.framework_output,
                                 "%s ras:base:node_insert updating HNP [%s] info to %ld slots",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), node->name,
                                 (long) node->slots));

            /* flag that hnp has been allocated */
            prte_hnp_is_allocated = true;
            /* update the total slots in the job */
            prte_ras_base.total_slots_alloc += node->slots;
            /* copy the allocation data to that node's info */
            hnp_node->slots = node->slots;
            hnp_node->slots_max = node->slots_max;
            /* copy across any attributes */
            PMIX_LIST_FOREACH(kv, &node->attributes, prte_attribute_t)
            {
                prte_set_attribute(&node->attributes, kv->key,
                                   PRTE_ATTR_LOCAL,
                                   &kv->data, kv->data.type);
            }
            if (prte_managed_allocation || PRTE_FLAG_TEST(node, PRTE_NODE_FLAG_SLOTS_GIVEN)) {
                /* the slots are always treated as sacred
                 * in managed allocations
                 */
                PRTE_FLAG_SET(hnp_node, PRTE_NODE_FLAG_SLOTS_GIVEN);
            } else {
                PRTE_FLAG_UNSET(hnp_node, PRTE_NODE_FLAG_SLOTS_GIVEN);
            }
            /* if the node name is different, store it as an alias */
            PMIX_ARGV_APPEND_UNIQUE_COMPAT(&hnp_node->aliases, node->name);
            if (NULL != node->rawname) {
                if (NULL != hnp_node->rawname) {
                    free(hnp_node->rawname);
                }
                hnp_node->rawname = strdup(node->rawname);
            }
            /* don't keep duplicate copy */
            PMIX_RELEASE(node);
            /* create copies, if required */
            for (i = 1; i < prte_ras_base.multiplier; i++) {
                rc = prte_node_copy(&node, hnp_node);
                if (PRTE_SUCCESS != rc) {
                    return rc;
                }
                PRTE_FLAG_UNSET(node, PRTE_NODE_FLAG_DAEMON_LAUNCHED);
                node->index = pmix_pointer_array_add(prte_node_pool, node);
            }
        } else {
            /* insert the object onto the prte_nodes global array */
            PMIX_OUTPUT_VERBOSE((5, prte_ras_base_framework.framework_output,
                                 "%s ras:base:node_insert node %s slots %d",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                 (NULL == node->name) ? "NULL" : node->name, node->slots));
            if (prte_managed_allocation) {
                /* the slots are always treated as sacred
                 * in managed allocations
                 */
                PRTE_FLAG_SET(node, PRTE_NODE_FLAG_SLOTS_GIVEN);
            }
            /* insert it into the array */
            node->index = pmix_pointer_array_add(prte_node_pool, (void *) node);
            if (PRTE_SUCCESS > (rc = node->index)) {
                PRTE_ERROR_LOG(rc);
                return rc;
            }
            if (prte_get_attribute(&djob->attributes, PRTE_JOB_DO_NOT_LAUNCH, NULL, PMIX_BOOL)) {
                /* create a daemon for this node since we won't be launching
                 * and the mapper needs to see a daemon - this is used solely
                 * for testing the mappers */
                daemon = PMIX_NEW(prte_proc_t);
                PMIX_LOAD_PROCID(&daemon->name, PRTE_PROC_MY_NAME->nspace, node->index);
                daemon->state = PRTE_PROC_STATE_RUNNING;
                PMIX_RETAIN(node);
                daemon->node = node;
                pmix_pointer_array_set_item(djob->procs, daemon->name.rank, daemon);
                djob->num_procs++;
                PMIX_RETAIN(daemon);
                node->daemon = daemon;
            }
            /* update the total slots in the job */
            prte_ras_base.total_slots_alloc += node->slots;
            /* check if we have fqdn names in the allocation */
            if (!pmix_net_isaddr(node->name) && NULL != strchr(node->name, '.')) {
                prte_have_fqdn_allocation = true;
            }
            /* duplicate the node if requested */
            for (i = 1; i < prte_ras_base.multiplier; i++) {
                rc = prte_node_copy(&nptr, node);
                if (PRTE_SUCCESS != rc) {
                    return rc;
                }
                nptr->index = pmix_pointer_array_add(prte_node_pool, nptr);
            }
        }
    }

    return PRTE_SUCCESS;
}
