/*
 * Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2012      Los Alamos National Security, LLC. All rights reserved
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "prte_config.h"
#include "constants.h"
#include "types.h"

#include <ctype.h>
#include <string.h>
#include <unistd.h>

#include "src/class/pmix_list.h"
#include "src/hwloc/hwloc-internal.h"
#include "src/util/pmix_argv.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/runtime/prte_globals.h"
#include "src/util/pmix_show_help.h"

#include "ras_sim.h"

/*
 * Local functions
 */
static int allocate(prte_job_t *jdata, pmix_list_t *nodes);
static int finalize(void);

/*
 * Global variable
 */
prte_ras_base_module_t prte_ras_sim_module = {NULL, allocate, NULL, finalize};

static int allocate(prte_job_t *jdata, pmix_list_t *nodes)
{
    int i, n, val, dig, num_nodes, nslots;
    prte_node_t *node;
    prte_topology_t *t;
    hwloc_topology_t topo;
    hwloc_obj_t obj;
    char **node_cnt = NULL;
    char **slot_cnt = NULL;
    char **max_slot_cnt = NULL;
    char *tmp, *job_cpuset = NULL;
    char prefix[6];
    bool use_hwthread_cpus = false;
    hwloc_cpuset_t available;

    node_cnt = PMIX_ARGV_SPLIT_COMPAT(prte_mca_ras_simulator_component.num_nodes, ',');
    num_nodes = PMIX_ARGV_COUNT_COMPAT(node_cnt);

    if (NULL != prte_mca_ras_simulator_component.slots) {
        slot_cnt = PMIX_ARGV_SPLIT_COMPAT(prte_mca_ras_simulator_component.slots, ',');
        /* if they didn't provide a slot count for each node, then
         * backfill the slot_cnt so every node has a cnt */
        nslots = PMIX_ARGV_COUNT_COMPAT(slot_cnt);
        if (nslots < num_nodes) {
            // take the last one given and extend it to cover remaining nodes
            tmp = slot_cnt[nslots - 1];
            for (n = nslots; n < num_nodes; n++) {
                PMIX_ARGV_APPEND_NOSIZE_COMPAT(&slot_cnt, tmp);
            }
        }
    }
    if (NULL != prte_mca_ras_simulator_component.slots_max) {
        max_slot_cnt = PMIX_ARGV_SPLIT_COMPAT(prte_mca_ras_simulator_component.slots_max, ',');
        /* if they didn't provide a max slot count for each node, then
         * backfill the slot_cnt so every node has a cnt */
        nslots = PMIX_ARGV_COUNT_COMPAT(max_slot_cnt);
         if (nslots < num_nodes) {
            // take the last one given and extend it to cover remaining nodes
            tmp = max_slot_cnt[nslots - 1];
            for (n = nslots; n < num_nodes; n++) {
                PMIX_ARGV_APPEND_NOSIZE_COMPAT(&max_slot_cnt, tmp);
            }
        }
    }

    /* setup the prefix to the node names */
    snprintf(prefix, 6, "nodeA");

    /* see if this job has a "soft" cgroup assignment */
    job_cpuset = NULL;
    if (!prte_get_attribute(&jdata->attributes, PRTE_JOB_CPUSET, (void **) &job_cpuset, PMIX_STRING)) {
        job_cpuset = NULL;
    }

    /* see if they want are using hwthreads as cpus */
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_HWT_CPUS, NULL, PMIX_BOOL)) {
        use_hwthread_cpus = true;
    } else {
        use_hwthread_cpus = false;
    }

    /* use our topology */
    t = (prte_topology_t *) pmix_pointer_array_get_item(prte_node_topologies, 0);
    if (NULL == t) {
        return PRTE_ERR_NOT_FOUND;
    }
    topo = t->topo;
    if (NULL != job_cpuset) {
        available = prte_hwloc_base_generate_cpuset(topo, use_hwthread_cpus, job_cpuset);
    } else {
        available = prte_hwloc_base_filter_cpus(topo);
    }

    /* process the request */
    for (n = 0; NULL != node_cnt[n]; n++) {
        num_nodes = strtol(node_cnt[n], NULL, 10);

        /* get number of digits */
        val = num_nodes;
        for (dig = 0; 0 != val; dig++) {
            val /= 10;
        }

        /* set the prefix for this group of nodes */
        prefix[4] += n;

        for (i = 0; i < num_nodes; i++) {
            node = PMIX_NEW(prte_node_t);
            pmix_asprintf(&node->name, "%s%0*d", prefix, dig, i);
            node->state = PRTE_NODE_STATE_UP;
            node->slots_inuse = 0;
            if (NULL == slot_cnt || NULL == slot_cnt[n]) {
                obj = hwloc_get_root_obj(t->topo);
                node->slots = prte_hwloc_base_get_npus(t->topo, use_hwthread_cpus, available, obj);
            } else {
                node->slots = strtol(slot_cnt[n], NULL, 10);
            }
            if (NULL == max_slot_cnt || NULL == max_slot_cnt[n]) {
                obj = hwloc_get_root_obj(t->topo);
                node->slots_max = prte_hwloc_base_get_npus(t->topo, use_hwthread_cpus, available, obj);
            } else {
                node->slots_max = strtol(max_slot_cnt[n], NULL, 10);
            }
            PMIX_RETAIN(t);
            node->topology = t;
            pmix_output_verbose(1, prte_ras_base_framework.framework_output,
                                "Created Node <%10s> [%3d : %3d]", node->name, node->slots,
                                node->slots_max);
            node->available = hwloc_bitmap_dup(available);
            pmix_list_append(nodes, &node->super);
        }
    }
    hwloc_bitmap_free(available);

    /* record the number of allocated nodes */
    prte_num_allocated_nodes = pmix_list_get_size(nodes);

    // ensure we do not attempt to launch this job
    prte_set_attribute(&jdata->attributes, PRTE_JOB_DO_NOT_LAUNCH, PRTE_ATTR_GLOBAL,
                       NULL, PMIX_BOOL);

    if (NULL != max_slot_cnt) {
        PMIX_ARGV_FREE_COMPAT(max_slot_cnt);
    }
    if (NULL != slot_cnt) {
        PMIX_ARGV_FREE_COMPAT(slot_cnt);
    }
    if (NULL != node_cnt) {
        PMIX_ARGV_FREE_COMPAT(node_cnt);
    }
    if (NULL != job_cpuset) {
        free(job_cpuset);
    }
    return PRTE_SUCCESS;
}

/*
 * There's really nothing to do here
 */
static int finalize(void)
{
    return PRTE_SUCCESS;
}
