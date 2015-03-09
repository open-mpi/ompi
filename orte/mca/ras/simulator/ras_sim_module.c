/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC. All rights reserved
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <unistd.h>
#include <string.h>
#include <ctype.h>

#include "opal/class/opal_list.h"
#include "opal/mca/hwloc/hwloc.h"
#include "opal/util/argv.h"

#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"

#include "ras_sim.h"


/*
 * Local functions
 */
static int allocate(orte_job_t *jdata, opal_list_t *nodes);
static int finalize(void);


/*
 * Global variable
 */
orte_ras_base_module_t orte_ras_sim_module = {
    NULL,
    allocate,
    NULL,
    finalize
};

static int allocate(orte_job_t *jdata, opal_list_t *nodes)
{
    int i, n, val, dig, num_nodes;
    orte_node_t *node;
#if OPAL_HAVE_HWLOC
    orte_topology_t *t;
    hwloc_topology_t topo;
    hwloc_obj_t obj;
    unsigned j, k;
    struct hwloc_topology_support *support;
    char **files=NULL;
    bool use_local_topology = false;
#endif
    char **node_cnt=NULL;
    char **slot_cnt=NULL;
    char **max_slot_cnt=NULL;
    char *tmp;
    char prefix[6];

    node_cnt = opal_argv_split(mca_ras_simulator_component.num_nodes, ',');
    slot_cnt = opal_argv_split(mca_ras_simulator_component.slots, ',');
    max_slot_cnt = opal_argv_split(mca_ras_simulator_component.slots_max, ',');

    /* backfill the slot_cnt as reqd so we don't have to
     * specify slot_cnt for each set of nodes - we'll set
     * */
    tmp = slot_cnt[opal_argv_count(slot_cnt)-1];
    for (n=opal_argv_count(slot_cnt); n < opal_argv_count(node_cnt); n++) {
        opal_argv_append_nosize(&slot_cnt, tmp);
    }
    /* backfill the max_slot_cnt as reqd */
    tmp = max_slot_cnt[opal_argv_count(slot_cnt)-1];
    for (n=opal_argv_count(max_slot_cnt); n < opal_argv_count(max_slot_cnt); n++) {
        opal_argv_append_nosize(&max_slot_cnt, tmp);
    }

#if OPAL_HAVE_HWLOC
    if (NULL == mca_ras_simulator_component.topofiles) {
        /* use our topology */
        use_local_topology = true;
    } else {
        files = opal_argv_split(mca_ras_simulator_component.topofiles, ',');
        if (opal_argv_count(files) != opal_argv_count(node_cnt)) {
            orte_show_help("help-ras-base.txt", "ras-sim:mismatch", true);
            goto error_silent;
        }
    }
#else
    /* If we don't have hwloc and hwloc files were specified, then
       error out (because we can't deliver that functionality) */
    if (NULL == mca_ras_simulator_component.topofiles) {
        orte_show_help("help-ras-simulator.txt", 
                       "no hwloc support for topofiles", true);
        goto error_silent;
    }
#endif

    /* setup the prefix to the node names */
    snprintf(prefix, 6, "nodeA");

    /* process the request */
    for (n=0; NULL != node_cnt[n]; n++) {
        num_nodes = strtol(node_cnt[n], NULL, 10);

        /* get number of digits */
        val = num_nodes;
        for (dig=0; 0 != val; dig++) {
            val /= 10;
        }

        /* set the prefix for this group of nodes */
        prefix[4] += n;

        /* check for topology */
#if OPAL_HAVE_HWLOC
        if (use_local_topology) {
            /* use our topology */
            topo = opal_hwloc_topology;
        } else {
            if (0 != hwloc_topology_init(&topo)) {
                orte_show_help("help-ras-simulator.txt", 
                               "hwloc API fail", true, 
                               __FILE__, __LINE__, "hwloc_topology_init");
                goto error_silent;
            }
            if (0 != hwloc_topology_set_xml(topo, files[n])) {
                orte_show_help("help-ras-simulator.txt", 
                               "hwloc failed to load xml", true, files[n]);
                hwloc_topology_destroy(topo);
                goto error_silent;
            }
            /* since we are loading this from an external source, we have to
             * explicitly set a flag so hwloc sets things up correctly
             */
            if (0 != hwloc_topology_set_flags(topo, HWLOC_TOPOLOGY_FLAG_IS_THISSYSTEM)) {
                orte_show_help("help-ras-simulator.txt", 
                               "hwloc API fail", true, 
                               __FILE__, __LINE__, "hwloc_topology_set_flags");
                hwloc_topology_destroy(topo);
                goto error_silent;
            }
            if (0 != hwloc_topology_load(topo)) {
                orte_show_help("help-ras-simulator.txt", 
                               "hwloc API fail", true, 
                               __FILE__, __LINE__, "hwloc_topology_load");
                hwloc_topology_destroy(topo);
                goto error_silent;
            }
            /* remove the hostname from the topology. Unfortunately, hwloc
             * decided to add the source hostname to the "topology", thus
             * rendering it unusable as a pure topological description. So
             * we remove that information here.
             */
            obj = hwloc_get_root_obj(topo);
            for (k=0; k < obj->infos_count; k++) {
                if (NULL == obj->infos[k].name ||
                    NULL == obj->infos[k].value) {
                    continue;
                }
                if (0 == strncmp(obj->infos[k].name, "HostName", strlen("HostName"))) {
                    free(obj->infos[k].name);
                    free(obj->infos[k].value);
                    /* left justify the array */
                    for (j=k; j < obj->infos_count-1; j++) {
                        obj->infos[j] = obj->infos[j+1];
                    }
                    obj->infos[obj->infos_count-1].name = NULL;
                    obj->infos[obj->infos_count-1].value = NULL;
                    obj->infos_count--;
                    break;
                }
            }
            /* unfortunately, hwloc does not include support info in its
             * xml output :-(( To aid in debugging, we set it here
             */
            support = (struct hwloc_topology_support*)hwloc_topology_get_support(topo);
            support->cpubind->set_thisproc_cpubind = mca_ras_simulator_component.have_cpubind;
            support->membind->set_thisproc_membind = mca_ras_simulator_component.have_membind;
            /* add it to our array */
            t = OBJ_NEW(orte_topology_t);
            t->topo = topo;
            t->sig = opal_hwloc_base_get_topo_signature(topo);
            opal_pointer_array_add(orte_node_topologies, t);
        }
#endif

        for (i=0; i < num_nodes; i++) {
            node = OBJ_NEW(orte_node_t);
            asprintf(&node->name, "%s%0*d", prefix, dig, i);
            node->state = ORTE_NODE_STATE_UP;
            node->slots_inuse = 0;
            node->slots_max = (NULL == max_slot_cnt[n] ? 0 : atoi(max_slot_cnt[n]));
            node->slots = (NULL == slot_cnt[n] ? 0 : atoi(slot_cnt[n]));
#if OPAL_HAVE_HWLOC
            node->topology = topo;
#endif
            opal_output_verbose(1, orte_ras_base_framework.framework_output,
                                "Created Node <%10s> [%3d : %3d]",
                                node->name, node->slots, node->slots_max);
            opal_list_append(nodes, &node->super);
        }
    }

    /* record the number of allocated nodes */
    orte_num_allocated_nodes = opal_list_get_size(nodes);

    if (NULL != max_slot_cnt) {
        opal_argv_free(max_slot_cnt);
    }
    if (NULL != slot_cnt) {
        opal_argv_free(slot_cnt);
    }
    if (NULL != node_cnt) {
        opal_argv_free(node_cnt);
    }

    return ORTE_SUCCESS;

error_silent:
    if (NULL != max_slot_cnt) {
        opal_argv_free(max_slot_cnt);
    }
    if (NULL != slot_cnt) {
        opal_argv_free(slot_cnt);
    }
    if (NULL != node_cnt) {
        opal_argv_free(node_cnt);
    }
    return ORTE_ERR_SILENT;

}

/*
 * There's really nothing to do here
 */
static int finalize(void)
{
    return ORTE_SUCCESS;
}
