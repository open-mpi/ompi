/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
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

#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"

#include "ras_sim.h"


/*
 * Local functions
 */
static int allocate(opal_list_t *nodes);
static int finalize(void);


/*
 * Global variable
 */
orte_ras_base_module_t orte_ras_sim_module = {
    allocate,
    finalize
};

static int allocate(opal_list_t *nodes)
{
    int i, n, val, dig, num_nodes;
    orte_node_t *node;
#if OPAL_HAVE_HWLOC
    hwloc_topology_t topo;
    hwloc_obj_t obj;
    unsigned j, k;
    struct hwloc_topology_support *support;
    char **files=NULL;
    bool use_local_topology = false;
#endif
    char **node_cnt=NULL;

    node_cnt = opal_argv_split(mca_ras_simulator_component.num_nodes, ',');

#if OPAL_HAVE_HWLOC
    if (NULL == mca_ras_simulator_component.topofiles) {
        /* use our topology */
        use_local_topology = true;
        if (1 != opal_argv_count(node_cnt)) {
            orte_show_help("help-ras-base.txt", "ras-sim:mismatch", true);
            return ORTE_ERR_SILENT;
        }
    } else {
        files = opal_argv_split(mca_ras_simulator_component.topofiles, ',');
        if (opal_argv_count(files) != opal_argv_count(node_cnt)) {
            orte_show_help("help-ras-base.txt", "ras-sim:mismatch", true);
            return ORTE_ERR_SILENT;
        }
    }

#endif

    /* count the total number of nodes */
    val = 0;
    for (n=0; NULL != node_cnt[n]; n++) {
        val += strtol(node_cnt[n], NULL, 10);
    }
    /* get number of digits */
    for (dig=0; 0 != val; dig++) {
        val /= 10;
    }

    /* process the request */
    val = 0;
    for (n=0; NULL != node_cnt[n]; n++) {
        num_nodes = strtol(node_cnt[n], NULL, 10);

        /* check for topology */
#if OPAL_HAVE_HWLOC
        if (use_local_topology) {
            /* use our topology */
            topo = opal_hwloc_topology;
        } else {
            if (0 != hwloc_topology_init(&topo)) {
                return ORTE_ERROR;
            }
            if (0 != hwloc_topology_set_xml(topo, files[n])) {
                hwloc_topology_destroy(topo);
                return ORTE_ERROR;
            }
            /* since we are loading this from an external source, we have to
             * explicitly set a flag so hwloc sets things up correctly
             */
            if (0 != hwloc_topology_set_flags(topo, HWLOC_TOPOLOGY_FLAG_IS_THISSYSTEM)) {
                hwloc_topology_destroy(topo);
                return ORTE_ERROR;
            }
            if (0 != hwloc_topology_load(topo)) {
                hwloc_topology_destroy(topo);
                return ORTE_ERROR;
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
            opal_pointer_array_add(orte_node_topologies, topo);
        }
#endif

        for (i=0; i < num_nodes; i++) {
            node = OBJ_NEW(orte_node_t);
            asprintf(&node->name, "node%0*d", dig, val++);
            node->state = ORTE_NODE_STATE_UP;
            node->slots_inuse = 0;
            node->slots_max = mca_ras_simulator_component.slots_max;
            node->slots = mca_ras_simulator_component.slots;
#if OPAL_HAVE_HWLOC
            node->topology = topo;
#endif
            opal_list_append(nodes, &node->super);
        }
    }

    /* record the number of allocated nodes */
    orte_num_allocated_nodes = opal_list_get_size(nodes);

    return ORTE_SUCCESS;
}

/*
 * There's really nothing to do here
 */
static int finalize(void)
{
    return ORTE_SUCCESS;
}
