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
    int i, val, dig;
    orte_node_t *node;

    /* get number of digits */
    val = mca_ras_simulator_component.num_nodes;
    for (dig=0; 0 != val; dig++) {
        val /= 10;
    }

    for (i=0; i < mca_ras_simulator_component.num_nodes; i++) {
        node = OBJ_NEW(orte_node_t);
        asprintf(&node->name, "node%0*d", dig, i);
        node->state = ORTE_NODE_STATE_UP;
        node->slots_inuse = 0;
        node->slots_max = mca_ras_simulator_component.slots_max;
        node->slots = mca_ras_simulator_component.slots;
#if OPAL_HAVE_HWLOC
        node->topology = opal_hwloc_topology;
#endif
        opal_list_append(nodes, &node->super);
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
