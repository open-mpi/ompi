/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
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

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/if/if.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/ras/base/ras_private.h"
#include "ras_sim.h"

/*
 * Local functions
 */
static int ras_sim_open(void);
static int ras_sim_component_query(mca_base_module_t **module, int *priority);


orte_ras_sim_component_t mca_ras_simulator_component = {
    {
        /* First, the mca_base_component_t struct containing meta
           information about the component itself */

        {
            ORTE_RAS_BASE_VERSION_2_0_0,
        
            /* Component name and version */
            "simulator",
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,
        
            /* Component open and close functions */
            ras_sim_open,
            NULL,
            ras_sim_component_query
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};


static int ras_sim_open(void)
{
    mca_base_param_reg_int(&mca_ras_simulator_component.super.base_version,
                           "num_nodes",
                           "Number of nodes to simulate",
                           false, false, 0, &mca_ras_simulator_component.num_nodes);
    mca_base_param_reg_int(&mca_ras_simulator_component.super.base_version,
                           "slots",
                           "Number of slots on each node to simulate",
                           false, false, 1, &mca_ras_simulator_component.slots);
    mca_base_param_reg_int(&mca_ras_simulator_component.super.base_version,
                           "max_slots",
                           "Number of max slots on each node to simulate",
                           false, false, 0, &mca_ras_simulator_component.slots_max);
#if OPAL_HAVE_HWLOC
    mca_base_param_reg_string(&mca_ras_simulator_component.super.base_version,
                              "topo_file",
                              "File containing xml topology description for simulated nodes",
                              false, false, NULL, &mca_ras_simulator_component.topofile);
#endif

    return ORTE_SUCCESS;
}


static int ras_sim_component_query(mca_base_module_t **module, int *priority)
{
    if (0 < mca_ras_simulator_component.num_nodes) {
        *module = (mca_base_module_t *) &orte_ras_sim_module;
        *priority = 1000;
        /* cannot launch simulated nodes or resolve their names to addresses */
        orte_do_not_launch = true;
        opal_if_do_not_resolve = true;
        return ORTE_SUCCESS;
    }

    /* Sadly, no */
    *module = NULL;
    *priority = 0;
    return ORTE_ERROR;
}
