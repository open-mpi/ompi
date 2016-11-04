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
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#include "orte/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/event/event.h"

#include "orte/mca/ras/base/ras_private.h"
#include "orte/mca/ras/base/base.h"


/* NOTE: the RAS does not require a proxy as only the
 * HNP can open the framework in orte_init - non-HNP
 * procs are not allowed to allocate resources
 */

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/ras/base/static-components.h"

/*
 * Global variables
 */
orte_ras_base_t orte_ras_base = {0};

static int ras_register(mca_base_register_flag_t flags)
{
    orte_ras_base.multiplier = 1;
    mca_base_var_register("orte", "ras", "base", "multiplier",
                          "Simulate a larger cluster by launching N daemons/node",
                          MCA_BASE_VAR_TYPE_INT,
                          NULL, 0, 0,
                          OPAL_INFO_LVL_9,
                          MCA_BASE_VAR_SCOPE_READONLY, &orte_ras_base.multiplier);
    return ORTE_SUCCESS;
}

static int orte_ras_base_close(void)
{
    /* Close selected component */
    if (NULL != orte_ras_base.active_module) {
        orte_ras_base.active_module->finalize();
    }

    return mca_base_framework_components_close(&orte_ras_base_framework, NULL);
}

/**
 *  * Function for finding and opening either all MCA components, or the one
 *   * that was specifically requested via a MCA parameter.
 *    */
static int orte_ras_base_open(mca_base_open_flag_t flags)
{
    /* set default flags */
    orte_ras_base.active_module = NULL;
    orte_ras_base.allocation_read = false;
    orte_ras_base.total_slots_alloc = 0;

    /* Open up all available components */
    return mca_base_framework_components_open(&orte_ras_base_framework, flags);
}

MCA_BASE_FRAMEWORK_DECLARE(orte, ras, "ORTE Resource Allocation Subsystem",
                           ras_register, orte_ras_base_open, orte_ras_base_close,
                           mca_ras_base_static_components, 0);
