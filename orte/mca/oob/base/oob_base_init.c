/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stdio.h>
#include <string.h>

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"

#include "orte/runtime/orte_globals.h"
#include "orte/mca/oob/oob.h"
#include "orte/mca/oob/base/base.h"


OBJ_CLASS_INSTANCE(
    mca_oob_t,
    opal_list_item_t,
    NULL,
    NULL
);


/**
 * Function for selecting one module from all those that are
 * available.
 *
 * Call the init function on all available modules.
 */
int mca_oob_base_init(void)
{
    opal_list_item_t *item;
    mca_base_component_list_item_t *cli;
    mca_oob_base_component_t *component, *s_component;
    mca_oob_t *module;
    mca_oob_t *s_module = NULL;
    int  s_priority = -1;

    /* Traverse the list of available modules; call their init functions. */
    for (item = opal_list_get_first(&orte_oob_base_framework.framework_components);
         item != opal_list_get_end(&orte_oob_base_framework.framework_components);
         item = opal_list_get_next(item)) {
        cli = (mca_base_component_list_item_t *) item;
        component = (mca_oob_base_component_t *) cli->cli_component;

        if (NULL == component->oob_init) {
            opal_output_verbose(10, orte_oob_base_framework.framework_output,
                                "mca_oob_base_init: no init function; ignoring component");
        } else {
            int priority = -1;
            module = component->oob_init(&priority);
            if (NULL != module) {
                if (priority > s_priority) {
                    s_module = module;
                    s_priority = priority;
                    s_component = component;
                } else {
                    if (NULL != module->oob_fini) {
                        module->oob_fini();
                    }
                }
            }
        }
    }
    /* set the global variable to point to the first initialize module */
    if (s_module == NULL) {
        opal_output_verbose(10, orte_oob_base_framework.framework_output,
                            "mca_oob_base_init: no OOB modules available\n");
        /* the oob modules will have printed out an error msg - so be silent here */
        return ORTE_ERR_SILENT;
    }

    mca_oob = *s_module;

    orte_selected_oob_component = strdup(s_component->oob_base.mca_component_name);
    opal_output_verbose(10, orte_oob_base_framework.framework_output,
                        "mca_oob_base_init: %s module selected\n",
                        s_component->oob_base.mca_component_name);
    return ORTE_SUCCESS;
}

