/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2007-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2019 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "orte/mca/mca.h"
#include "opal/class/opal_bitmap.h"
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_component_repository.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/routed/routed.h"
#include "orte/mca/routed/base/base.h"


/* The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct. */
#include "orte/mca/routed/base/static-components.h"

orte_routed_base_t orte_routed_base = {0};
orte_routed_module_t orte_routed = {0};

static int orte_routed_base_open(mca_base_open_flag_t flags)
{
    /* start with routing DISABLED */
    orte_routed_base.routing_enabled = false;

    /* Open up all available components */
    return mca_base_framework_components_open(&orte_routed_base_framework, flags);
}

static int orte_routed_base_close(void)
{
    orte_routed_base.routing_enabled = false;
    if (NULL != orte_routed.finalize) {
        orte_routed.finalize();
    }
    return mca_base_framework_components_close(&orte_routed_base_framework, NULL);
}

MCA_BASE_FRAMEWORK_DECLARE(orte, routed, "ORTE Message Routing Subsystem", NULL,
                           orte_routed_base_open, orte_routed_base_close,
                           mca_routed_base_static_components, 0);

int orte_routed_base_select(void)
{
    orte_routed_component_t *best_component = NULL;
    orte_routed_module_t *best_module = NULL;

    /*
     * Select the best component
     */
    if( OPAL_SUCCESS != mca_base_select("routed", orte_routed_base_framework.framework_output,
                                        &orte_routed_base_framework.framework_components,
                                        (mca_base_module_t **) &best_module,
                                        (mca_base_component_t **) &best_component, NULL) ) {
        /* This will only happen if no component was selected */
        /* If we didn't find one to select, that is an error */
        return ORTE_ERROR;
    }

    /* Save the winner */
    orte_routed = *best_module;
    if (NULL != orte_routed.initialize) {
        orte_routed.initialize();
    }
    return ORTE_SUCCESS;
}

static void construct(orte_routed_tree_t *rt)
{
    rt->vpid = ORTE_VPID_INVALID;
    OBJ_CONSTRUCT(&rt->relatives, opal_bitmap_t);
}
static void destruct(orte_routed_tree_t *rt)
{
    OBJ_DESTRUCT(&rt->relatives);
}
OBJ_CLASS_INSTANCE(orte_routed_tree_t,
                   opal_list_item_t,
                   construct, destruct);
