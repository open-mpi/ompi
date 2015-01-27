/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/types.h"
#include "opal/types.h"

#include "opal/util/show_help.h"

#include "orte/mca/schizo/schizo.h"
#include "schizo_ompi.h"

static int component_query(mca_base_module_t **module, int *priority);

/*
 * Struct of function pointers and all that to let us be initialized
 */
orte_schizo_base_component_t mca_schizo_ompi_component = {
    {
        MCA_SCHIZO_BASE_VERSION_1_0_0,
        "ompi", /* MCA module name */
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,
        NULL,  /* component open */
        NULL, /* component close */
        component_query, /* component query */
        NULL, /* component register */
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};

static int component_query(mca_base_module_t **module, int *priority)
{
    *module = (mca_base_module_t*)&orte_schizo_ompi_module;
    *priority = 1;
    return ORTE_SUCCESS;
}

