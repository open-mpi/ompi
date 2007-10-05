/* -*- C -*-
*
* Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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
/** @file:
*
*/

/*
 * includes
 */
#include "orte_config.h"

#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"

#include "routed_cnos.h"



/*
 * Struct of function pointers that need to be initialized
 */
orte_routed_component_t mca_routed_cnos_component = {
    {
        ORTE_ROUTED_BASE_VERSION_1_0_0,
        
        "cnos", /* MCA module name */
        ORTE_MAJOR_VERSION,  /* MCA module major version */
        ORTE_MINOR_VERSION,  /* MCA module minor version */
        ORTE_RELEASE_VERSION,  /* MCA module release version */
        NULL,  /* module open */
        NULL /* module close */
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    orte_routed_cnos_init    /* component init */
};

/*
 * instantiate globals needed within cnos component
 */

orte_routed_module_t* orte_routed_cnos_init(int *priority)
{
    /* we are the default, so set a low priority so we can be overridden */
    *priority = 50;
    
    return &orte_routed_cnos_module;
}
