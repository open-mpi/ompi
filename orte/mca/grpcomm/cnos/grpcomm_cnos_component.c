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
* The Open MPI Name Server
*
* The Open MPI Name Server provides unique name ranges for processes
* within the universe. Each universe will have one name server
* running within the seed daemon.  This is done to prevent the
* inadvertent duplication of names.
*/

/*
 * includes
 */
#include "orte_config.h"

#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "opal/threads/mutex.h"
#include "opal/class/opal_list.h"
#include "opal/util/output.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"

#include "grpcomm_cnos.h"



/*
 * Struct of function pointers that need to be initialized
 */
orte_grpcomm_base_component_t mca_grpcomm_cnos_component = {
    {
        ORTE_GRPCOMM_BASE_VERSION_2_0_0,
        
        "cnos", /* MCA module name */
        ORTE_MAJOR_VERSION,  /* MCA module major version */
        ORTE_MINOR_VERSION,  /* MCA module minor version */
        ORTE_RELEASE_VERSION,  /* MCA module release version */
        orte_grpcomm_cnos_open,  /* module open */
        orte_grpcomm_cnos_close /* module close */
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    orte_grpcomm_cnos_init,    /* component init */
    orte_grpcomm_cnos_finalize /* component shutdown */
};

/*
 * instantiate globals needed within cnos component
 */

/* Open the component */
int orte_grpcomm_cnos_open(void)
{
    int value;
    char *mode;

    return ORTE_SUCCESS;
}

/* Close the component */
int orte_grpcomm_cnos_close(void)
{
    return ORTE_SUCCESS;
}

orte_grpcomm_base_module_t* orte_grpcomm_cnos_init(int *priority)
{
    /* we are the default, so set a low priority so we can be overridden */
    *priority = 50;
    
    return &orte_grpcomm_cnos_module;
}

/*
 * finalize routine
 */
int orte_grpcomm_cnos_finalize(void)
{
    return ORTE_SUCCESS;
}
