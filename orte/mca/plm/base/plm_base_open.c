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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#if !ORTE_DISABLE_FULL_SUPPORT

#include "opal/util/argv.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/show_help.h"

#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/plm_private.h"

#endif

#include "orte/mca/plm/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "orte/mca/plm/base/static-components.h"

#if ORTE_DISABLE_FULL_SUPPORT
/* have to include a bogus function here so that
 * the build system sees at least one function
 * in the library
 */
int orte_plm_base_open(void)
{
    return ORTE_SUCCESS;
}

#else


/*
 * Global public variables
 */
orte_plm_base_t orte_plm_base;

/*
 * Global variables for use within PLM frameworks
 */
orte_plm_globals_t orte_plm_globals;

/*
 * The default module
 */
orte_plm_base_module_t orte_plm = {
    orte_plm_proxy_init,
    NULL,   /* cannot set hnp name in a proxy */
    orte_plm_proxy_spawn,
    NULL,   /* cannot remotely spawn by default */
    NULL,   /* cannot terminate job from a proxy */
    NULL,   /* cannot terminate orteds from a proxy */
    NULL,   /* cannot signal job from a proxy */
    orte_plm_proxy_finalize
};


/**
 * Function for finding and opening either all MCA modules, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_plm_base_open(void)
{
    /* Debugging / verbose output.  Always have stream open, with
       verbose set by the mca open system... */
    orte_plm_globals.output = opal_output_open(NULL);
    
    /* init selected to be false */
    orte_plm_base.selected = false;

    /* initialize the condition variables for orted comm */
    OBJ_CONSTRUCT(&orte_plm_globals.orted_cmd_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&orte_plm_globals.orted_cmd_cond, opal_condition_t);
    
    /* init the next jobid */
    orte_plm_globals.next_jobid = 0;
    
    /* Open up all the components that we can find */

    if (ORTE_SUCCESS != 
        mca_base_components_open("plm", orte_plm_globals.output,
                                 mca_plm_base_static_components, 
                                 &orte_plm_base.available_components, true)) {
       return ORTE_ERROR;
    }
    
    /* All done */

    return ORTE_SUCCESS;
}

#endif
