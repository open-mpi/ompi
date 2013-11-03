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
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/plm/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "orte/mca/plm/base/static-components.h"

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
    NULL,   /* cannot terminate procs from a proxy */
    NULL,   /* cannot signal job from a proxy */
    orte_plm_proxy_finalize
};


static int orte_plm_base_close(void)
{
    int rc;

    /* Close the selected component */
    if( NULL != orte_plm.finalize ) {
        orte_plm.finalize();
    }

   /* if we are the HNP, then stop our receive */
    if (ORTE_PROC_IS_HNP) {
        if (ORTE_SUCCESS != (rc = orte_plm_base_comm_stop())) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    return mca_base_framework_components_close(&orte_plm_base_framework, NULL);
}

/**
 * Function for finding and opening either all MCA components,
 * or the one that was specifically requested via a MCA parameter.
 */
static int orte_plm_base_open(mca_base_open_flag_t flags)
{
    /* init the next jobid */
    orte_plm_globals.next_jobid = 1;
    
    /* default to assigning daemons to nodes at launch */
    orte_plm_globals.daemon_nodes_assigned_at_launch = true;

     /* Open up all available components */
    return mca_base_framework_components_open(&orte_plm_base_framework, flags);
}

MCA_BASE_FRAMEWORK_DECLARE(orte, plm, NULL, NULL, orte_plm_base_open, orte_plm_base_close,
                           mca_plm_base_static_components, 0);
