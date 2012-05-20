/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Los Alamos National Security, LLC.
 *                         All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/** @file **/

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/runtime/opal.h"
#include "opal/util/output.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_locks.h"
#include "orte/util/show_help.h"

/**
 * Leave ORTE.
 *
 * @retval ORTE_SUCCESS Upon success.
 * @retval ORTE_ERROR Upon failure.
 *
 * This function performs 
 */
int orte_finalize(void)
{
    if (!orte_initialized) {
        return ORTE_SUCCESS;
    }

    /* protect against multiple calls */
    if (opal_atomic_trylock(&orte_finalize_lock)) {
        return ORTE_SUCCESS;
    }
    
    /* set the flag indicating we are finalizing */
    orte_finalizing = true;

    /* close the orte_show_help system */
    orte_show_help_finalize();

    /* call the finalize function for this environment */
    orte_ess.finalize();
    
    /* close the ess itself */
    orte_ess_base_close();
    
    /* cleanup the process info */
    orte_proc_info_finalize();

#if !ORTE_DISABLE_FULL_SUPPORT
    /* Free some MCA param strings */
    if (NULL != orte_launch_agent) {
        free(orte_launch_agent);
    }
    if( NULL != orte_default_hostfile ) {
        free(orte_default_hostfile);
    }
#if ORTE_ENABLE_PROGRESS_THREADS
    if (ORTE_PROC_IS_APP) {
        /* stop the progress thread */
        orte_event_base_active = false;
        /* must trigger the "finalize" event to break us
         * out of the event loop
         */
        opal_event_active(&orte_finalize_event, OPAL_EV_WRITE, 1);
        /* wait for thread to exit */
        opal_thread_join(&orte_progress_thread, NULL);
        OBJ_DESTRUCT(&orte_progress_thread);
        /* release the event base */
        opal_event_base_free(orte_event_base);
    }
#endif
#endif

    /* Close the general debug stream */
    opal_output_close(orte_debug_output);
    
    /* finalize the opal utilities */
    opal_finalize();
    
    orte_initialized = false;
    return ORTE_SUCCESS;
}
