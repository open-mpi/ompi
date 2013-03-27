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
#include "orte/util/name_fns.h"
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
    --orte_initialized;
    if (0 != orte_initialized) {
        /* check for mismatched calls */
        if (0 > orte_initialized) {
            opal_output(0, "%s MISMATCHED CALLS TO ORTE FINALIZE",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        }
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
    (void) mca_base_framework_close(&orte_ess_base_framework);
    
#if ORTE_ENABLE_PROGRESS_THREADS
    /* stop the progress thread */
    orte_event_base_active = false;
    /* break the event loop */
    opal_event_base_loopbreak(orte_event_base);
    /* wait for thread to exit */
    opal_thread_join(&orte_progress_thread, NULL);
    OBJ_DESTRUCT(&orte_progress_thread);
    /* release the event base */
/*    opal_event_base_free(orte_event_base); */
#endif

    /* cleanup the process info */
    orte_proc_info_finalize();

    /* Close the general debug stream */
    opal_output_close(orte_debug_output);
    
    /* finalize the opal utilities */
    opal_finalize();
    
    return ORTE_SUCCESS;
}
