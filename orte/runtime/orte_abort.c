/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "ompi_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "include/constants.h"
#include "runtime/runtime.h"
#include "opal/util/output.h"
#include "opal/runtime/opal_progress.h"
#include "opal/event/event.h"
#include "orte/util/session_dir.h"
#include "orte/util/sys_info.h"


int orte_abort(int status, char *fmt, ...)
{
    va_list arglist;

    /* If there was a message, output it */

    va_start(arglist, fmt);
    if( NULL != fmt ) {
        char* buffer = NULL;
        vasprintf( &buffer, fmt, arglist );
        opal_output( 0, buffer );
        free( buffer );
    }
    va_end(arglist);

    /* Exit - do NOT do normal finalize as this will very likely
     * hang the process. We are aborting due to an abnormal condition
     * that precludes normal cleanup 
     *
     * We do need to do the following bits to make sure we leave a 
     * clean environment. Taken from orte_finalize():
     * - Assume errmgr cleans up child processes before we exit.
     */

    /* - Turn of progress engine */
    opal_progress_finalize();

    /* - Turn off event loop */
    opal_event_fini();

    /* - Clean up session directory */
    orte_session_dir_finalize(orte_process_info.my_name);

    /* - Clean out the global structures (no necessary, but good practice) */
    orte_sys_info_finalize();
    orte_proc_info_finalize();
    orte_univ_info_finalize();

    /* Now Exit */
    exit(status);
}
