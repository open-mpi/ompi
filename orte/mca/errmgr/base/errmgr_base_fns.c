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

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdlib.h>

#include "orte/util/show_help.h"
#include "opal/util/trace.h"
#include "opal/util/error.h"

#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/util/session_dir.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/odls/odls.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/errmgr_private.h"


void orte_errmgr_base_log(int error_code, char *filename, int line)
{
    OPAL_TRACE(1);
    
    if (ORTE_ERR_SILENT == error_code) {
        /* if the error is silent, say nothing */
        return;
    }
    
    opal_output(0, "%s ORTE_ERROR_LOG: %s in file %s at line %d",
                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                ORTE_ERROR_NAME(error_code), filename, line);
}

void orte_errmgr_base_proc_aborted_not_avail(orte_process_name_t *name, int exit_code)
{
    return;
}

void orte_errmgr_base_incomplete_start_not_avail(orte_jobid_t job, int exit_code)
{
    return;
}

void orte_errmgr_base_error_abort(int error_code, char *fmt, ...)
{
    va_list arglist;
    
    /* If there was a message, output it */
    va_start(arglist, fmt);
    if( NULL != fmt ) {
        char* buffer = NULL;
        vasprintf( &buffer, fmt, arglist );
        opal_output( 0, "%s", buffer );
        free( buffer );
    }
    va_end(arglist);
    
    /* if I am a daemon or the HNP... */
    if (orte_process_info.hnp || orte_process_info.daemon) {
        /* whack my local procs */
        orte_odls.kill_local_procs(ORTE_JOBID_WILDCARD, false);
        /* whack any session directories */
        orte_session_dir_cleanup(ORTE_JOBID_WILDCARD);
    } else {
        /* cleanup my session directory */
        orte_session_dir_finalize(ORTE_PROC_MY_NAME);
    }
    
    /* abnormal exit */
    orte_ess.abort(error_code, false);
}

int orte_errmgr_base_register_cb_not_avail(orte_jobid_t job,
                                           orte_job_state_t state,
                                           orte_errmgr_cb_fn_t cbfunc,
                                           void *cbdata)
{
    return ORTE_ERR_NOT_AVAILABLE;
}
