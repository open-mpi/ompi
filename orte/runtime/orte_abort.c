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
#include "orte/orte_constants.h"

#include <stdio.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/util/output.h"
#include "opal/runtime/opal_progress.h"
#include "opal/event/event.h"
#include "opal/util/os_path.h"

#include "orte/runtime/runtime.h"
#include "orte/util/session_dir.h"
#include "orte/util/sys_info.h"
#include "orte/mca/errmgr/errmgr.h"


/*
 * We do NOT call the regular C-library "abort" function, even
 * though that would have alerted us to the fact that this is
 * an abnormal termination, because it would automatically cause
 * a core file to be generated. On large systems, that can be
 * overwhelming (imagine a few thousand Gbyte-sized files hitting
 * a shared file system simultaneously...ouch!).
 *
 * However, this causes a problem for OpenRTE as the system truly
 * needs to know that this actually IS an abnormal termination.
 * To get around the problem, we create a file in the session
 * directory - we don't need to put anything in it, though, as its
 * very existence simply alerts us that this was an abnormal
 * termination.
 *
 * The session directory finalize system will clean this file up
 * for us automagically. However, it needs to stick around long
 * enough for our local daemon to find it! So, we do NOT call
 * session_dir_finalize here!!! Someone will clean up for us.
 *
 * In some cases, however, we DON'T want to create that alert. For
 * example, if an orted detects that the HNP has died, then there
 * is truly nobody to alert! In these cases, we pass report=false
 * to prevent the abort file from being created. This allows the
 * session directory tree to cleanly be eliminated.
 */
int orte_abort(int status, bool report)
{
    char *abort_file;
    int fd;

    /* Exit - do NOT do a normal finalize as this will very likely
     * hang the process. We are aborting due to an abnormal condition
     * that precludes normal cleanup 
     *
     * We do need to do the following bits to make sure we leave a 
     * clean environment. Taken from orte_finalize():
     * - Assume errmgr cleans up child processes before we exit.
     */

    /* If we were asked to report this termination,
     * write an "abort" file into our session directory
     */
    if (report) {
        abort_file = opal_os_path(false, orte_process_info.proc_session_dir, "abort", NULL);
        if (NULL == abort_file) {
            /* got a problem */
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            goto CLEANUP;
        }
        fd = open(abort_file, O_CREAT);
        if (0 < fd) close(fd);        
    }
    
CLEANUP:
    /* - Clean out the global structures 
     * (not really necessary, but good practice) */
    orte_sys_info_finalize();
    orte_proc_info_finalize();
    orte_univ_info_finalize();

    /* Now Exit */
    exit(status);
}
