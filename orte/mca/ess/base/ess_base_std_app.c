/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2012 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <sys/types.h>
#include <stdio.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#include "opal/mca/event/event.h"
#include "opal/mca/pmix/pmix.h"
#include "opal/util/arch.h"
#include "opal/util/os_path.h"
#include "opal/util/output.h"
#include "opal/util/proc.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_cr.h"
#include "opal/runtime/opal_progress_threads.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/dfs/base/base.h"
#include "orte/mca/grpcomm/base/base.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/filem/base/base.h"
#include "orte/mca/errmgr/base/base.h"
#if OPAL_ENABLE_FT_CR == 1
#include "orte/mca/snapc/base/base.h"
#include "orte/mca/sstore/base/base.h"
#endif
#include "orte/mca/state/base/base.h"
#include "orte/util/proc_info.h"
#include "orte/util/session_dir.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"

#include "orte/runtime/orte_cr.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/ess/base/base.h"

static bool progress_thread_running = false;

int orte_ess_base_app_setup(bool db_restrict_local)
{
    int ret;
    char *error = NULL;
    opal_value_t kv;

    /*
     * stdout/stderr buffering
     * If the user requested to override the default setting then do
     * as they wish.
     */
    if( orte_ess_base_std_buffering > -1 ) {
        if( 0 == orte_ess_base_std_buffering ) {
            setvbuf(stdout, NULL, _IONBF, 0);
            setvbuf(stderr, NULL, _IONBF, 0);
        }
        else if( 1 == orte_ess_base_std_buffering ) {
            setvbuf(stdout, NULL, _IOLBF, 0);
            setvbuf(stderr, NULL, _IOLBF, 0);
        }
        else if( 2 == orte_ess_base_std_buffering ) {
            setvbuf(stdout, NULL, _IOFBF, 0);
            setvbuf(stderr, NULL, _IOFBF, 0);
        }
    }

    /* if I am an MPI app, we will let the MPI layer define and
     * control the opal_proc_t structure. Otherwise, we need to
     * do so here */
    if (ORTE_PROC_NON_MPI) {
        orte_process_info.super.proc_name = *(opal_process_name_t*)ORTE_PROC_MY_NAME;
        orte_process_info.super.proc_hostname = orte_process_info.nodename;
        orte_process_info.super.proc_flags = OPAL_PROC_ALL_LOCAL;
        orte_process_info.super.proc_arch = opal_local_arch;
        opal_proc_local_set(&orte_process_info.super);
    }

    /* get an async event base - we use the opal_async one so
     * we don't startup extra threads if not needed */
    orte_event_base = opal_progress_thread_init(NULL);
    progress_thread_running = true;
    /* open and setup the state machine */
    if (ORTE_SUCCESS != (ret = mca_base_framework_open(&orte_state_base_framework, 0))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_state_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_state_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_state_base_select";
        goto error;
    }

    /* open the errmgr */
    if (ORTE_SUCCESS != (ret = mca_base_framework_open(&orte_errmgr_base_framework, 0))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_errmgr_base_open";
        goto error;
    }

    /* setup my session directory */
    if (orte_create_session_dirs) {
        OPAL_OUTPUT_VERBOSE((2, orte_ess_base_framework.framework_output,
                             "%s setting up session dir with\n\ttmpdir: %s\n\thost %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (NULL == orte_process_info.tmpdir_base) ? "UNDEF" : orte_process_info.tmpdir_base,
                             orte_process_info.nodename));
        if (ORTE_SUCCESS != (ret = orte_session_dir(true,
                                                    orte_process_info.tmpdir_base,
                                                    orte_process_info.nodename, NULL,
                                                    ORTE_PROC_MY_NAME))) {
            ORTE_ERROR_LOG(ret);
            error = "orte_session_dir";
            goto error;
        }
        /* Once the session directory location has been established, set
           the opal_output env file location to be in the
           proc-specific session directory. */
        opal_output_set_output_file_info(orte_process_info.proc_session_dir,
                                         "output-", NULL, NULL);
        /* store the session directory location */
        OBJ_CONSTRUCT(&kv, opal_value_t);
        kv.key = strdup(OPAL_PMIX_NSDIR);
        kv.type = OPAL_STRING;
        kv.data.string = strdup(orte_process_info.job_session_dir);
        if (OPAL_SUCCESS != (ret = opal_pmix.store_local(ORTE_PROC_MY_NAME, &kv))) {
            ORTE_ERROR_LOG(ret);
            OBJ_DESTRUCT(&kv);
            error = "opal pmix put job sessiondir";
            goto error;
        }
        OBJ_DESTRUCT(&kv);
        OBJ_CONSTRUCT(&kv, opal_value_t);
        kv.key = strdup(OPAL_PMIX_PROCDIR);
        kv.type = OPAL_STRING;
        kv.data.string = strdup(orte_process_info.proc_session_dir);
        if (OPAL_SUCCESS != (ret = opal_pmix.store_local(ORTE_PROC_MY_NAME, &kv))) {
            ORTE_ERROR_LOG(ret);
            OBJ_DESTRUCT(&kv);
            error = "opal pmix put proc sessiondir";
            goto error;
        }
        OBJ_DESTRUCT(&kv);
    }

    /* setup the errmgr */
    if (ORTE_SUCCESS != (ret = orte_errmgr_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_errmgr_base_select";
        goto error;
    }

#if OPAL_ENABLE_FT_CR == 1
    /*
     * Setup the SnapC
     */
    if (ORTE_SUCCESS != (ret = mca_base_framework_open(&orte_snapc_base_framework, 0))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_snapc_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = mca_base_framework_open(&orte_sstore_base_framework, 0))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_sstore_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_snapc_base_select(ORTE_PROC_IS_HNP, ORTE_PROC_IS_APP))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_snapc_base_select";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_sstore_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_sstore_base_select";
        goto error;
    }
    /* apps need the OPAL CR stuff */
    opal_cr_set_enabled(true);
#else
    opal_cr_set_enabled(false);
#endif
    /* Initalize the CR setup
     * Note: Always do this, even in non-FT builds.
     * If we don't some user level tools may hang.
     */
    if (ORTE_SUCCESS != (ret = orte_cr_init())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_cr_init";
        goto error;
    }
    /* open the distributed file system */
    if (ORTE_SUCCESS != (ret = mca_base_framework_open(&orte_dfs_base_framework, 0))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_dfs_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_dfs_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_dfs_base_select";
        goto error;
    }
    return ORTE_SUCCESS;
 error:
    if (!progress_thread_running) {
        /* can't send the help message, so ensure it
         * comes out locally
         */
        orte_show_help_finalize();
    }
    orte_show_help("help-orte-runtime.txt",
                   "orte_init:startup:internal-failure",
                   true, error, ORTE_ERROR_NAME(ret), ret);
    return ret;
}

int orte_ess_base_app_finalize(void)
{
    orte_cr_finalize();

#if OPAL_ENABLE_FT_CR == 1
    (void) mca_base_framework_close(&orte_snapc_base_framework);
    (void) mca_base_framework_close(&orte_sstore_base_framework);
#endif

    /* close frameworks */
    (void) mca_base_framework_close(&orte_filem_base_framework);
    (void) mca_base_framework_close(&orte_errmgr_base_framework);

    (void) mca_base_framework_close(&orte_dfs_base_framework);
    (void) mca_base_framework_close(&orte_state_base_framework);

    orte_session_dir_finalize(ORTE_PROC_MY_NAME);

    /* release the event base */
    if (progress_thread_running) {
        opal_progress_thread_finalize(NULL);
        progress_thread_running = false;
    }

    return ORTE_SUCCESS;
}

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
 * To get around the problem, we drop a marker in the proc-level
 * session dir. If session dir's were not allowed, then we just
 * ignore this question.
 *
 * In some cases, however, we DON'T want to create that alert. For
 * example, if an orted detects that the HNP has died, then there
 * is truly nobody to alert! In these cases, we pass report=false
 * to indicate that we don't want the marker dropped.
 */
void orte_ess_base_app_abort(int status, bool report)
{
    int fd;
    char *myfile;
    struct timespec tp = {0, 100000};

    /* Exit - do NOT do a normal finalize as this will very likely
     * hang the process. We are aborting due to an abnormal condition
     * that precludes normal cleanup
     *
     * We do need to do the following bits to make sure we leave a
     * clean environment. Taken from orte_finalize():
     * - Assume errmgr cleans up child processes before we exit.
     */
    /* CRS cleanup since it may have a named pipe and thread active */
    orte_cr_finalize();
    /* If we were asked to report this termination, do so.
     * Since singletons don't start an HNP unless necessary, and
     * direct-launched procs don't have daemons at all, only send
     * the message if routing is enabled as this indicates we
     * have someone to send to
     */
    if (report && orte_create_session_dirs) {
        myfile = opal_os_path(false, orte_process_info.proc_session_dir, "aborted", NULL);
        fd = open(myfile, O_CREAT, S_IRUSR);
        close(fd);
        /* now introduce a short delay to allow any pending
         * messages (e.g., from a call to "show_help") to
         * have a chance to be sent */
        nanosleep(&tp, NULL);
    }
    /* - Clean out the global structures
     * (not really necessary, but good practice) */
    orte_proc_info_finalize();
    /* Now Exit */
    _exit(status);
}
