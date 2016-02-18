/*
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2015 Intel, Inc. All rights reserved
 * Copyright (c) 2012-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2014      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 */
#include "ompi_config.h"
#include "ompi/constants.h"

#include <string.h>
#include <stdio.h>
#include <ctype.h>

#include "opal/dss/dss.h"
#include "opal/util/argv.h"
#include "opal/util/proc.h"
#include "opal/util/opal_getcwd.h"
#include "opal/mca/pmix/pmix.h"
#include "opal/threads/threads.h"
#include "opal/class/opal_list.h"
#include "opal/dss/dss.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/grpcomm/base/base.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/state/state.h"
#include "orte/mca/routed/routed.h"
#include "orte/util/name_fns.h"
#include "orte/util/session_dir.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_data_server.h"

#include "ompi/mca/rte/base/base.h"
#include "ompi/mca/rte/rte.h"
#include "ompi/debuggers/debuggers.h"
#include "ompi/proc/proc.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"

extern ompi_rte_orte_component_t mca_rte_orte_component;

typedef struct {
    volatile bool active;
    int status;
    int errhandler;
} errhandler_t;

static void register_cbfunc(int status, int errhndler, void *cbdata)
{
    errhandler_t *cd = (errhandler_t*)cbdata;
    cd->status = status;
    cd->errhandler = errhndler;
    cd->active = false;
}

static volatile bool wait_for_release = true;
static int errhandler = -1;

static void notify_cbfunc(int status,
                          opal_list_t *procs,
                          opal_list_t *info,
                          opal_pmix_release_cbfunc_t cbfunc,
                          void *cbdata)
{
    if (NULL != cbfunc) {
        cbfunc(cbdata);
    }
    wait_for_release = false;
}


int ompi_rte_init(int *pargc, char ***pargv)
{
    int rc;
    opal_list_t info;
    opal_value_t val;
    errhandler_t cd;

    if (ORTE_SUCCESS != (rc = orte_init(pargc, pargv, ORTE_PROC_MPI))) {
        return rc;
    }

    if (!orte_standalone_operation) {
        /* register to receive any debugger release */
        OBJ_CONSTRUCT(&info, opal_list_t);
        OBJ_CONSTRUCT(&val, opal_value_t);
        val.key = strdup(OPAL_PMIX_ERROR_NAME);
        val.type = OPAL_INT;
        val.data.integer = OPAL_ERR_DEBUGGER_RELEASE;
        opal_list_append(&info, &val.super);
        cd.status = ORTE_ERROR;
        cd.errhandler = -1;
        cd.active = true;

        opal_pmix.register_errhandler(&info, notify_cbfunc, register_cbfunc, &cd);

        /* let the MPI progress engine run while we wait for
         * registration to complete */
        OMPI_WAIT_FOR_COMPLETION(cd.active);
        /* safely deconstruct the list */
        opal_list_remove_first(&info);
        OBJ_DESTRUCT(&val);
        OBJ_DESTRUCT(&info);
        if (OPAL_SUCCESS != cd.status) {
            /* ouch - we are doomed */
            ORTE_ERROR_LOG(cd.status);
            return OMPI_ERROR;
        }
        errhandler = cd.errhandler;
    }

    return OMPI_SUCCESS;
}

void ompi_rte_abort(int error_code, char *fmt, ...)
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
    if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_DAEMON) {
        /* whack my local procs */
        orte_odls.kill_local_procs(NULL);
        /* whack any session directories */
        orte_session_dir_cleanup(ORTE_JOBID_WILDCARD);
    } else {
        /* cleanup my session directory */
        orte_session_dir_finalize(ORTE_PROC_MY_NAME);
    }

    /* if a critical connection failed, or a sensor limit was exceeded, exit without dropping a core */
    if (ORTE_ERR_CONNECTION_FAILED == error_code ||
        ORTE_ERR_SENSOR_LIMIT_EXCEEDED == error_code) {
        orte_ess.abort(error_code, false);
    } else {
        orte_ess.abort(error_code, true);
    }

    /*
     * We must exit in orte_ess.abort; all implementations of orte_ess.abort
     * contain __opal_attribute_noreturn__
     */
    /* No way to reach here, but put an exit() here a) just to cover
       for bugs, and b) to let the compiler know we're honoring the
       __opal_attribute_noreturn__. */
    exit(-1);
}

/*
 * Wait for a debugger if asked.  We support two ways of waiting for
 * attaching debuggers -- see big comment in
 * orte/tools/orterun/debuggers.c explaining the two scenarios.
 */

void ompi_rte_wait_for_debugger(void)
{
    int debugger;

    /* See lengthy comment in orte/tools/orterun/debuggers.c about
       orte_in_parallel_debugger */
    debugger = orte_in_parallel_debugger;

    if (1 == MPIR_being_debugged) {
        debugger = 1;
    }

    if (!debugger) {
        /* if not, just return */
        return;
    }
    /* if we are being debugged, then we need to find
     * the correct plug-ins
     */
    ompi_debugger_setup_dlls();

    /* wait for the debugger to attach */
    if (orte_standalone_operation) {
        /* spin until debugger attaches and releases us */
        while (MPIR_debug_gate == 0) {
#if defined(HAVE_USLEEP)
            usleep(100000); /* microseconds */
#else
            sleep(1);       /* seconds */
#endif
        }
    } else {
        /* now wait for the notification to occur */
        OMPI_WAIT_FOR_COMPLETION(wait_for_release);
        /* deregister the errhandler */
        opal_pmix.deregister_errhandler(errhandler, NULL, NULL);
    }
}
