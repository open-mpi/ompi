/*
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 */
#include "ompi_config.h"
#include "ompi/constants.h"

#include <string.h>
#include <stdio.h>
#include <ctype.h>

#include "opal/dss/dss.h"
#include "opal/util/argv.h"
#include "opal/util/opal_getcwd.h"
#include "opal/mca/db/db.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/routed/routed.h"
#include "orte/util/name_fns.h"
#include "orte/util/session_dir.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_data_server.h"

#include "ompi/mca/rte/base/base.h"
#include "ompi/mca/rte/rte.h"
#include "ompi/debuggers/debuggers.h"

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
    /* No way to reach here */
}


/*
 * Wait for a debugger if asked.  We support two ways of waiting for
 * attaching debuggers -- see big comment in
 * orte/tools/orterun/debuggers.c explaining the two scenarios.
 */
void ompi_rte_wait_for_debugger(void)
{
    opal_buffer_t buf;
    int debugger, rc;

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
        /* only the rank=0 proc waits for either a message from the
         * HNP or for the debugger to attach - everyone else will just
         * spin in * the grpcomm barrier in ompi_mpi_init until rank=0
         * joins them.
         */
        if (0 != OMPI_PROC_MY_NAME->vpid) {
            return;
        }
    
        /* VPID 0 waits for a message from the HNP */
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        rc = ompi_rte_recv_buffer(OMPI_NAME_WILDCARD, &buf, 
                                  ORTE_RML_TAG_DEBUGGER_RELEASE, 0);
        OBJ_DESTRUCT(&buf);  /* don't care about contents of message */
        if (rc < 0) {
            /* if it failed for some reason, then we are in trouble -
             * for now, just report the problem and give up waiting
             */
            opal_output(0, "Debugger_attach[rank=%ld]: could not wait for debugger!",
                        (long)OMPI_PROC_MY_NAME->vpid);
        }
    }
}    

int ompi_rte_db_store(const orte_process_name_t *nm, const char* key,
                      const void *data, opal_data_type_t type)
{
    opal_identifier_t *id;

    id = (opal_identifier_t*)nm;
    return opal_db.store((*id), OPAL_DB_GLOBAL, key, data, type);
}

int ompi_rte_db_fetch(const orte_process_name_t *nm,
                      const char *key,
                      void **data, opal_data_type_t type)
{
    opal_identifier_t *id;

    id = (opal_identifier_t*)nm;
    return opal_db.fetch((*id), key, data, type);
}

int ompi_rte_db_fetch_pointer(const orte_process_name_t *nm,
                              const char *key,
                              void **data, opal_data_type_t type)
{
    opal_identifier_t *id;

    id = (opal_identifier_t*)nm;
    return opal_db.fetch_pointer((*id), key, data, type);
}

int ompi_rte_db_fetch_multiple(const orte_process_name_t *nm,
                               const char *key,
                               opal_list_t *kvs)
{
    opal_identifier_t *id;

    id = (opal_identifier_t*)nm;
    return opal_db.fetch_multiple((*id), key, kvs);
}

int ompi_rte_db_remove(const orte_process_name_t *nm,
                       const char *key)
{
    opal_identifier_t *id;

    id = (opal_identifier_t*)nm;
    return opal_db.remove((*id), key);
}

