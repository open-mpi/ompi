/*
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
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
#include "opal/threads/threads.h"

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

extern ompi_rte_orte_component_t mca_rte_orte_component;
static void recv_callback(int status, orte_process_name_t* sender,
                          opal_buffer_t *buffer,
                          orte_rml_tag_t tag, void *cbdata);

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
    orte_rml_recv_cb_t xfer;

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
        if (0 != ORTE_PROC_MY_NAME->vpid) {
            return;
        }
    
        /* VPID 0 waits for a message from the HNP */
        OBJ_CONSTRUCT(&xfer, orte_rml_recv_cb_t);
        xfer.active = true;
        orte_rml.recv_buffer_nb(OMPI_NAME_WILDCARD,
                                ORTE_RML_TAG_DEBUGGER_RELEASE,
                                ORTE_RML_NON_PERSISTENT,
                                orte_rml_recv_callback, &xfer);
        /* let the MPI progress engine run while we wait */
        OMPI_WAIT_FOR_COMPLETION(xfer.active);
    }
}    

static bool direct_modex_enabled = false;

int ompi_rte_modex(ompi_rte_collective_t *coll)
{
    /* mark that this process reached modex */
    orte_grpcomm_base.modex_ready = true;

    if ((orte_process_info.num_procs < ompi_hostname_cutoff) ||
         !mca_rte_orte_component.direct_modex ||
         orte_standalone_operation) {
        /* if we are direct launched and/or below a user-specified
         * cutoff value, then we just fall thru to the ORTE modex
         */
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_framework.framework_output,
                             "%s running modex",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        return orte_grpcomm.modex(coll);
    }

    /* if the user defined a cutoff value that we are larger
     * than, and if we were not direct launched, then skip
     * the modex operation. We already have all the RTE-level
     * info we need, and we will retrieve the MPI-level info
     * only as requested. This will provide a faster startup
     * time since we won't do a massive allgather operation,
     * but will make first-message connections slower.
     */
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_framework.framework_output,
                         "%s using direct modex",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    direct_modex_enabled = true;

    /* if direct modex was enabled, setup the receive for it */
    orte_rml.recv_buffer_nb(OMPI_NAME_WILDCARD,
                            ORTE_RML_TAG_DIRECT_MODEX_RESP,
                            ORTE_RML_PERSISTENT,
                            recv_callback, NULL);

    /* process any pending requests for our data */
    ORTE_ACTIVATE_PROC_STATE(ORTE_PROC_MY_NAME, ORTE_PROC_STATE_MODEX_READY);
    /* release the barrier */
    if (NULL != coll->cbfunc) {
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_framework.framework_output,
                             "%s CALLING MODEX RELEASE",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        coll->cbfunc(NULL, coll->cbdata);
    } else {
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_framework.framework_output,
                             "%s NO MODEX RELEASE CBFUNC",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    }
    /* flag the collective as complete */
    coll->active = false;
    return OMPI_SUCCESS;
}

int ompi_rte_db_store(const orte_process_name_t *nm, const char* key,
                      const void *data, opal_data_type_t type)
{
    /* MPI connection data is to be shared with ALL other processes */
    return opal_db.store((opal_identifier_t*)nm, OPAL_SCOPE_GLOBAL, key, data, type);
}

static int direct_modex(orte_process_name_t *peer, opal_scope_t scope)
{
    int rc;
    ompi_orte_tracker_t *req;
    opal_buffer_t *buf;

    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_framework.framework_output,
                         "%s requesting direct modex from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer)));

    buf = OBJ_NEW(opal_buffer_t);
    /* pack the scope of the request */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(buf, &scope, 1, OPAL_DATA_SCOPE_T))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        return rc;
    }

    /* create a tracker for this request */
    req = OBJ_NEW(ompi_orte_tracker_t);
    req->peer = *peer;

    /* add this to our list of requests - protect it as the
     * callback that returns data comes in the ORTE event base
     */
    opal_mutex_lock(&mca_rte_orte_component.lock);
    opal_list_append(&mca_rte_orte_component.modx_reqs, &req->super);
    opal_mutex_unlock(&mca_rte_orte_component.lock);

    /* send the request */
    if (ORTE_SUCCESS != (rc = orte_rml.send_buffer_nb(peer, buf,
                                                      ORTE_RML_TAG_DIRECT_MODEX,
                                                      orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        opal_mutex_lock(&mca_rte_orte_component.lock);
        opal_list_remove_item(&mca_rte_orte_component.modx_reqs, &req->super);
        opal_mutex_unlock(&mca_rte_orte_component.lock);
        OBJ_RELEASE(req);
        return rc;
    }

    /* wait for the response */
    opal_mutex_lock(&req->lock);
    while (req->active) {
        opal_condition_wait(&req->cond, &req->lock);
    }
    /* now can safely destruct the request */
    OBJ_RELEASE(req);

    return ORTE_SUCCESS;
}

int ompi_rte_db_fetch(const struct ompi_proc_t *proc,
                      const char *key,
                      void **data, opal_data_type_t type)
{
    int rc;

    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_framework.framework_output,
                         "%s fetch data from %s for %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&proc->proc_name), key));

    if (OPAL_SUCCESS != (rc = opal_db.fetch((opal_identifier_t*)(&proc->proc_name), key, data, type))) {
        if (direct_modex_enabled) {
            OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_framework.framework_output,
                                 "%s requesting direct modex from %s for %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc->proc_name), key));
            /* if we couldn't fetch the data via the db, then we will attempt
             * to retrieve it from the target proc
             */
            if (ORTE_SUCCESS != (rc = direct_modex((orte_process_name_t*)&proc->proc_name, OPAL_SCOPE_PEER))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* now retrieve the requested piece */
            if (OPAL_SUCCESS != (rc = opal_db.fetch((opal_identifier_t*)(&proc->proc_name), key, data, type))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        } else {
            return rc;
        }
    }
    /* update the hostname upon first call to modex-recv for this proc */
    if (NULL == proc->proc_hostname) {
        opal_db.fetch_pointer((opal_identifier_t*)(&proc->proc_name), ORTE_DB_HOSTNAME, (void**)&proc->proc_hostname, OPAL_STRING);
    }
    return OMPI_SUCCESS;
}

int ompi_rte_db_fetch_pointer(const struct ompi_proc_t *proc,
                              const char *key,
                              void **data, opal_data_type_t type)
{
    int rc;

    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_framework.framework_output,
                         "%s fetch data pointer from %s for %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&proc->proc_name), key));

    if (OPAL_SUCCESS != (rc = opal_db.fetch_pointer((opal_identifier_t*)(&proc->proc_name), key, data, type))) {
        if (direct_modex_enabled) {
            /* if we couldn't fetch the data via the db, then we will attempt
             * to retrieve it from the target proc
             */
            OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_framework.framework_output,
                                 "%s requesting direct modex from %s for %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc->proc_name), key));
            if (ORTE_SUCCESS != (rc = direct_modex((orte_process_name_t*)&proc->proc_name, OPAL_SCOPE_PEER))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* now retrieve the requested piece */
            if (OPAL_SUCCESS != (rc = opal_db.fetch_pointer((opal_identifier_t*)(&proc->proc_name), key, data, type))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        } else {
            return rc;
        }
    }
    /* update the hostname upon first call to modex-recv for this proc */
    if (NULL == proc->proc_hostname) {
        opal_db.fetch_pointer((opal_identifier_t*)(&proc->proc_name), ORTE_DB_HOSTNAME, (void**)&proc->proc_hostname, OPAL_STRING);
    }
    return OMPI_SUCCESS;
}

int ompi_rte_db_fetch_multiple(const struct ompi_proc_t *proc,
                               const char *key,
                               opal_list_t *kvs)
{
    int rc;

    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_framework.framework_output,
                         "%s fetch multiple from %s for %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&proc->proc_name), key));

    /* MPI processes are only concerned with shared info */
    if (OPAL_SUCCESS != (rc = opal_db.fetch_multiple((opal_identifier_t*)(&proc->proc_name),
                                                     OPAL_SCOPE_GLOBAL, key, kvs))) {
        if (direct_modex_enabled) {
            /* if we couldn't fetch the data via the db, then we will attempt
             * to retrieve it from the target proc
             */
            OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_framework.framework_output,
                                 "%s requesting direct modex from %s for %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc->proc_name), key));
            if (ORTE_SUCCESS != (rc = direct_modex((orte_process_name_t*)&proc->proc_name, OPAL_SCOPE_GLOBAL))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* now retrieve the requested pieces */
            if (OPAL_SUCCESS != (rc = opal_db.fetch_multiple((opal_identifier_t*)(&proc->proc_name),
                                                             OPAL_SCOPE_GLOBAL, key, kvs))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        } else {
            return rc;
        }
    }
    /* update the hostname upon first call to modex-recv for this proc */
    if (NULL == proc->proc_hostname) {
        opal_db.fetch_pointer((opal_identifier_t*)(&proc->proc_name), ORTE_DB_HOSTNAME, (void**)&proc->proc_hostname, OPAL_STRING);
    }
    return OMPI_SUCCESS;
}

int ompi_rte_db_remove(const orte_process_name_t *nm,
                       const char *key)
{
    return opal_db.remove((opal_identifier_t*)nm, key);
}


/* this function executes in the RML event base, and so
 * we must take care to protect against threading conflicts
 * with the MPI layer
 */
static void recv_callback(int status, orte_process_name_t* sender,
                          opal_buffer_t *buffer,
                          orte_rml_tag_t tag, void *cbdata)
{
    ompi_orte_tracker_t *req, *nxt;

    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_framework.framework_output,
                         "%s received direct modex data from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));

    /* this is a std modex package, so unpack it with the
     * grpcomm function and cache it locally so we can quickly get
     * more pieces if necessary - don't need to thread protect
     * here as only one RML callback can be active at a time
     */
    orte_grpcomm_base_store_modex(buffer, NULL);

    /* protect */
    opal_mutex_lock(&mca_rte_orte_component.lock);

    /* find all requests against this sender and release them */
    OPAL_LIST_FOREACH_SAFE(req, nxt, &mca_rte_orte_component.modx_reqs, ompi_orte_tracker_t) {
        if (req->peer.jobid == sender->jobid &&
            req->peer.vpid == sender->vpid) {
            /* remove the request from the list */
            opal_list_remove_item(&mca_rte_orte_component.modx_reqs, &req->super);
            /* wake up the waiting thread */
            req->active = false;
            opal_condition_signal(&req->cond);
        }
    }

    /* release */
    opal_mutex_unlock(&mca_rte_orte_component.lock);
}

