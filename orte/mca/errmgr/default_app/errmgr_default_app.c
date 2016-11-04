/*
 * Copyright (c) 2009-2011 The Trustees of Indiana University.
 *                         All rights reserved.
 *
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 *
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2015-2016 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#include <string.h>

#include "opal/util/output.h"
#include "opal/dss/dss.h"
#include "opal/mca/pmix/pmix.h"

#include "orte/util/error_strings.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/state/state.h"

#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/errmgr/base/errmgr_private.h"
#include "errmgr_default_app.h"

/*
 * Module functions: Global
 */
 static int init(void);
 static int finalize(void);

 static int abort_peers(orte_process_name_t *procs,
                        orte_std_cntr_t num_procs,
                        int error_code);

/******************
 * HNP module
 ******************/
 orte_errmgr_base_module_t orte_errmgr_default_app_module = {
    init,
    finalize,
    orte_errmgr_base_log,
    orte_errmgr_base_abort,
    abort_peers,
    NULL,
    NULL,
    NULL,
    orte_errmgr_base_register_migration_warning,
    orte_errmgr_base_register_error_callback,
    orte_errmgr_base_execute_error_callbacks
};

static void proc_errors(int fd, short args, void *cbdata);

static size_t myerrhandle = SIZE_MAX;

static void register_cbfunc(int status, size_t errhndler, void *cbdata)
{
    myerrhandle = errhndler;
}

static void notify_cbfunc(int status,
                          const opal_process_name_t *source,
                          opal_list_t *info, opal_list_t *results,
                          opal_pmix_notification_complete_fn_t cbfunc, void *cbdata)
{
    orte_proc_state_t state;

    OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base_framework.framework_output,
                        "%s errmgr:default_app: pmix event handler called with status %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_ERROR_NAME(status)));

    /* we must convert the incoming status into an equivalent state
     * so we can activate the state machine */
    switch(status) {
        case OPAL_ERR_PROC_ABORTED:
            state = ORTE_PROC_STATE_ABORTED;
            break;
        case OPAL_ERR_PROC_REQUESTED_ABORT:
            state = ORTE_PROC_STATE_CALLED_ABORT;
            break;
        default:
            state = ORTE_PROC_STATE_TERMINATED;
    }

    /* let the caller know we processed this, but allow the
     * chain to continue */
    if (NULL != cbfunc) {
        cbfunc(ORTE_SUCCESS, NULL, NULL, NULL, cbdata);
    }

    /* push it into our event base */
    ORTE_ACTIVATE_PROC_STATE(ORTE_PROC_MY_NAME, state);
}

/************************
 * API Definitions
 ************************/
 static int init(void)
 {
    /* setup state machine to trap proc errors */
    orte_state.add_proc_state(ORTE_PROC_STATE_ERROR, proc_errors, ORTE_ERROR_PRI);

    /* tie the default PMIx event handler back to us */
    opal_pmix.register_evhandler(NULL, NULL, notify_cbfunc, register_cbfunc, NULL);

    return ORTE_SUCCESS;
}

static int finalize(void)
{
    if (SIZE_MAX != myerrhandle) {
        opal_pmix.deregister_evhandler(myerrhandle, NULL, NULL);
    }
    return ORTE_SUCCESS;
}

static void proc_errors(int fd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;
    char *nodename;
    orte_error_t err;
    opal_pointer_array_t errors;

    OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base_framework.framework_output,
                        "%s errmgr:default_app: proc %s state %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&caddy->name),
                        orte_proc_state_to_str(caddy->proc_state)));

    /*
     * if orte is trying to shutdown, just let it
     */
     if (orte_finalizing) {
        OBJ_RELEASE(caddy);
        return;
    }

    /* pass the error to the error_callbacks for processing */
    OBJ_CONSTRUCT(&errors, opal_pointer_array_t);
    opal_pointer_array_init(&errors, 1, INT_MAX, 1);
    err.errcode = caddy->proc_state;
    err.proc = caddy->name;
    opal_pointer_array_add(&errors, &err);


    if (ORTE_PROC_STATE_UNABLE_TO_SEND_MSG == caddy->proc_state) {
        /* we can't send a message - print a message */
        nodename = orte_get_proc_hostname(&caddy->name);
        orte_show_help("help-errmgr-base",
                       "undeliverable-msg",
                       true, ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                       orte_process_info.nodename,
                       ORTE_NAME_PRINT(&caddy->name),
                       (NULL == nodename) ? "Unknown" : nodename);
        /* flag that we must abnormally terminate as far as the
         * RTE is concerned
         */
         orte_abnormal_term_ordered = true;
     } else if (ORTE_PROC_STATE_LIFELINE_LOST == caddy->proc_state) {
        /* we need to die, so mark us so */
        orte_abnormal_term_ordered = true;
    }

    orte_errmgr_base_execute_error_callbacks(&errors);
    OBJ_DESTRUCT(&errors);

    OBJ_RELEASE(caddy);
}

static int abort_peers(orte_process_name_t *procs,
                       orte_std_cntr_t num_procs,
                       int error_code)
{
    /* just abort */
    if (0 < opal_output_get_verbosity(orte_errmgr_base_framework.framework_output)) {
        orte_errmgr_base_abort(error_code, "%s called abort_peers",
                               ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
    } else {
        orte_errmgr_base_abort(error_code, NULL);
    }
    return ORTE_SUCCESS;
}
