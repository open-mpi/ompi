/*
 * Copyright (c) 2009-2011 The Trustees of Indiana University.
 *                         All rights reserved.
 *
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 *
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2011      Los Alamos National Security, LLC.
 *                         All rights reserved.
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
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "opal/util/output.h"
#include "opal/dss/dss.h"

#include "orte/util/error_strings.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/routed/routed.h"
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
                       orte_std_cntr_t num_procs);
static orte_errmgr_fault_callback_t* set_fault_callback(orte_errmgr_fault_callback_t *cbfunc);

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
    set_fault_callback
};

static void proc_errors(int fd, short args, void *cbdata);

/************************
 * API Definitions
 ************************/
static int init(void)
{
    /* setup state machine to trap proc errors */
    orte_state.add_proc_state(ORTE_PROC_STATE_ERROR, proc_errors, ORTE_ERROR_PRI);

    return ORTE_SUCCESS;
}

static int finalize(void)
{
    return ORTE_SUCCESS;
}

static void proc_errors(int fd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;
    orte_ns_cmp_bitmask_t mask;

    OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base.output,
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

    if (ORTE_PROC_STATE_COMM_FAILED == caddy->proc_state) {
        mask = ORTE_NS_CMP_ALL;
        /* if it is our own connection, ignore it */
        if (OPAL_EQUAL == orte_util_compare_name_fields(mask, ORTE_PROC_MY_NAME, &caddy->name)) {
            OBJ_RELEASE(caddy);
            return;
        }
        /* see is this was a lifeline */
        if (ORTE_SUCCESS != orte_routed.route_lost(&caddy->name)) {
            /* order an exit */
            ORTE_ERROR_LOG(ORTE_ERR_UNRECOVERABLE);
            OBJ_RELEASE(caddy);
            exit(1);
        }
    }

    /* cleanup */
    OBJ_RELEASE(caddy);
}

static int abort_peers(orte_process_name_t *procs, orte_std_cntr_t num_procs)
{
    /* just abort */
    if (0 < opal_output_get_verbosity(orte_errmgr_base.output)) {
        orte_errmgr_base_abort(ORTE_ERROR_DEFAULT_EXIT_CODE, "%s called abort_peers",
                               ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
    } else {
        orte_errmgr_base_abort(ORTE_ERROR_DEFAULT_EXIT_CODE, NULL);
    }
    return ORTE_SUCCESS;
}

static orte_errmgr_fault_callback_t* set_fault_callback(orte_errmgr_fault_callback_t *cbfunc)
{
    return NULL;
}
