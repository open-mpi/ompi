/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2007-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007-2008 Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file **/

#include "orte_config.h"
#include "orte/constants.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/mca/dstore/base/base.h"
#include "opal/mca/pmix/base/base.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/util/proc.h"
#include "opal/runtime/opal.h"
#include "opal/threads/threads.h"

#include "orte/util/show_help.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/util/error_strings.h"
#include "orte/orted/pmix/pmix_server.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_locks.h"

/**
 * Static functions used to configure the interactions between the OPAL and
 * the runtime.
 */
static char*
_process_name_print_for_opal(const opal_process_name_t procname)
{
    orte_process_name_t* rte_name = (orte_process_name_t*)&procname;
    return ORTE_NAME_PRINT(rte_name);
}

static uint32_t
_process_name_jobid_for_opal(const opal_process_name_t procname)
{
    orte_process_name_t* rte_name = (orte_process_name_t*)&procname;
    return rte_name->jobid;
}

static uint32_t
_process_name_vpid_for_opal(const opal_process_name_t procname)
{
    orte_process_name_t* rte_name = (orte_process_name_t*)&procname;
    return rte_name->vpid;
}

static int
_process_name_compare(const opal_process_name_t p1, const opal_process_name_t p2)
{
    orte_process_name_t* o1 = (orte_process_name_t*)&p1;
    orte_process_name_t* o2 = (orte_process_name_t*)&p2;
    return orte_util_compare_name_fields(ORTE_NS_CMP_ALL, o1, o2);
}

/*
 * Whether we have completed orte_init or we are in orte_finalize
 */
int orte_initialized = 0;
bool orte_finalizing = false;
bool orte_debug_flag = false;
int orte_debug_verbosity;
char *orte_prohibited_session_dirs = NULL;
bool orte_create_session_dirs = true;
opal_event_base_t *orte_event_base;
bool orte_event_base_active = true;
bool orte_proc_is_bound = false;
int orte_progress_thread_debug = -1;
#if OPAL_HAVE_HWLOC
hwloc_cpuset_t orte_proc_applied_binding = NULL;
#endif

orte_process_name_t orte_name_wildcard = {ORTE_JOBID_WILDCARD, ORTE_VPID_WILDCARD};

orte_process_name_t orte_name_invalid = {ORTE_JOBID_INVALID, ORTE_VPID_INVALID}; 


#if OPAL_CC_USE_PRAGMA_IDENT
#pragma ident ORTE_IDENT_STRING
#elif OPAL_CC_USE_IDENT
#ident ORTE_IDENT_STRING
#endif
const char orte_version_string[] = ORTE_IDENT_STRING;

int orte_init(int* pargc, char*** pargv, orte_proc_type_t flags)
{
    int ret;
    char *error = NULL;

    if (0 < orte_initialized) {
        /* track number of times we have been called */
        orte_initialized++;
        return ORTE_SUCCESS;
    }
    orte_initialized++;

    /* initialize the opal layer */
    if (ORTE_SUCCESS != (ret = opal_init(pargc, pargv))) {
        error = "opal_init";
        goto error;
    }
    
    /* Convince OPAL to use our naming scheme */
    opal_process_name_print = _process_name_print_for_opal;
    opal_process_name_vpid = _process_name_vpid_for_opal;
    opal_process_name_jobid = _process_name_jobid_for_opal;
    opal_compare_proc = _process_name_compare;

    /* ensure we know the type of proc for when we finalize */
    orte_process_info.proc_type = flags;

    /* setup the locks */
    if (ORTE_SUCCESS != (ret = orte_locks_init())) {
        error = "orte_locks_init";
        goto error;
    }
    
    /* Register all MCA Params */
    if (ORTE_SUCCESS != (ret = orte_register_params())) {
        error = "orte_register_params";
        goto error;
    }
    
    /* setup the orte_show_help system */
    if (ORTE_SUCCESS != (ret = orte_show_help_init())) {
        error = "opal_output_init";
        goto error;
    }
    
    /* register handler for errnum -> string conversion */
    opal_error_register("ORTE", ORTE_ERR_BASE, ORTE_ERR_MAX, orte_err2str);

    /* Ensure the rest of the process info structure is initialized */
    if (ORTE_SUCCESS != (ret = orte_proc_info())) {
        error = "orte_proc_info";
        goto error;
    }

    /* setup the dstore framework */
    if (ORTE_SUCCESS != (ret = mca_base_framework_open(&opal_dstore_base_framework, 0))) {
        ORTE_ERROR_LOG(ret);
        error = "opal_dstore_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = opal_dstore_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "opal_dstore_base_select";
        goto error;
    }
    /* create the handle */
    if (0 > (opal_dstore_internal = opal_dstore.open("INTERNAL"))) {
        error = "opal dstore internal";
        ret = ORTE_ERR_FATAL;
        goto error;
    }

    if (ORTE_PROC_IS_APP) {
        /* we must have the pmix framework setup prior to opening/selecting ESS
         * as some of those components may depend on it */
        if (OPAL_SUCCESS != (ret = mca_base_framework_open(&opal_pmix_base_framework, 0))) {
            ORTE_ERROR_LOG(ret);
            error = "opal_pmix_base_open";
            goto error;
        }
        if (OPAL_SUCCESS != (ret = opal_pmix_base_select())) {
            ORTE_ERROR_LOG(ret);
            error = "opal_pmix_base_select";
            goto error;
        }
    } else if (!ORTE_PROC_IS_TOOL) {
        /* let the pmix server register params */
        pmix_server_register();
    }

    /* open the ESS and select the correct module for this environment */
    if (ORTE_SUCCESS != (ret = mca_base_framework_open(&orte_ess_base_framework, 0))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ess_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_ess_base_select())) {
        error = "orte_ess_base_select";
        goto error;
    }

    if (!ORTE_PROC_IS_APP) {
        /* ORTE tools "block" in their own loop over the event
         * base, so no progress thread is required - apps will
         * start their progress thread in ess_base_std_app.c
         * at the appropriate point
         */
        orte_event_base = opal_event_base;
    }

    /* initialize the RTE for this environment */
    if (ORTE_SUCCESS != (ret = orte_ess.init())) {
        error = "orte_ess_init";
        goto error;
    }
    
    /* All done */
    return ORTE_SUCCESS;
    
 error:
    if (ORTE_ERR_SILENT != ret) {
        orte_show_help("help-orte-runtime",
                       "orte_init:startup:internal-failure",
                       true, error, ORTE_ERROR_NAME(ret), ret);
    }

    return ret;
}
