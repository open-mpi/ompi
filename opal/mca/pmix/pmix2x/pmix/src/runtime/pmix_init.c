/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2010-2015 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2017 Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file **/

#include <src/include/pmix_config.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include PMIX_EVENT_HEADER
#include "event2/thread.h"

#include <pmix_rename.h>

#include "src/util/output.h"
#include "src/util/show_help.h"
#include "src/mca/base/base.h"
#include "src/mca/base/pmix_mca_base_var.h"
#include "src/mca/bfrops/base/base.h"
#include "src/mca/gds/base/base.h"
#include "src/mca/pif/base/base.h"
#include "src/mca/pinstalldirs/base/base.h"
#include "src/mca/pnet/base/base.h"
#include "src/mca/psec/base/base.h"
#include "src/mca/ptl/base/base.h"

#include "src/event/pmix_event.h"
#include "src/include/types.h"
#include "src/util/error.h"
#include "src/util/keyval_parse.h"

#include "src/runtime/pmix_rte.h"
#include "src/runtime/pmix_progress_threads.h"

#if PMIX_CC_USE_PRAGMA_IDENT
#pragma ident PMIX_IDENT_STRING
#elif PMIX_CC_USE_IDENT
#ident PMIX_IDENT_STRING
#endif
const char pmix_version_string[] = PMIX_IDENT_STRING;

int pmix_initialized = 0;
bool pmix_init_called = false;
/* we have to export the pmix_globals object so
 * all plugins can access it. However, it is included
 * in the pmix_rename.h file for external protection */
PMIX_EXPORT pmix_globals_t pmix_globals = {
    .init_cntr = 0,
    .mypeer = NULL,
    .proc_type = PMIX_PROC_UNDEF,
    .pindex = 0,
    .evbase = NULL,
    .external_evbase = false,
    .debug_output = -1,
    .connected = false,
    .commits_pending = false,
    .mygds = NULL
};


int pmix_rte_init(pmix_proc_type_t type,
                  pmix_info_t info[], size_t ninfo,
                  pmix_ptl_cbfunc_t cbfunc)
{
    int ret, debug_level;
    char *error = NULL, *evar;
    size_t n;

    if( ++pmix_initialized != 1 ) {
        if( pmix_initialized < 1 ) {
            return PMIX_ERROR;
        }
        return PMIX_SUCCESS;
    }

    #if PMIX_NO_LIB_DESTRUCTOR
        if (pmix_init_called) {
            /* can't use show_help here */
            fprintf (stderr, "pmix_init: attempted to initialize after finalize without compiler "
                     "support for either __attribute__(destructor) or linker support for -fini -- process "
                     "will likely abort\n");
            return PMIX_ERR_NOT_SUPPORTED;
        }
    #endif

    pmix_init_called = true;

    /* initialize the output system */
    if (!pmix_output_init()) {
        return PMIX_ERROR;
    }

    /* initialize install dirs code */
    if (PMIX_SUCCESS != (ret = pmix_mca_base_framework_open(&pmix_pinstalldirs_base_framework, 0))) {
        fprintf(stderr, "pmix_pinstalldirs_base_open() failed -- process will likely abort (%s:%d, returned %d instead of PMIX_SUCCESS)\n",
                __FILE__, __LINE__, ret);
        return ret;
    }

    /* initialize the help system */
    pmix_show_help_init();

    /* keyval lex-based parser */
    if (PMIX_SUCCESS != (ret = pmix_util_keyval_parse_init())) {
        error = "pmix_util_keyval_parse_init";
        goto return_error;
    }

    /* Setup the parameter system */
    if (PMIX_SUCCESS != (ret = pmix_mca_base_var_init())) {
        error = "mca_base_var_init";
        goto return_error;
    }

    /* register params for pmix */
    if (PMIX_SUCCESS != (ret = pmix_register_params())) {
        error = "pmix_register_params";
        goto return_error;
    }

    /* initialize the mca */
    if (PMIX_SUCCESS != (ret = pmix_mca_base_open())) {
        error = "mca_base_open";
        goto return_error;
    }

    /* setup the globals structure */
    pmix_globals.proc_type = type;
    memset(&pmix_globals.myid, 0, sizeof(pmix_proc_t));
    PMIX_CONSTRUCT(&pmix_globals.events, pmix_events_t);
    /* get our effective id's */
    pmix_globals.uid = geteuid();
    pmix_globals.gid = getegid();
    /* see if debug is requested */
    if (NULL != (evar = getenv("PMIX_DEBUG"))) {
        debug_level = strtol(evar, NULL, 10);
        pmix_globals.debug_output = pmix_output_open(NULL);
        pmix_output_set_verbosity(pmix_globals.debug_output, debug_level);
    }
    /* create our peer object */
    pmix_globals.mypeer = PMIX_NEW(pmix_peer_t);
    if (NULL == pmix_globals.mypeer) {
        ret = PMIX_ERR_NOMEM;
        goto return_error;
    }
    /* create an nspace object for ourselves - we will
     * fill in the nspace name later */
    pmix_globals.mypeer->nptr = PMIX_NEW(pmix_nspace_t);
    if (NULL == pmix_globals.mypeer->nptr) {
        PMIX_RELEASE(pmix_globals.mypeer);
        ret = PMIX_ERR_NOMEM;
        goto return_error;
    }

    /* scan incoming info for directives */
    if (NULL != info) {
        for (n=0; n < ninfo; n++) {
            if (0 == strcmp(PMIX_EVENT_BASE, info[n].key)) {
                pmix_globals.evbase = (pmix_event_base_t*)info[n].value.data.ptr;
                pmix_globals.external_evbase = true;
            }
        }
    }

    /* the choice of modules to use when communicating with a peer
     * will be done by the individual init functions and at the
     * time of connection to that peer */

    /* open the bfrops and select the active plugins */
    if( PMIX_SUCCESS != (ret = pmix_mca_base_framework_open(&pmix_bfrops_base_framework, 0)) ) {
        error = "pmix_bfrops_base_open";
        goto return_error;
    }
    if( PMIX_SUCCESS != (ret = pmix_bfrop_base_select()) ) {
        error = "pmix_bfrops_base_select";
        goto return_error;
    }

    /* open the ptl and select the active plugins */
    if( PMIX_SUCCESS != (ret = pmix_mca_base_framework_open(&pmix_ptl_base_framework, 0)) ) {
        error = "pmix_ptl_base_open";
        goto return_error;
    }
    if( PMIX_SUCCESS != (ret = pmix_ptl_base_select()) ) {
        error = "pmix_ptl_base_select";
        goto return_error;
    }
    /* set the notification callback function */
    if (PMIX_SUCCESS != (ret = pmix_ptl_base_set_notification_cbfunc(cbfunc))) {
        error = "pmix_ptl_set_notification_cbfunc";
        goto return_error;
    }

    /* open the psec and select the active plugins */
    if (PMIX_SUCCESS != (ret = pmix_mca_base_framework_open(&pmix_psec_base_framework, 0))) {
        error = "pmix_psec_base_open";
        goto return_error;
    }
    if (PMIX_SUCCESS != (ret = pmix_psec_base_select())) {
        error = "pmix_psec_base_select";
        goto return_error;
    }

    /* open the gds and select the active plugins */
    if( PMIX_SUCCESS != (ret = pmix_mca_base_framework_open(&pmix_gds_base_framework, 0)) ) {
        error = "pmix_gds_base_open";
        goto return_error;
    }
    if( PMIX_SUCCESS != (ret = pmix_gds_base_select(info, ninfo)) ) {
        error = "pmix_gds_base_select";
        goto return_error;
    }

    /* initialize pif framework */
    if (PMIX_SUCCESS != (ret = pmix_mca_base_framework_open(&pmix_pif_base_framework, 0))) {
        error = "pmix_pif_base_open";
        return ret;
    }

    /* open the pnet and select the active modules for this environment */
    if (PMIX_SUCCESS != (ret = pmix_mca_base_framework_open(&pmix_pnet_base_framework, 0))) {
        error = "pmix_pnet_base_open";
        goto return_error;
    }
    if (PMIX_SUCCESS != (ret = pmix_pnet_base_select())) {
        error = "pmix_pnet_base_select";
        goto return_error;
    }

    /* tell libevent that we need thread support */
    pmix_event_use_threads();

    /* if an external event base wasn't provide, create one */
    if (!pmix_globals.external_evbase) {
        /* create an event base and progress thread for us */
        if (NULL == (pmix_globals.evbase = pmix_progress_thread_init(NULL))) {
            error = "progress thread";
            ret = PMIX_ERROR;
            goto return_error;
        }
    }

    return PMIX_SUCCESS;

 return_error:
    pmix_show_help( "help-pmix-runtime.txt",
                    "pmix_init:startup:internal-failure", true,
                    error, ret );
    return ret;
}
