/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013      Intel, Inc. All rights reserved.
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

#include "opal/mca/event/event.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_cr.h"

#include "orte/mca/oob/base/base.h"
#include "orte/mca/rml/base/base.h"
#include "orte/mca/routed/base/base.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/state/base/base.h"
#if OPAL_ENABLE_FT_CR == 1
#include "orte/mca/snapc/base/base.h"
#endif
#include "orte/util/proc_info.h"
#include "orte/util/session_dir.h"
#include "orte/util/show_help.h"

#include "orte/runtime/orte_cr.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/ess/base/base.h"

int orte_ess_base_tool_setup(void)
{
    int ret;
    char *error = NULL;

    if (NULL != orte_process_info.my_hnp_uri) {
        /* if we were given an HNP, then we were launched
         * by mpirun in some fashion - in this case, we want
         * to look like an application as well as being a tool.
         * Need to do this before opening the routed framework
         * so it will do the right things.
         */
        orte_process_info.proc_type |= ORTE_PROC_NON_MPI;
    }
    
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

    /* Setup the communication infrastructure */
    if (ORTE_SUCCESS != (ret = mca_base_framework_open(&orte_oob_base_framework, 0))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_oob_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_oob_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_oob_base_select";
        goto error;
    }

    /* Runtime Messaging Layer */
    if (ORTE_SUCCESS != (ret = mca_base_framework_open(&orte_rml_base_framework, 0))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rml_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_rml_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rml_base_select";
        goto error;
    }
    /* Routed system */
    if (ORTE_SUCCESS != (ret = mca_base_framework_open(&orte_routed_base_framework, 0))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rml_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_routed_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_routed_base_select";
        goto error;
    }
    
    /* since I am a tool, then all I really want to do is communicate.
     * So setup communications and be done - finding the HNP
     * to which I want to communicate and setting up a route for
     * that link is my responsibility
     */
    
    /* enable communication via the rml */
    if (ORTE_SUCCESS != (ret = orte_rml.enable_comm())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rml.enable_comm";
        goto error;
    }
    
    /* we -may- need to know the name of the head
     * of our session directory tree, particularly the
     * tmp base where any other session directories on
     * this node might be located
     */
    if (ORTE_SUCCESS != (ret = orte_session_dir_get_name(NULL,
                                                         &orte_process_info.tmpdir_base,
                                                         &orte_process_info.top_session_dir,
                                                         orte_process_info.nodename, NULL, NULL))) {
        ORTE_ERROR_LOG(ret);
        error = "define session dir names";
        goto error;
    }
    
    /* setup the routed info - the selected routed component
     * will know what to do. 
     */
    if (ORTE_SUCCESS != (ret = orte_routed.init_routes(ORTE_PROC_MY_NAME->jobid, NULL))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_routed.init_routes";
        goto error;
    }
    
    /* setup I/O forwarding system - must come after we init routes */
    if (NULL != orte_process_info.my_hnp_uri) {
        /* only do this if we were given an HNP */
        if (ORTE_SUCCESS != (ret = mca_base_framework_open(&orte_iof_base_framework, 0))) {
            ORTE_ERROR_LOG(ret);
            error = "orte_iof_base_open";
            goto error;
        }
        if (ORTE_SUCCESS != (ret = orte_iof_base_select())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_iof_base_select";
            goto error;
        }
    }
    
#if OPAL_ENABLE_FT_CR == 1
    /*
     * Setup the SnapC
     */
    if (ORTE_SUCCESS != (ret = orte_snapc_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_snapc_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_snapc_base_select(ORTE_PROC_IS_HNP, !ORTE_PROC_IS_DAEMON))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_snapc_base_select";
        goto error;
    }
    
    /* Tools do not need all the OPAL CR stuff */
    opal_cr_set_enabled(false);
#endif
    
    return ORTE_SUCCESS;
    
 error:
    orte_show_help("help-orte-runtime.txt",
                   "orte_init:startup:internal-failure",
                   true, error, ORTE_ERROR_NAME(ret), ret);
    
    return ret;
}

int orte_ess_base_tool_finalize(void)
{
    orte_wait_finalize();

#if OPAL_ENABLE_FT_CR == 1
    orte_snapc_base_close();
#endif

    /* if I am a tool, then all I will have done is
     * a very small subset of orte_init - ensure that
     * I only back those elements out
     */
    if (NULL != orte_process_info.my_hnp_uri) {
        (void) mca_base_framework_close(&orte_iof_base_framework);
    }
    (void) mca_base_framework_close(&orte_routed_base_framework);
    (void) mca_base_framework_close(&orte_rml_base_framework);

    return ORTE_SUCCESS;    
}
