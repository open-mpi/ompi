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
 * Copyright (c) 2013-2017 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Hochschule Esslingen.  All rights reserved.
 *
 * Copyright (c) 2015      Cisco Systems, Inc.  All rights reserved.
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
#include "opal/mca/pmix/base/base.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_cr.h"
#include "opal/util/arch.h"
#include "opal/util/proc.h"

#include "orte/mca/oob/base/base.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/rml/base/base.h"
#include "orte/mca/routed/base/base.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/state/base/base.h"
#if OPAL_ENABLE_FT_CR == 1
#include "orte/mca/snapc/base/base.h"
#include "orte/mca/sstore/base/base.h"
#endif
#include "orte/util/proc_info.h"
#include "orte/util/session_dir.h"
#include "orte/util/show_help.h"

#include "orte/runtime/orte_cr.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/ess/base/base.h"


int orte_ess_base_tool_setup(void)
{
    int ret;
    char *error = NULL;
    opal_list_t transports;
    orte_jobid_t jobid;
    orte_vpid_t vpid;

    /* setup the PMIx framework - ensure it skips all non-PMIx components,
     * but do not override anything we were given */
    opal_setenv("OMPI_MCA_pmix", "^s1,s2,cray,isolated", false, &environ);
    if (OPAL_SUCCESS != (ret = mca_base_framework_open(&opal_pmix_base_framework, 0))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_pmix_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = opal_pmix_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "opal_pmix_base_select";
        goto error;
    }
    /* set the event base */
    opal_pmix_base_set_evbase(orte_event_base);

    /* initialize - PMIx may set our name here if we attach to
     * a PMIx server */
    if (NULL != opal_pmix.tool_init) {
        if (OPAL_SUCCESS != (ret = opal_pmix.tool_init(NULL))) {
            ORTE_ERROR_LOG(ret);
            error = "opal_pmix.init";
            goto error;
        }
        ORTE_PROC_MY_NAME->jobid = OPAL_PROC_MY_NAME.jobid;
        ORTE_PROC_MY_NAME->vpid = OPAL_PROC_MY_NAME.vpid;
    } else {
        /* if we connected to a PMIx server, then we were assigned
         * a name that we should use. Otherwise, we have to define
         * one here */
        if (NULL != orte_ess_base_jobid &&
            NULL != orte_ess_base_vpid) {
            opal_output_verbose(2, orte_ess_base_framework.framework_output,
                                "ess:tool:obtaining name from environment");
            if (ORTE_SUCCESS != (ret = orte_util_convert_string_to_jobid(&jobid, orte_ess_base_jobid))) {
                return(ret);
            }
            ORTE_PROC_MY_NAME->jobid = jobid;
            if (ORTE_SUCCESS != (ret = orte_util_convert_string_to_vpid(&vpid, orte_ess_base_vpid))) {
                return(ret);
            }
            ORTE_PROC_MY_NAME->vpid = vpid;
        } else {
            /* If we are a tool with no name, then define it here */
            uint16_t jobfam;
            uint32_t hash32;
            uint32_t bias;

            opal_output_verbose(2, orte_ess_base_framework.framework_output,
                                "ess:tool:computing name");
            /* hash the nodename */
            OPAL_HASH_STR(orte_process_info.nodename, hash32);
            bias = (uint32_t)orte_process_info.pid;
            /* fold in the bias */
            hash32 = hash32 ^ bias;

            /* now compress to 16-bits */
            jobfam = (uint16_t)(((0x0000ffff & (0xffff0000 & hash32) >> 16)) ^ (0x0000ffff & hash32));

            /* set the name */
            ORTE_PROC_MY_NAME->jobid = 0xffff0000 & ((uint32_t)jobfam << 16);
            ORTE_PROC_MY_NAME->vpid = 0;
        }
        /* my name is set, xfer it to the OPAL layer */
        orte_process_info.super.proc_name = *(opal_process_name_t*)ORTE_PROC_MY_NAME;
    }
    orte_process_info.super.proc_hostname = strdup(orte_process_info.nodename);
    orte_process_info.super.proc_flags = OPAL_PROC_ALL_LOCAL;
    orte_process_info.super.proc_arch = opal_local_arch;
    opal_proc_local_set(&orte_process_info.super);

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
    /* open and setup the error manager */
    if (ORTE_SUCCESS != (ret = mca_base_framework_open(&orte_errmgr_base_framework, 0))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_errmgr_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_errmgr_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_errmgr_base_select";
        goto error;
    }
    /* Setup the communication infrastructure */
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

    /* get a conduit for our use - we never route IO over fabric */
    OBJ_CONSTRUCT(&transports, opal_list_t);
    orte_set_attribute(&transports, ORTE_RML_TRANSPORT_TYPE,
                       ORTE_ATTR_LOCAL, orte_mgmt_transport, OPAL_STRING);
    orte_mgmt_conduit = orte_rml.open_conduit(&transports);
    OPAL_LIST_DESTRUCT(&transports);

    /* since I am a tool, then all I really want to do is communicate.
     * So setup communications and be done - finding the HNP
     * to which I want to communicate and setting up a route for
     * that link is my responsibility
     */

    /* we -may- need to know the name of the head
     * of our session directory tree, particularly the
     * tmp base where any other session directories on
     * this node might be located
     */

    ret = orte_session_setup_base(ORTE_PROC_MY_NAME);
    if (ORTE_SUCCESS != ret ) {
        ORTE_ERROR_LOG(ret);
        error = "define session dir names";
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
        /* if we were given an HNP, then also setup the PLM in case this
         * tool wants to request that we spawn something for it */
        if (ORTE_SUCCESS != (ret = mca_base_framework_open(&orte_plm_base_framework, 0))) {
            ORTE_ERROR_LOG(ret);
            error = "orte_plm_base_open";
            goto error;
        }
        /* we don't select the plm framework as we only want the
         * base proxy functions */
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
    mca_base_framework_close(&orte_snapc_base_framework);
    mca_base_framework_close(&orte_sstore_base_framework);
#endif

    orte_rml.close_conduit(orte_mgmt_conduit);

    /* if I am a tool, then all I will have done is
     * a very small subset of orte_init - ensure that
     * I only back those elements out
     */
    if (NULL != orte_process_info.my_hnp_uri) {
        (void) mca_base_framework_close(&orte_iof_base_framework);
    }
    (void) mca_base_framework_close(&orte_routed_base_framework);
    (void) mca_base_framework_close(&orte_rml_base_framework);
    (void) mca_base_framework_close(&orte_errmgr_base_framework);

    (void) mca_base_framework_close(&opal_pmix_base_framework);

    return ORTE_SUCCESS;
}
