/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "orte/util/show_help.h"
#include "opal/util/argv.h"

#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
#include "orte/util/regex.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/alps/ess_alps.h"

/*
 * use the Alps placement file to obtain
 * the global rank of the "first" local rank
 * on the node.  
 */

int 
orte_ess_alps_get_first_rank_on_node(int *first_rank)
{
    int alps_status = 0;
    uint64_t apid;
    size_t alps_count;
    int ret = ORTE_SUCCESS;
    int lli_ret = 0, place_ret;
    alpsAppLayout_t orted_layout;

    if (first_rank == NULL) {
        ret = ORTE_ERR_BAD_PARAM;
        goto fn_exit;
    }

    /*
     * First get our apid
     */

    lli_ret = alps_app_lli_lock();
    if (0 != ret) {
        OPAL_OUTPUT_VERBOSE((20, orte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_app_lli_lock returned %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), lli_ret));
        ret = ORTE_ERR_FILE_WRITE_FAILURE;
        goto fn_exit;
    }

    lli_ret = alps_app_lli_put_request(ALPS_APP_LLI_ALPS_REQ_APID, NULL, 0);
    if (ALPS_APP_LLI_ALPS_STAT_OK != lli_ret) {
        OPAL_OUTPUT_VERBOSE((20, orte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_app_lli_put_request - APID returned %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), lli_ret));
        ret = ORTE_ERR_FILE_WRITE_FAILURE;
        goto fn_exit_w_lock;
    }

    lli_ret = alps_app_lli_get_response (&alps_status, &alps_count);
    if (ALPS_APP_LLI_ALPS_STAT_OK != alps_status) {
        OPAL_OUTPUT_VERBOSE((20, orte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_app_lli_get_response returned %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), alps_status));
        ret = ORTE_ERR_FILE_READ_FAILURE;
        goto fn_exit_w_lock;
    }

    lli_ret = alps_app_lli_get_response_bytes (&apid, sizeof(apid));
    if (ALPS_APP_LLI_ALPS_STAT_OK != lli_ret) {
        OPAL_OUTPUT_VERBOSE((20, orte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_app_lli_get_response_bytes returned %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), lli_ret));
        ret = ORTE_ERR_FILE_READ_FAILURE;
        goto fn_exit_w_lock;
    }

    place_ret = alps_get_placement_info(apid,
                                        &orted_layout,
                                        NULL,
                                        NULL,
                                        NULL,
                                        NULL,
                                        NULL,
                                        NULL,
                                        NULL,
                                        NULL,
                                        NULL);
    if (1 != place_ret) {
        OPAL_OUTPUT_VERBOSE((20, orte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_get_placement_info returned %d (%s)",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), place_ret, strerror(errno)));
        ret = ORTE_ERROR;
        goto fn_exit;
    }

    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_framework.framework_output,
                           "%s ess:alps: alps_get_placement_info returned %d first pe on node is %d",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), place_ret, orted_layout.firstPe));
    *first_rank = orted_layout.firstPe;

   fn_exit_w_lock:
    lli_ret = alps_app_lli_unlock();
    if (ALPS_APP_LLI_ALPS_STAT_OK != lli_ret) {
        OPAL_OUTPUT_VERBOSE((20, orte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_app_lli_unlock returned %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), lli_ret));
        ret = ORTE_ERR_FILE_WRITE_FAILURE;
    }

   fn_exit:
    return ret;
}

/*
 * Function to check in with apshepherd to say we are a parallel application
 */
int
orte_ess_alps_sync_start(void)
{
    int ret = ORTE_SUCCESS;
    int lli_ret = 0;
    int alps_status = 0;
    size_t alps_count;

    lli_ret = alps_app_lli_lock();
    if (0 != ret) {
        OPAL_OUTPUT_VERBOSE((20, orte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_app_lli_lock returned %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), lli_ret));
        ret = ORTE_ERR_FILE_WRITE_FAILURE;
        goto fn_exit;
    }

    lli_ret = alps_app_lli_put_request(ALPS_APP_LLI_ALPS_REQ_START, NULL, 0);
    if (ALPS_APP_LLI_ALPS_STAT_OK != lli_ret) {
        OPAL_OUTPUT_VERBOSE((20, orte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_app_lli_put_request returned %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), lli_ret));
        ret = ORTE_ERR_FILE_WRITE_FAILURE;
        goto fn_exit_w_lock;
    }

    lli_ret = alps_app_lli_get_response (&alps_status, &alps_count);
    if (ALPS_APP_LLI_ALPS_STAT_OK != alps_status) {
        OPAL_OUTPUT_VERBOSE((20, orte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_app_lli_get_response returned %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), alps_status));
        ret = ORTE_ERR_FILE_READ_FAILURE;
        goto fn_exit_w_lock;
    }

   fn_exit_w_lock:
    lli_ret = alps_app_lli_unlock();
    if (ALPS_APP_LLI_ALPS_STAT_OK != lli_ret) {
        OPAL_OUTPUT_VERBOSE((20, orte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_app_lli_unlock returned %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), lli_ret));
        ret = ORTE_ERR_FILE_WRITE_FAILURE;
    }

   fn_exit:
    return ret;
}

/*
 * Function to check in with apshepherd to say we are a parallel application
 */

int
orte_ess_alps_sync_complete(void)
{
    int ret = ORTE_SUCCESS;
    int lli_ret = 0;
    int alps_status = 0;
    size_t alps_count;

    lli_ret = alps_app_lli_lock();
    if (0 != ret) {
        OPAL_OUTPUT_VERBOSE((20, orte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_app_lli_lock returned %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), lli_ret));
        ret = ORTE_ERR_FILE_WRITE_FAILURE;
        goto fn_exit;
    }

    lli_ret = alps_app_lli_put_request(ALPS_APP_LLI_ALPS_REQ_EXITING, NULL, 0);
    if (ALPS_APP_LLI_ALPS_STAT_OK != lli_ret) {
        OPAL_OUTPUT_VERBOSE((20, orte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_app_lli_put_request returned %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), lli_ret));
        ret = ORTE_ERR_FILE_WRITE_FAILURE;
        goto fn_exit_w_lock;
    }

    lli_ret = alps_app_lli_get_response (&alps_status, &alps_count);
    if (ALPS_APP_LLI_ALPS_STAT_OK != alps_status) {
        OPAL_OUTPUT_VERBOSE((20, orte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_app_lli_get_response returned %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), alps_status));
        ret = ORTE_ERR_FILE_READ_FAILURE;
        goto fn_exit_w_lock;
    }

   fn_exit_w_lock:
    lli_ret = alps_app_lli_unlock();
    if (ALPS_APP_LLI_ALPS_STAT_OK != lli_ret) {
        OPAL_OUTPUT_VERBOSE((20, orte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_app_lli_unlock returned %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), lli_ret));
        ret = ORTE_ERR_FILE_WRITE_FAILURE;
    }

   fn_exit:
    return ret;
}


