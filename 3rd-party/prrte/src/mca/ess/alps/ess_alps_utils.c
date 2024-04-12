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
 * Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2018-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "prte_config.h"
#include "constants.h"

#include "src/util/pmix_argv.h"
#include "src/util/pmix_show_help.h"

#include "src/mca/errmgr/base/base.h"
#include "src/runtime/prte_globals.h"
#include "src/util/name_fns.h"
#include "src/util/proc_info.h"

#include "src/mca/ess/alps/ess_alps.h"
#include "src/mca/ess/base/base.h"
#include "src/mca/ess/ess.h"

/*
 * use the Alps placement file to obtain
 * the global rank of the "first" local rank
 * on the node.
 */

int prte_ess_alps_get_first_rank_on_node(int *first_rank)
{
    int alps_status = 0;
    uint64_t apid;
    size_t alps_count;
    int ret = PRTE_SUCCESS;
    int lli_ret = 0, place_ret;
    alpsAppLayout_t orted_layout;

    if (first_rank == NULL) {
        ret = PRTE_ERR_BAD_PARAM;
        goto fn_exit;
    }

    /*
     * First get our apid
     */

    lli_ret = alps_app_lli_lock();
    if (0 != ret) {
        PMIX_OUTPUT_VERBOSE((20, prte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_app_lli_lock returned %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), lli_ret));
        ret = PRTE_ERR_FILE_WRITE_FAILURE;
        goto fn_exit;
    }

    lli_ret = alps_app_lli_put_request(ALPS_APP_LLI_ALPS_REQ_APID, NULL, 0);
    if (ALPS_APP_LLI_ALPS_STAT_OK != lli_ret) {
        PMIX_OUTPUT_VERBOSE((20, prte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_app_lli_put_request - APID returned %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), lli_ret));
        ret = PRTE_ERR_FILE_WRITE_FAILURE;
        goto fn_exit_w_lock;
    }

    lli_ret = alps_app_lli_get_response(&alps_status, &alps_count);
    if (ALPS_APP_LLI_ALPS_STAT_OK != alps_status) {
        PMIX_OUTPUT_VERBOSE((20, prte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_app_lli_get_response returned %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), alps_status));
        ret = PRTE_ERR_FILE_READ_FAILURE;
        goto fn_exit_w_lock;
    }

    lli_ret = alps_app_lli_get_response_bytes(&apid, sizeof(apid));
    if (ALPS_APP_LLI_ALPS_STAT_OK != lli_ret) {
        PMIX_OUTPUT_VERBOSE((20, prte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_app_lli_get_response_bytes returned %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), lli_ret));
        ret = PRTE_ERR_FILE_READ_FAILURE;
        goto fn_exit_w_lock;
    }

    place_ret = alps_get_placement_info(apid, &orted_layout, NULL, NULL, NULL, NULL, NULL, NULL,
                                        NULL, NULL, NULL);
    if (1 != place_ret) {
        PMIX_OUTPUT_VERBOSE((20, prte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_get_placement_info returned %d (%s)",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), place_ret, strerror(errno)));
        ret = PRTE_ERROR;
        goto fn_exit;
    }

    PMIX_OUTPUT_VERBOSE((2, prte_ess_base_framework.framework_output,
                         "%s ess:alps: alps_get_placement_info returned %d first pe on node is %d",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), place_ret, orted_layout.firstPe));
    *first_rank = orted_layout.firstPe;

fn_exit_w_lock:
    lli_ret = alps_app_lli_unlock();
    if (ALPS_APP_LLI_ALPS_STAT_OK != lli_ret) {
        PMIX_OUTPUT_VERBOSE((20, prte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_app_lli_unlock returned %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), lli_ret));
        ret = PRTE_ERR_FILE_WRITE_FAILURE;
    }

fn_exit:
    return ret;
}

/*
 * Function to check in with apshepherd to say we are a parallel application
 */
int prte_ess_alps_sync_start(void)
{
    int ret = PRTE_SUCCESS;
    int lli_ret = 0;
    int alps_status = 0;
    size_t alps_count;

    lli_ret = alps_app_lli_lock();
    if (0 != ret) {
        PMIX_OUTPUT_VERBOSE((20, prte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_app_lli_lock returned %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), lli_ret));
        ret = PRTE_ERR_FILE_WRITE_FAILURE;
        goto fn_exit;
    }

    lli_ret = alps_app_lli_put_request(ALPS_APP_LLI_ALPS_REQ_START, NULL, 0);
    if (ALPS_APP_LLI_ALPS_STAT_OK != lli_ret) {
        PMIX_OUTPUT_VERBOSE((20, prte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_app_lli_put_request returned %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), lli_ret));
        ret = PRTE_ERR_FILE_WRITE_FAILURE;
        goto fn_exit_w_lock;
    }

    lli_ret = alps_app_lli_get_response(&alps_status, &alps_count);
    if (ALPS_APP_LLI_ALPS_STAT_OK != alps_status) {
        PMIX_OUTPUT_VERBOSE((20, prte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_app_lli_get_response returned %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), alps_status));
        ret = PRTE_ERR_FILE_READ_FAILURE;
        goto fn_exit_w_lock;
    }

fn_exit_w_lock:
    lli_ret = alps_app_lli_unlock();
    if (ALPS_APP_LLI_ALPS_STAT_OK != lli_ret) {
        PMIX_OUTPUT_VERBOSE((20, prte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_app_lli_unlock returned %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), lli_ret));
        ret = PRTE_ERR_FILE_WRITE_FAILURE;
    }

fn_exit:
    return ret;
}

/*
 * Function to check in with apshepherd to say we are a parallel application
 */

int prte_ess_alps_sync_complete(void)
{
    int ret = PRTE_SUCCESS;
    int lli_ret = 0;
    int alps_status = 0;
    size_t alps_count;

    lli_ret = alps_app_lli_lock();
    if (0 != ret) {
        PMIX_OUTPUT_VERBOSE((20, prte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_app_lli_lock returned %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), lli_ret));
        ret = PRTE_ERR_FILE_WRITE_FAILURE;
        goto fn_exit;
    }

    lli_ret = alps_app_lli_put_request(ALPS_APP_LLI_ALPS_REQ_EXITING, NULL, 0);
    if (ALPS_APP_LLI_ALPS_STAT_OK != lli_ret) {
        PMIX_OUTPUT_VERBOSE((20, prte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_app_lli_put_request returned %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), lli_ret));
        ret = PRTE_ERR_FILE_WRITE_FAILURE;
        goto fn_exit_w_lock;
    }

    lli_ret = alps_app_lli_get_response(&alps_status, &alps_count);
    if (ALPS_APP_LLI_ALPS_STAT_OK != alps_status) {
        PMIX_OUTPUT_VERBOSE((20, prte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_app_lli_get_response returned %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), alps_status));
        ret = PRTE_ERR_FILE_READ_FAILURE;
        goto fn_exit_w_lock;
    }

fn_exit_w_lock:
    lli_ret = alps_app_lli_unlock();
    if (ALPS_APP_LLI_ALPS_STAT_OK != lli_ret) {
        PMIX_OUTPUT_VERBOSE((20, prte_ess_base_framework.framework_output,
                             "%s ess:alps: alps_app_lli_unlock returned %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), lli_ret));
        ret = PRTE_ERR_FILE_WRITE_FAILURE;
    }

fn_exit:
    return ret;
}
