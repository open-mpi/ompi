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
 * Copyright (c) 2017-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting.  All rights reserved.
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
#include "src/pmix/pmix-internal.h"
#include "src/runtime/prte_globals.h"
#include "src/util/name_fns.h"
#include "src/util/proc_info.h"

#include "src/mca/ess/alps/ess_alps.h"
#include "src/mca/ess/base/base.h"
#include "src/mca/ess/ess.h"

#include <errno.h>

static int alps_set_name(void);
static int rte_init(int argc, char **argv);
static int rte_finalize(void);

prte_ess_base_module_t prte_ess_alps_module = {
    .init = rte_init,
    .finalize = rte_finalize
};

/* Local variables */
static pmix_rank_t starting_vpid = 0;

static int rte_init(int argc, char **argv)
{
    int ret;
    char *error = NULL;

    PMIX_OUTPUT_VERBOSE((1, prte_ess_base_framework.framework_output, "ess:alps in rte_init"));

    /* run the prolog */
    if (PRTE_SUCCESS != (ret = prte_ess_base_std_prolog())) {
        error = "prte_ess_base_std_prolog";
        goto fn_fail;
    }

    if (PRTE_SUCCESS != (ret = alps_set_name())) {
        error = "alps_set_name";
        goto fn_fail;
    }

    if (PRTE_SUCCESS != (ret = prte_ess_base_prted_setup())) {
        PRTE_ERROR_LOG(ret);
        error = "prte_ess_base_prted_setup";
        goto fn_fail;
    }

    /*
     * now synchronize with aprun.
     */

    if (PRTE_SUCCESS != (ret = prte_ess_alps_sync_start())) {
        error = "prte_ess_alps_sync";
        goto fn_fail;
    }

    return PRTE_SUCCESS;

fn_fail:
    if (PRTE_ERR_SILENT != ret && !prte_report_silent_errors) {
        pmix_show_help("help-prte-runtime.txt", "prte_init:startup:internal-failure", true, error,
                       PRTE_ERROR_NAME(ret), ret);
    }
    return ret;
}

static int rte_finalize(void)
{
    int ret = PRTE_SUCCESS;

    if (PRTE_SUCCESS != (ret = prte_ess_base_prted_finalize())) {
        PRTE_ERROR_LOG(ret);
        goto fn_exit;
    }

    /* notify alps that we're done */
    if (PRTE_SUCCESS != (ret = prte_ess_alps_sync_complete())) {
        PRTE_ERROR_LOG(ret);
    }

fn_exit:
    return ret;
}

static int alps_set_name(void)
{
    int rc;
    int rank;

    if (NULL == prte_ess_base_nspace) {
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        return PRTE_ERR_NOT_FOUND;
    }

    PMIX_LOAD_NSPACE(PRTE_PROC_MY_NAME->nspace, prte_ess_base_nspace);

    if (NULL == prte_ess_base_vpid) {
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        return PRTE_ERR_NOT_FOUND;
    }
    starting_vpid = strtoul(prte_ess_base_vpid, NULL, 10);

    if (PRTE_SUCCESS != (rc = prte_ess_alps_get_first_rank_on_node(&rank))) {
        PRTE_ERROR_LOG(rc);
        return (rc);
    }

    PRTE_PROC_MY_NAME->rank = (pmix_rank_t) rank + starting_vpid;

    /* get the num procs as provided in the cmd line param */
    prte_process_info.num_daemons = prte_ess_base_num_procs;

    return PRTE_SUCCESS;
}
