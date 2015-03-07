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

#include <errno.h>

static int alps_set_name(void);
static int rte_init(void);
static int rte_finalize(void);

orte_ess_base_module_t orte_ess_alps_module = {
    rte_init,
    rte_finalize,
    orte_ess_base_app_abort,
    NULL /* ft_event */
};

/* Local variables */
static orte_vpid_t starting_vpid = 0;


static int rte_init(void)
{
    int ret, i;
    char *error = NULL;
    char **hosts = NULL;

    OPAL_OUTPUT_VERBOSE((1, orte_ess_base_framework.framework_output,
                         "ess:alps in rte_init"));

    /*
     * shouldn't have been able to open this ess component if
     * process is app proc
     */

    if (ORTE_PROC_IS_APP) {
        error = "mpi rank invoking alps rte_init";
        ret = ORTE_ERR_NOT_SUPPORTED;
        goto fn_fail;
    }

    /* run the prolog */
    if (ORTE_SUCCESS != (ret = orte_ess_base_std_prolog())) {
        error = "orte_ess_base_std_prolog";
        goto fn_fail;
    }

    if (ORTE_SUCCESS != (ret = alps_set_name())) {
        error = "alps_set_name";
        goto fn_fail;
    }

    /*
     * if I am a daemon, complete my setup using the
     * default procedure
     */
    if (ORTE_PROC_IS_DAEMON) {
        if (NULL != orte_node_regex) {
            /* extract the nodes */
            if (ORTE_SUCCESS != (ret =
                orte_regex_extract_node_names(orte_node_regex, &hosts)) ||
                NULL == hosts) {
                error = "orte_regex_extract_node_names";
                goto fn_fail;
            }

            /* find our host in the list */
            for (i=0; NULL != hosts[i]; i++) {
                if (0 == strncmp(hosts[i], orte_process_info.nodename,
                                 strlen(hosts[i]))) {
                    /* correct our vpid - this is probably not necessary with aprun*/
                    ORTE_PROC_MY_NAME->vpid = starting_vpid + i;
                    OPAL_OUTPUT_VERBOSE((1, orte_ess_base_framework.framework_output,
                                         "ess:alps reset name to %s",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                    break;
                }
            }
        }
        if (ORTE_SUCCESS != (ret = orte_ess_base_orted_setup(hosts))) {
            ORTE_ERROR_LOG(ret);
            error = "orte_ess_base_orted_setup";
            goto fn_fail;
        }
        if (NULL != hosts) {
            opal_argv_free(hosts);
        }

        /*
         * now synchronize with aprun.
         */

        if (ORTE_SUCCESS != (ret = orte_ess_alps_sync_start())) {
            error = "orte_ess_alps_sync";
            goto fn_fail;
        }

        ret = ORTE_SUCCESS;
        goto fn_exit;
    }

    if (ORTE_PROC_IS_TOOL) {
        /* otherwise, if I am a tool proc, use that procedure */
        if (ORTE_SUCCESS != (ret = orte_ess_base_tool_setup())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_ess_base_tool_setup";
            goto fn_fail;
        }
        /* as a tool, I don't need a nidmap - so just return now */
        ret = ORTE_SUCCESS;
        goto fn_exit;
    }

   fn_exit:
    return ret;

   fn_fail:
    if (ORTE_ERR_SILENT != ret && !orte_report_silent_errors) {
        orte_show_help("help-orte-runtime.txt",
                       "orte_init:startup:internal-failure",
                       true, error, ORTE_ERROR_NAME(ret), ret);
    }
    goto fn_exit;
}

static int rte_finalize(void)
{
    int ret = ORTE_SUCCESS;

    /* if I am a daemon, finalize using the default procedure */
    if (ORTE_PROC_IS_DAEMON) {
        if (ORTE_SUCCESS != (ret = orte_ess_base_orted_finalize())) {
            ORTE_ERROR_LOG(ret);
            goto fn_exit;
        }

        /* notify alps that we're done */
        if (ORTE_SUCCESS != (ret = orte_ess_alps_sync_complete())) {
            ORTE_ERROR_LOG(ret);
        }

    } else if (ORTE_PROC_IS_TOOL) {
        /* otherwise, if I am a tool proc, use that procedure */
        if (ORTE_SUCCESS != (ret = orte_ess_base_tool_finalize())) {
            ORTE_ERROR_LOG(ret);
        }
    }

   fn_exit:
    return ret;
}

static int alps_set_name(void)
{
    int rc;
    int rank;
    orte_jobid_t jobid;

    if (NULL == orte_ess_base_jobid) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    if (ORTE_SUCCESS != (rc = orte_util_convert_string_to_jobid(&jobid, orte_ess_base_jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (NULL == orte_ess_base_vpid) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    if (ORTE_SUCCESS != (rc = orte_util_convert_string_to_vpid(&starting_vpid,
                                                               orte_ess_base_vpid))) {
        ORTE_ERROR_LOG(rc);
        return(rc);
    }

    ORTE_PROC_MY_NAME->jobid = jobid;

    if (ORTE_SUCCESS != (rc = orte_ess_alps_get_first_rank_on_node(&rank))) {
        ORTE_ERROR_LOG(rc);
        return(rc);
    }

    ORTE_PROC_MY_NAME->vpid = (orte_vpid_t)rank + starting_vpid;

    /* get the num procs as provided in the cmd line param */
    if (ORTE_SUCCESS != (rc = orte_ess_env_get())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}
