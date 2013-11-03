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

#if defined(HAVE_CNOS_MPI_OS_H)
#include "cnos_mpi_os.h"
#elif defined(HAVE_CATAMOUNT_CNOS_MPI_OS_H)
#include "catamount/cnos_mpi_os.h"
#endif

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

    /* run the prolog */
    if (ORTE_SUCCESS != (ret = orte_ess_base_std_prolog())) {
        error = "orte_ess_base_std_prolog";
        goto error;
    }

    /* Start by getting a unique name */
    alps_set_name();

    /* if I am a daemon, complete my setup using the
     * default procedure
     */
    if (ORTE_PROC_IS_DAEMON) {
        if (NULL != orte_node_regex) {
            /* extract the nodes */
            if (ORTE_SUCCESS != (ret =
                orte_regex_extract_node_names(orte_node_regex, &hosts)) ||
                NULL == hosts) {
                error = "orte_regex_extract_node_names";
                goto error;
            }

            /* find our host in the list */
            for (i=0; NULL != hosts[i]; i++) {
                if (0 == strncmp(hosts[i], orte_process_info.nodename,
                                 strlen(hosts[i]))) {
                    /* correct our vpid */
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
            goto error;
        }
        opal_argv_free(hosts);
        return ORTE_SUCCESS;
    }
    if (ORTE_PROC_IS_TOOL) {
        /* otherwise, if I am a tool proc, use that procedure */
        if (ORTE_SUCCESS != (ret = orte_ess_base_tool_setup())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_ess_base_tool_setup";
            goto error;
        }
        /* as a tool, I don't need a nidmap - so just return now */
        return ORTE_SUCCESS;
    }

    /* otherwise, I must be a direct-launched application process - if
     * we were selected, then PMI isn't available and we cannot
     * possibly run
     */

    return ORTE_ERR_NOT_SUPPORTED;

error:
    if (ORTE_ERR_SILENT != ret && !orte_report_silent_errors) {
        orte_show_help("help-orte-runtime.txt",
                       "orte_init:startup:internal-failure",
                       true, error, ORTE_ERROR_NAME(ret), ret);
    }

    return ret;
}

static int rte_finalize(void)
{
    int ret;

    /* if I am a daemon, finalize using the default procedure */
    if (ORTE_PROC_IS_DAEMON) {
        if (ORTE_SUCCESS != (ret = orte_ess_base_orted_finalize())) {
            ORTE_ERROR_LOG(ret);
        }
        return ret;
    } else if (ORTE_PROC_IS_TOOL) {
        /* otherwise, if I am a tool proc, use that procedure */
        if (ORTE_SUCCESS != (ret = orte_ess_base_tool_finalize())) {
            ORTE_ERROR_LOG(ret);
        }
        return ret;
    }
    return ORTE_SUCCESS;
}

static char *orte_ess_alps_jobid = NULL;

static int alps_set_name(void)
{
    int rc;
    orte_jobid_t jobid;

    OPAL_OUTPUT_VERBOSE((1, orte_ess_base_framework.framework_output,
                         "ess:alps setting name"));

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
    ORTE_PROC_MY_NAME->vpid = (orte_vpid_t)cnos_get_rank() + starting_vpid;

    OPAL_OUTPUT_VERBOSE((1, orte_ess_base_framework.framework_output,
                         "ess:alps set name to %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* get the num procs as provided in the cmd line param */
    if (ORTE_SUCCESS != (rc = orte_ess_env_get())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (orte_process_info.max_procs < orte_process_info.num_procs) {
        orte_process_info.max_procs = orte_process_info.num_procs;
    }

    return ORTE_SUCCESS;
}
