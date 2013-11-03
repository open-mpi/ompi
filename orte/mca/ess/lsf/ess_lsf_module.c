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
 * Copyright (c) 2007-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#include <ctype.h>

#include <lsf/lsbatch.h>

#include "opal/util/opal_environ.h"

#include "orte/util/show_help.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/nidmap.h"
#include "orte/util/regex.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/lsf/ess_lsf.h"

static int lsf_set_name(void);

static int rte_init(void);
static int rte_finalize(void);

orte_ess_base_module_t orte_ess_lsf_module = {
    rte_init,
    rte_finalize,
    orte_ess_base_app_abort,
    NULL /* ft_event */
};

/*
 * Local variables
 */
static orte_node_rank_t my_node_rank=ORTE_NODE_RANK_INVALID;


static int rte_init(void)
{
    int ret;
    char *error = NULL;
    char **hosts = NULL;

    /* run the prolog */
    if (ORTE_SUCCESS != (ret = orte_ess_base_std_prolog())) {
        error = "orte_ess_base_std_prolog";
        goto error;
    }
    
    /* Start by getting a unique name */
    lsf_set_name();
    
    /* if I am a daemon, complete my setup using the
     * default procedure
     */
    if (ORTE_PROC_IS_DAEMON) {
        if (NULL != orte_node_regex) {
            /* extract the nodes */
            if (ORTE_SUCCESS != (ret = orte_regex_extract_node_names(orte_node_regex, &hosts))) {
                error = "orte_regex_extract_node_names";
                goto error;
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
    
    /* otherwise, I must be an application process - use
     * the default procedure to finish my setup
     */
    if (ORTE_SUCCESS != (ret = orte_ess_base_app_setup(false))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ess_base_app_setup";
        goto error;
    }
    
    /* setup the nidmap arrays */
    if (ORTE_SUCCESS != (ret = orte_util_nidmap_init(orte_process_info.sync_buf))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_util_nidmap_init";
        goto error;
    }
    
    return ORTE_SUCCESS;
    
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
            return ret;
        }
    } else if (ORTE_PROC_IS_TOOL) {
        /* otherwise, if I am a tool proc, use that procedure */
        if (ORTE_SUCCESS != (ret = orte_ess_base_tool_finalize())) {
            ORTE_ERROR_LOG(ret);
        }
        /* as a tool, I didn't create a nidmap - so just return now */
        return ret;
    } else {
        /* otherwise, I must be an application process
         * use the default procedure to finish
         */
        if (ORTE_SUCCESS != (ret = orte_ess_base_app_finalize())) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    }

    /* deconstruct my nidmap and jobmap arrays */
    orte_util_nidmap_finalize();

    return ORTE_SUCCESS;;
}

static int lsf_set_name(void)
{
    int rc;
    int lsf_nodeid;
    orte_jobid_t jobid;
    orte_vpid_t vpid;
      
    if (NULL ==orte_ess_base_jobid) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    if (ORTE_SUCCESS != (rc = orte_util_convert_string_to_jobid(&jobid, orte_ess_base_jobid))) {
        ORTE_ERROR_LOG(rc);
        return(rc);
    }
    ORTE_PROC_MY_NAME->jobid = jobid;

    /* get the vpid from the nodeid */
    if (NULL == orte_ess_base_vpid) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    if (ORTE_SUCCESS != (rc = orte_util_convert_string_to_vpid(&vpid, orte_ess_base_vpid))) {
        ORTE_ERROR_LOG(rc);
        return(rc);
    }
    lsf_nodeid = atoi(getenv("LSF_PM_TASKID"));
    opal_output_verbose(1, orte_ess_base_framework.framework_output,
                        "ess:lsf found LSF_PM_TASKID set to %d",
                        lsf_nodeid);
    ORTE_PROC_MY_NAME->vpid = vpid + lsf_nodeid - 1;

    /* get the non-name common environmental variables */
    if (ORTE_SUCCESS != (rc = orte_ess_env_get())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ORTE_SUCCESS;
}
