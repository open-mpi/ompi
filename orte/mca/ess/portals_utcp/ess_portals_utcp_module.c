/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
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

#include <string.h>

#include "opal/util/output.h"

#include "orte/mca/errmgr/base/base.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/portals_utcp/ess_portals_utcp.h"

static int rte_init(char flags);
static int rte_finalize(void);
static void rte_abort(int status, bool report) __opal_attribute_noreturn__;

orte_ess_base_module_t orte_ess_portals_utcp_module = {
    rte_init,
    rte_finalize,
    rte_abort,
    NULL /* ft_event */
};

static int rte_init(char flags)
{
    int rc, i, len, num_procs;
    orte_vpid_t vpid;
    char *vpid_string, *jobid_str;
    char *nidmap_string;

    vpid_string = getenv("PTL_MY_RID");
    nidmap_string = getenv("PTL_NIDMAP");
    if (NULL == vpid_string || NULL == nidmap_string ||
        NULL == getenv("PTL_PIDMAP") || NULL == getenv("PTL_IFACE")) {
        return ORTE_ERR_NOT_FOUND;
    }

    /* Get our process information */

    /* Procs in this environment are directly launched. Hence, there
     * was no mpirun to create a jobid for us, and each app proc is
     * going to have to fend for itself. For now, we assume that the
     * jobid is some arbitrary number (say, 1).
     */
    ORTE_PROC_MY_NAME->jobid = 1; /* not 0, since it has special meaning */
    
    /*  find our vpid assuming range starts at 0 */
    if (ORTE_SUCCESS != (rc = orte_util_convert_string_to_vpid(&vpid, vpid_string))) {
        ORTE_ERROR_LOG(rc);
        return(rc);
    }
    ORTE_PROC_MY_NAME->vpid = vpid;
    
    /*
     * Get the number of procs in the job.  We assume vpids start at 0.  We
     * assume that there are <num : + 1> procs, since the nidmap is a
     * : seperated list of nids, and the utcp reference implementation
     * assumes all will be present
     */
    len = strlen(nidmap_string);
    num_procs = 1;
    for (i = 0 ; i < len ; ++i) {
        if (nidmap_string[i] == ':') num_procs++;
    }
    
    orte_process_info.num_procs = (orte_std_cntr_t) num_procs;

    /* MPI_Init needs the grpcomm framework, so we have to init it */
    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_open())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_select())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* we also want our session directory for shared memory support */
    if (ORTE_SUCCESS != (rc = orte_util_convert_jobid_to_string(&jobid_str, ORTE_PROC_MY_NAME->jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_util_convert_vpid_to_string(&vpid_string, ORTE_PROC_MY_NAME->vpid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_debug_output,
                         "%s setting up session dir with\n\ttmpdir: %s\n\tuser %s\n\thost %s\n\tjobid %s\n\tprocid %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (NULL == orte_process_info.tmpdir_base) ? "UNDEF" : orte_process_info.tmpdir_base,
                         orte_process_info.nodename, jobid_str, vpid_string));
    
    if (ORTE_SUCCESS != (rc = orte_session_dir(true,
                                                orte_process_info.tmpdir_base,
                                                orte_process_info.nodename, NULL,
                                                jobid_str, vpid_string))) {
        if (jobid_str != NULL) free(jobid_str);
        if (vpid_string != NULL) free(vpid_string);
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (NULL != jobid_str) {
        free(jobid_str);
    }
    if (NULL != vpid_string) {
        free(vpid_string);
    }
    
    /* Once the session directory location has been established, set
        the opal_output env file location to be in the
        proc-specific session directory. */
    opal_output_set_output_file_info(orte_process_info.proc_session_dir,
                                     "output-", NULL, NULL);
    
    /* that's all we need here */
    return ORTE_SUCCESS;
}


static int rte_finalize(void)
{
    /* just cleanup the things we used */
    orte_grpcomm_base_close();
    orte_session_dir_finalize(ORTE_PROC_MY_NAME);
    
    /* clean out the global structures */
    orte_proc_info_finalize();
    
    return ORTE_SUCCESS;
}

static void rte_abort(int status, bool report)
{
    exit(status);
}
