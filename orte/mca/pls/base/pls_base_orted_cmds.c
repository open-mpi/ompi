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
#include "orte/orte_constants.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "opal/event/event.h"
#include "opal/threads/condition.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/trace.h"

#include "orte/dss/dss.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmgr/rmgr.h"

#include "orte/mca/pls/base/base.h"
#include "orte/mca/pls/base/pls_private.h"

static int get_jobids(orte_jobid_t **jobs, orte_std_cntr_t *num_jobs, bool *allocated,
                      orte_jobid_t job, opal_list_t *attrs)
{
    int rc;
    
    if (NULL != orte_rmgr.find_attribute(attrs, ORTE_NS_USE_JOB_FAMILY)) {
        /* need to include entire job family */
        if (ORTE_SUCCESS != (rc = orte_ns.get_job_family(jobs, num_jobs, job))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        *allocated = true;
    } else if (NULL != orte_rmgr.find_attribute(attrs, ORTE_NS_INCLUDE_DESCENDANTS)) {
        /* need to include all descendants in list */
        if (ORTE_SUCCESS != (rc = orte_ns.get_job_descendants(jobs, num_jobs, job))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        *allocated = true;
    } else if (NULL != orte_rmgr.find_attribute(attrs, ORTE_NS_INCLUDE_CHILDREN)) {
        /* just include the direct children of the job */
        if (ORTE_SUCCESS != (rc = orte_ns.get_job_children(jobs, num_jobs, job))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        *allocated = true;
    } else {
        /* just want daemons for this one job */
        *jobs = &job;
        *num_jobs = 1;
        *allocated = false;
    }
    
    return ORTE_SUCCESS;
}

int orte_pls_base_orted_exit(struct timeval *timeout, opal_list_t *attrs)
{
    int rc;
    orte_buffer_t cmd;
    orte_daemon_cmd_flag_t command;
    
    OPAL_TRACE(1);
    
    OBJ_CONSTRUCT(&cmd, orte_buffer_t);
    
    /* Default: tell the daemons to exit IF they are not part of a virtual
     * machine - i.e., they are not persistent
     */
    command = ORTE_DAEMON_EXIT_CMD;
    
    if (NULL != orte_rmgr.find_attribute(attrs, ORTE_DAEMON_HARD_KILL)) {
        /* order daemons to exit NO MATTER WHAT */
        command = ORTE_DAEMON_HALT_VM_CMD;
    }
    
    /* pack the command */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        return rc;
    }
    
    /* send it! */
    if (ORTE_SUCCESS != (rc = orte_grpcomm.xcast(0, &cmd, ORTE_RML_TAG_DAEMON))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_DESTRUCT(&cmd);
    
    return rc;
}


int orte_pls_base_orted_kill_local_procs(orte_jobid_t job, struct timeval *timeout, opal_list_t *attrs)
{
    int rc;
    orte_buffer_t cmd;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_KILL_LOCAL_PROCS;
    orte_jobid_t *jobs;
    orte_std_cntr_t num_jobs;
    bool allocated;
    
    OPAL_TRACE(1);
    
    /* get the array of jobs we need to kill */
    if (ORTE_SUCCESS != (rc = get_jobids(&jobs, &num_jobs, &allocated, job, attrs))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    OBJ_CONSTRUCT(&cmd, orte_buffer_t);
    
    /* pack the command */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        if (allocated) free(jobs);
        return rc;
    }
    
    /* pack the number of jobids */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &num_jobs, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        if (allocated) free(jobs);
        return rc;
    }
    
    /* pack the array of jobids */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, jobs, num_jobs, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        if (allocated) free(jobs);
        return rc;
    }
    if (allocated) free(jobs);  /* not needed any more */
    
    /* send it! */
    if (ORTE_SUCCESS != (rc = orte_grpcomm.xcast(0, &cmd, ORTE_RML_TAG_DAEMON))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_DESTRUCT(&cmd);
    
    /* we're done! */
    return rc;
}



int orte_pls_base_orted_signal_local_procs(orte_jobid_t job, int32_t signal, opal_list_t *attrs)
{
    int rc;
    orte_buffer_t cmd;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_SIGNAL_LOCAL_PROCS;
    orte_jobid_t *jobs;
    orte_std_cntr_t num_jobs;
    bool allocated;
    
    OPAL_TRACE(1);
    
    /* get the array of jobs we need to signal */
    if (ORTE_SUCCESS != (rc = get_jobids(&jobs, &num_jobs, &allocated, job, attrs))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    OBJ_CONSTRUCT(&cmd, orte_buffer_t);
    
    /* pack the command */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        if (allocated) free(jobs);
        return rc;
    }
    
    /* pack the number of jobids */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &num_jobs, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        if (allocated) free(jobs);
        return rc;
    }
    
    /* pack the array of jobids */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, jobs, num_jobs, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        if (allocated) free(jobs);
        return rc;
    }
    if (allocated) free(jobs);  /* not needed any more */
    
    /* pack the signal */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &signal, 1, ORTE_INT32))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        return rc;
    }
    
    /* send it! */
    if (ORTE_SUCCESS != (rc = orte_grpcomm.xcast(0, &cmd, ORTE_RML_TAG_DAEMON))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_DESTRUCT(&cmd);
    
    /* we're done! */
    return ORTE_SUCCESS;
}
