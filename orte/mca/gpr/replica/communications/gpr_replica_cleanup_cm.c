/* -*- C -*-
 *
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
 */
/** @file:
 *
 * The Open MPI General Purpose Registry - Replica component
 *
 */

/*
 * includes
 */
#include "orte_config.h"

#include "opal/util/trace.h"

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/gpr/replica/communications/gpr_replica_comm.h"

int orte_gpr_replica_recv_cleanup_job_cmd(orte_buffer_t *input_buffer,
                                          orte_buffer_t *output_buffer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_CLEANUP_JOB_CMD;
    orte_jobid_t jobid=0;
    orte_std_cntr_t n;
    int rc, ret;
    
    OPAL_TRACE(3);
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(output_buffer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    n = 1;
    if (ORTE_SUCCESS != (ret = orte_dss.unpack(input_buffer, &jobid, &n, ORTE_JOBID))) {
        ORTE_ERROR_LOG(ret);
        goto RETURN_ERROR;
    }

    ret = orte_gpr_replica_cleanup_job_fn(jobid);

    if (ORTE_SUCCESS != ret) {
        ORTE_ERROR_LOG(ret);
    }
    
RETURN_ERROR:
    if (ORTE_SUCCESS != (rc = orte_dss.pack(output_buffer, &ret, 1, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ret;
}


int orte_gpr_replica_recv_cleanup_proc_cmd(orte_buffer_t *input_buffer,
                                           orte_buffer_t *output_buffer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_CLEANUP_PROC_CMD;
    orte_process_name_t proc;
    orte_std_cntr_t n;
    int rc, ret;
    
    OPAL_TRACE(3);
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(output_buffer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    n = 1;
    if (ORTE_SUCCESS != (ret = orte_dss.unpack(input_buffer, &proc, &n, ORTE_NAME))) {
        ORTE_ERROR_LOG(ret);
        goto RETURN_ERROR;
    }

    ret = orte_gpr_replica_cleanup_proc_fn(&proc);

    if (ORTE_SUCCESS != ret) {
        ORTE_ERROR_LOG(ret);
    }
    
RETURN_ERROR:
    if (ORTE_SUCCESS != (rc = orte_dss.pack(output_buffer, &ret, 1, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ret;
}


