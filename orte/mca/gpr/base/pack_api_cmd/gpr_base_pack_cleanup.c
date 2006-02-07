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
 */
/** @file:
 *
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "opal/util/trace.h"

#include "orte/dss/dss.h"

#include "orte/mca/gpr/base/base.h"

int orte_gpr_base_pack_cleanup_job(orte_buffer_t *buffer, orte_jobid_t jobid)
{
    orte_gpr_cmd_flag_t command;
    int rc;

    OPAL_TRACE(3);

    command = ORTE_GPR_CLEANUP_JOB_CMD;

    if (ORTE_SUCCESS != (rc = orte_dss.pack(buffer, &command, 1, ORTE_GPR_CMD))) {
       return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.pack(buffer, &jobid, 1, ORTE_JOBID))) {
       return rc;
    }

    return ORTE_SUCCESS;
}


int orte_gpr_base_pack_cleanup_proc(orte_buffer_t *buffer, orte_process_name_t *proc)
{
    orte_gpr_cmd_flag_t command;
    int rc;

    OPAL_TRACE(3);

    command = ORTE_GPR_CLEANUP_PROC_CMD;

    if (ORTE_SUCCESS != (rc = orte_dss.pack(buffer, &command, 1, ORTE_GPR_CMD))) {
       return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.pack(buffer, proc, 1, ORTE_NAME))) {
       return rc;
    }

    return ORTE_SUCCESS;
}
