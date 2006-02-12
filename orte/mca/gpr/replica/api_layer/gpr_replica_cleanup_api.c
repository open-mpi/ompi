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

#include "orte/mca/ns/ns.h"
#include "orte/mca/errmgr/errmgr.h"

#include "gpr_replica_api.h"
#include "orte/mca/gpr/replica/functional_layer/gpr_replica_fn.h"


int orte_gpr_replica_cleanup_job(orte_jobid_t jobid)
{
    int rc;

    OPAL_TRACE(1);

    OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    rc = orte_gpr_replica_cleanup_job_fn(jobid);

    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_check_events())) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return rc;
    }

    rc = orte_gpr_replica_process_callbacks();

    OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    return rc;
}


int orte_gpr_replica_cleanup_proc(orte_process_name_t *proc)
{
    int rc;

    OPAL_TRACE(1);

    OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);
    rc = orte_gpr_replica_cleanup_proc_fn(proc);

    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_check_events())) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return rc;
    }

    rc = orte_gpr_replica_process_callbacks();
    OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    return rc;
}
