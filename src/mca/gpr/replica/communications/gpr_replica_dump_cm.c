/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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

#include "dps/dps.h"
#include "mca/errmgr/errmgr.h"

#include "gpr_replica_comm.h"


int orte_gpr_replica_recv_dump_all_cmd(orte_buffer_t *answer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_DUMP_ALL_CMD;
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_dps.pack(answer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.mutex);
    rc = orte_gpr_replica_dump_all_fn(answer);
    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}

int orte_gpr_replica_recv_dump_segments_cmd(orte_buffer_t *answer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_DUMP_SEGMENTS_CMD;
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_dps.pack(answer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.mutex);
    rc = orte_gpr_replica_dump_segments_fn(answer);
    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}

int orte_gpr_replica_recv_dump_triggers_cmd(orte_buffer_t *answer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_DUMP_TRIGGERS_CMD;
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_dps.pack(answer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.mutex);
    rc = orte_gpr_replica_dump_triggers_fn(answer);
    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}
