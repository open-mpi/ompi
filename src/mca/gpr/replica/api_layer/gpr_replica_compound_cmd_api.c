/*
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
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "dps/dps.h"
#include "util/output.h"
#include "util/proc_info.h"

#include "mca/ns/ns_types.h"

#include "mca/gpr/replica/gpr_replica.h"
#include "mca/gpr/replica/communications/gpr_replica_comm.h"

#include "gpr_replica_api.h"


int orte_gpr_replica_begin_compound_cmd(void)
{
    orte_gpr_cmd_flag_t command;
    int rc;
    
    command = ORTE_GPR_COMPOUND_CMD;

    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.wait_for_compound_mutex);

    while (orte_gpr_replica_globals.compound_cmd_mode) {
	    orte_gpr_replica_globals.compound_cmd_waiting++;
	    ompi_condition_wait(&orte_gpr_replica_globals.compound_cmd_condition,
                                &orte_gpr_replica_globals.wait_for_compound_mutex);
	    orte_gpr_replica_globals.compound_cmd_waiting--;
    }

    orte_gpr_replica_globals.compound_cmd_mode = true;
    if (NULL != orte_gpr_replica_globals.compound_cmd) {
        OBJ_RELEASE(orte_gpr_replica_globals.compound_cmd);
    }
    
    orte_gpr_replica_globals.compound_cmd = OBJ_NEW(orte_buffer_t);
    if (NULL == orte_gpr_replica_globals.compound_cmd) {
        orte_gpr_replica_globals.compound_cmd_mode = false;
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dps.pack(orte_gpr_replica_globals.compound_cmd, &command,
                                            1, ORTE_GPR_CMD))) {
        orte_gpr_replica_globals.compound_cmd_mode = false;
        OBJ_RELEASE(orte_gpr_replica_globals.compound_cmd);
        return rc;
    }
    
    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.wait_for_compound_mutex);
    return ORTE_SUCCESS;
}


int orte_gpr_replica_stop_compound_cmd(void)
{
    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.wait_for_compound_mutex);

    orte_gpr_replica_globals.compound_cmd_mode = false;
    if (NULL != orte_gpr_replica_globals.compound_cmd) {
        OBJ_RELEASE(orte_gpr_replica_globals.compound_cmd);
    }

    if (orte_gpr_replica_globals.compound_cmd_waiting) {
	   ompi_condition_signal(&orte_gpr_replica_globals.compound_cmd_condition);
    }

    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.wait_for_compound_mutex);
    return ORTE_SUCCESS;
}


int orte_gpr_replica_exec_compound_cmd(void)
{
    int rc;
    orte_buffer_t *answer;
    
    if (orte_gpr_replica_globals.debug) {
	   ompi_output(0, "[%d,%d,%d] Executing compound command",
		    ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.wait_for_compound_mutex);

    rc = orte_gpr_replica_process_command_buffer(orte_gpr_replica_globals.compound_cmd,
                                        NULL, &answer);

    orte_gpr_replica_globals.compound_cmd_mode = false;
    if (NULL != orte_gpr_replica_globals.compound_cmd) {  /* shouldn't be any way this could be true, but just to be safe... */
        OBJ_RELEASE(orte_gpr_replica_globals.compound_cmd);
    }

    if (orte_gpr_replica_globals.compound_cmd_waiting) {
	   ompi_condition_signal(&orte_gpr_replica_globals.compound_cmd_condition);
    }

    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.wait_for_compound_mutex);

    if (ORTE_SUCCESS != rc) {
        return rc;
    }
    
    return orte_gpr_replica_process_callbacks();

}
