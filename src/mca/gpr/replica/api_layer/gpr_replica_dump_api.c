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
#include "mca/errmgr/errmgr.h"

#include "mca/gpr/replica/api_layer/gpr_replica_api.h"

int orte_gpr_replica_dump_all(int output_id)
{
    orte_buffer_t *buffer;
    int rc;

    if (orte_gpr_replica_globals.debug) {
	   ompi_output(0, "[%d,%d,%d] gpr_replica_dump_all: entered for output on %d",
		    ORTE_NAME_ARGS(orte_process_info.my_name), output_id);
    }

    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    if (orte_gpr_replica_globals.compound_cmd_mode) {
	    if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_dump_all(orte_gpr_replica_globals.compound_cmd))) {
            ORTE_ERROR_LOG(rc);
        }
	    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
	    return rc;
    }

    buffer = OBJ_NEW(orte_buffer_t);
    if (NULL == buffer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_dump_all_fn(buffer))) {
        ORTE_ERROR_LOG(rc);
    }

    if (ORTE_SUCCESS == rc) {
        orte_gpr_base_print_dump(buffer, output_id);
    }
    OBJ_RELEASE(buffer);

    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    return rc;
}

int orte_gpr_replica_dump_segments(int output_id)
{
    orte_buffer_t *buffer;
    int rc;

    if (orte_gpr_replica_globals.debug) {
      ompi_output(0, "[%d,%d,%d] gpr_replica_dump_segments: entered for output on %d",
         ORTE_NAME_ARGS(orte_process_info.my_name), output_id);
    }

    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    if (orte_gpr_replica_globals.compound_cmd_mode) {
        if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_dump_segments(orte_gpr_replica_globals.compound_cmd))) {
            ORTE_ERROR_LOG(rc);
        }
     OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
       return rc;
    }

    buffer = OBJ_NEW(orte_buffer_t);
    if (NULL == buffer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_dump_segments_fn(buffer))) {
        ORTE_ERROR_LOG(rc);
    }

    if (ORTE_SUCCESS == rc) {
        orte_gpr_base_print_dump(buffer, output_id);
    }
    OBJ_RELEASE(buffer);

    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    return rc;
}

int orte_gpr_replica_dump_triggers(int output_id)
{
    orte_buffer_t *buffer;
    int rc;

    if (orte_gpr_replica_globals.debug) {
      ompi_output(0, "[%d,%d,%d] gpr_replica_dump_triggers: entered for output on %d",
         ORTE_NAME_ARGS(orte_process_info.my_name), output_id);
    }

    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    if (orte_gpr_replica_globals.compound_cmd_mode) {
        if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_dump_triggers(orte_gpr_replica_globals.compound_cmd))) {
            ORTE_ERROR_LOG(rc);
        }
     OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
       return rc;
    }

    buffer = OBJ_NEW(orte_buffer_t);
    if (NULL == buffer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_dump_triggers_fn(buffer))) {
        ORTE_ERROR_LOG(rc);
    }

    if (ORTE_SUCCESS == rc) {
        orte_gpr_base_print_dump(buffer, output_id);
    }
    OBJ_RELEASE(buffer);

    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    return rc;
}
