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

#include "ompi_config.h"

#include "mca/gpr/base/base.h"

int mca_gpr_base_pack_cleanup_job(ompi_buffer_t buffer, mca_ns_base_jobid_t jobid)
{
    mca_gpr_cmd_flag_t command;

    command = MCA_GPR_CLEANUP_JOB_CMD;

    if (OMPI_SUCCESS != ompi_pack(buffer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(buffer, &jobid, 1, MCA_GPR_OOB_PACK_JOBID)) {
	return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


int mca_gpr_base_pack_cleanup_proc(ompi_buffer_t buffer, bool purge, ompi_process_name_t *proc)
{
    mca_gpr_cmd_flag_t command;

    command = MCA_GPR_CLEANUP_PROC_CMD;

    if (OMPI_SUCCESS != ompi_pack(buffer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(buffer, &purge, 1, MCA_GPR_OOB_PACK_BOOL)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(buffer, proc, 1, MCA_GPR_OOB_PACK_NAME)) {
	return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
