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
 * The Open MPI General Purpose Registry - pack functions
 *
 */

/*
 * includes
 */
#include "ompi_config.h"

#include "mca/gpr/base/base.h"

int mca_gpr_base_pack_get_startup_msg(ompi_buffer_t cmd,
				      mca_ns_base_jobid_t jobid)
{
    mca_gpr_cmd_flag_t command;

    command = MCA_GPR_GET_STARTUP_MSG_CMD;

    if (OMPI_SUCCESS != ompi_pack(cmd, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
		return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &jobid, 1, OMPI_JOBID)) {
		return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


int mca_gpr_base_pack_get_shutdown_msg(ompi_buffer_t cmd,
				       mca_ns_base_jobid_t jobid)
{
    mca_gpr_cmd_flag_t command;

    command = MCA_GPR_GET_SHUTDOWN_MSG_CMD;

    if (OMPI_SUCCESS != ompi_pack(cmd, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
		return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &jobid, 1, OMPI_JOBID)) {
		return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
