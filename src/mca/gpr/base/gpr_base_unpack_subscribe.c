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
 * The Open MPI general purpose registry - unpack functions.
 *
 */

/*
 * includes
 */

#include "ompi_config.h"

#include "mca/gpr/base/base.h"


int mca_gpr_base_unpack_subscribe(ompi_buffer_t buffer, ompi_registry_notify_id_t *remote_idtag)
{
    mca_gpr_cmd_flag_t command;

    if ((OMPI_SUCCESS != ompi_unpack(buffer, &command, 1, MCA_GPR_OOB_PACK_CMD))
	|| (MCA_GPR_SUBSCRIBE_CMD != command)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, remote_idtag, 1, MCA_GPR_OOB_PACK_NOTIFY_ID)) {
	return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


int mca_gpr_base_unpack_unsubscribe(ompi_buffer_t buffer)
{
    mca_gpr_cmd_flag_t command;
    int32_t response;

    if ((OMPI_SUCCESS != ompi_unpack(buffer, &command, 1, MCA_GPR_OOB_PACK_CMD))
	|| (MCA_GPR_UNSUBSCRIBE_CMD != command)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &response, 1, OMPI_INT32)) {
	return OMPI_ERROR;
    }

    return (int)response;
}
