/*
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


int mca_gpr_base_unpack_synchro(ompi_buffer_t buffer, ompi_registry_notify_id_t *remote_idtag)
{
    mca_gpr_cmd_flag_t command;
    int32_t response;

    if ((OMPI_SUCCESS != ompi_unpack(buffer, &command, 1, MCA_GPR_OOB_PACK_CMD))
	|| (MCA_GPR_SUBSCRIBE_CMD != command)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, remote_idtag, 1, MCA_GPR_OOB_PACK_NOTIFY_ID)) {
	return OMPI_ERROR;
    }

    return (int)response;
}


int mca_gpr_base_unpack_cancel_synchro(ompi_buffer_t buffer)
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
