/*
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

int mca_gpr_base_unpack_triggers_active_cmd(ompi_buffer_t cmd)
{
    return OMPI_SUCCESS;
}

int mca_gpr_base_unpack_triggers_inactive(ompi_buffer_t cmd)
{
    return OMPI_SUCCESS;

}

int mca_gpr_base_unpack_notify_on(ompi_buffer_t cmd)
{
    return OMPI_SUCCESS;

}

int mca_gpr_base_unpack_notify_off(ompi_buffer_t cmd)
{
    return OMPI_SUCCESS;
}

int mca_gpr_base_unpack_assume_ownership(ompi_buffer_t cmd)
{
    mca_gpr_cmd_flag_t command;
    int32_t response;

    if ((OMPI_SUCCESS != ompi_unpack(cmd, &command, 1, MCA_GPR_OOB_PACK_CMD)) ||
	(MCA_GPR_ASSUME_OWNERSHIP_CMD != command)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_unpack(cmd, &response, 1, OMPI_INT32)) {
	return OMPI_ERROR;
    }

    return (int)response;
}
