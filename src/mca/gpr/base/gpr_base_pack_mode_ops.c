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

int mca_gpr_base_pack_triggers_active_cmd(ompi_buffer_t cmd,
				          mca_ns_base_jobid_t jobid)
{
    mca_gpr_cmd_flag_t command;

    command = MCA_GPR_TRIGGERS_ACTIVE_CMD;

    if (OMPI_SUCCESS != ompi_pack(cmd, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &jobid, 1, MCA_GPR_OOB_PACK_JOBID)) {
	return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

int mca_gpr_base_pack_triggers_inactive_cmd(ompi_buffer_t cmd,
					    mca_ns_base_jobid_t jobid)
{
    mca_gpr_cmd_flag_t command;

    command = MCA_GPR_TRIGGERS_INACTIVE_CMD;
    if (OMPI_SUCCESS != ompi_pack(cmd, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &jobid, 1, MCA_GPR_OOB_PACK_JOBID)) {
	return OMPI_ERROR;
    }

    return OMPI_SUCCESS;

}

int mca_gpr_base_pack_notify_on(ompi_buffer_t cmd,
				ompi_process_name_t *proc,
				ompi_registry_notify_id_t sub_number)
{
    mca_gpr_cmd_flag_t command;

    command = MCA_GPR_NOTIFY_ON_CMD;

    if (OMPI_SUCCESS != ompi_pack(cmd, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, proc, 1, MCA_GPR_OOB_PACK_NAME)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &sub_number, 1, MCA_GPR_OOB_PACK_NOTIFY_ID)) {
	return OMPI_ERROR;
    }

    return OMPI_SUCCESS;

}

int mca_gpr_base_pack_notify_off(ompi_buffer_t cmd,
				 ompi_process_name_t *proc,
				 ompi_registry_notify_id_t sub_number)
{
    mca_gpr_cmd_flag_t command;

    command = MCA_GPR_NOTIFY_OFF_CMD;

    if (OMPI_SUCCESS != ompi_pack(cmd, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, proc, 1, MCA_GPR_OOB_PACK_NAME)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &sub_number, 1, MCA_GPR_OOB_PACK_NOTIFY_ID)) {
	return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

int mca_gpr_base_pack_assume_ownership(ompi_buffer_t cmd, bool silent,
				       mca_ns_base_jobid_t jobid, char *segment)
{
    mca_gpr_cmd_flag_t command;
    int8_t tmp_bool;

    command = MCA_GPR_ASSUME_OWNERSHIP_CMD;

    if (OMPI_SUCCESS != ompi_pack(cmd, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	return OMPI_ERROR;
    }

    tmp_bool = (int8_t)silent;
    if (OMPI_SUCCESS != ompi_pack(cmd, &tmp_bool, 1, MCA_GPR_OOB_PACK_BOOL)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &jobid, 1, MCA_GPR_OOB_PACK_JOBID)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack_string(cmd, segment)) {
	return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
