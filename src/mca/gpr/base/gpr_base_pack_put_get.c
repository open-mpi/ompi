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

int mca_gpr_base_pack_put(ompi_buffer_t cmd, bool silent,
			  ompi_registry_mode_t mode, char *segment,
			  char **tokens, ompi_registry_object_t object,
			  ompi_registry_object_size_t size)
{
    mca_gpr_cmd_flag_t command;
    mca_ns_base_jobid_t jobid;
    char **tokptr;
    int i;
    int32_t num_tokens, object_size;
    int8_t tmp_bool;


    command = MCA_GPR_PUT_CMD;

    if (OMPI_SUCCESS != ompi_pack(cmd, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	return OMPI_ERROR;
    }

    tmp_bool = (int8_t)silent;
    if (OMPI_SUCCESS != ompi_pack(cmd, &tmp_bool, 1, MCA_GPR_OOB_PACK_BOOL)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &mode, 1, MCA_GPR_OOB_PACK_MODE)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack_string(cmd, segment)) {
	return OMPI_ERROR;
    }

    jobid = ompi_name_server.get_jobid(ompi_rte_get_self());
    if (OMPI_SUCCESS != ompi_pack(cmd, &jobid, 1, MCA_GPR_OOB_PACK_JOBID)) {
	return OMPI_ERROR;
    }

    /* compute number of tokens */
    tokptr = tokens;
    num_tokens = 0;
    while (NULL != *tokptr) {
	num_tokens++;
	tokptr++;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &num_tokens, 1, OMPI_INT32)) {
	return OMPI_ERROR;
    }

    tokptr = tokens;
    for (i=0; i<num_tokens; i++) {  /* pack the tokens */
	if (OMPI_SUCCESS != ompi_pack_string(cmd, *tokptr)) {
	    return OMPI_ERROR;
	}
	tokptr++;
    }

    object_size = (int32_t)size;

    if (OMPI_SUCCESS != ompi_pack(cmd, &object_size, 1, MCA_GPR_OOB_PACK_OBJECT_SIZE)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, object, object_size, OMPI_BYTE)) {
	return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


int mca_gpr_base_pack_get(ompi_buffer_t cmd,
			  ompi_registry_mode_t mode,
			  char *segment, char **tokens)
{
    mca_gpr_cmd_flag_t command;
    char **tokptr;
    int i;
    int32_t num_tokens;

    command = MCA_GPR_GET_CMD;

    if (OMPI_SUCCESS != ompi_pack(cmd, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &mode, 1, MCA_GPR_OOB_PACK_MODE)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack_string(cmd, segment)) {
	return OMPI_ERROR;
    }

    /* compute number of tokens */
    tokptr = tokens;
    num_tokens = 0;
    while (NULL != *tokptr) {
	num_tokens++;
	tokptr++;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &num_tokens, 1, OMPI_INT32)) {
	return OMPI_ERROR;
    }

    tokptr = tokens;
    for (i=0; i<num_tokens; i++) {  /* pack the tokens */
	if (OMPI_SUCCESS != ompi_pack_string(cmd, *tokptr)) {
	    return OMPI_ERROR;
	}
	tokptr++;
    }

    return OMPI_SUCCESS;
}
