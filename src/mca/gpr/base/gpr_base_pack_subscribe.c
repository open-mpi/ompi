/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

int mca_gpr_base_pack_subscribe(ompi_buffer_t cmd,
				ompi_registry_mode_t mode,
				ompi_registry_notify_action_t action,
				char *segment, char **tokens)
{
    mca_gpr_cmd_flag_t command;
    char **tokptr;
    int i;
    int32_t num_tokens;

    command = MCA_GPR_SUBSCRIBE_CMD;

    if (OMPI_SUCCESS != ompi_pack(cmd, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &mode, 1, MCA_GPR_OOB_PACK_MODE)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &action, 1, MCA_GPR_OOB_PACK_ACTION)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack_string(cmd, segment)) {
	return OMPI_ERROR;
    }

    num_tokens = 0;
    if (NULL != tokens) {
	/* compute number of tokens */
	tokptr = tokens;
	while (NULL != *tokptr) {
	    num_tokens++;
	    tokptr++;
	}
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &num_tokens, 1, OMPI_INT32)) {
	return OMPI_ERROR;
    }

    if (0 < num_tokens) {
	tokptr = tokens;
	for (i=0; i<num_tokens; i++) {  /* pack the tokens */
	    if (OMPI_SUCCESS != ompi_pack_string(cmd, *tokptr)) {
		return OMPI_ERROR;
	    }
	    tokptr++;
	}
    }

    return OMPI_SUCCESS;
}


int mca_gpr_base_pack_unsubscribe(ompi_buffer_t cmd, bool silent,
				  ompi_registry_notify_id_t remote_idtag)
{
    mca_gpr_cmd_flag_t command;
    int8_t tmp_bool;

    command = MCA_GPR_UNSUBSCRIBE_CMD;

    if (OMPI_SUCCESS != ompi_pack(cmd, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	return OMPI_ERROR;
    }

    tmp_bool = (int8_t)silent;
    if (OMPI_SUCCESS != ompi_pack(cmd, &tmp_bool, 1, MCA_GPR_OOB_PACK_BOOL)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &remote_idtag, 1, MCA_GPR_OOB_PACK_NOTIFY_ID)) {
	return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
