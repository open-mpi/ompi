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
 */

#include "ompi_config.h"

#include "mca/gpr/base/base.h"

int mca_gpr_base_pack_delete_segment(ompi_buffer_t cmd, bool silent, char *segment)
{
    mca_gpr_cmd_flag_t command;
    int8_t tmp_bool;

    command = MCA_GPR_DELETE_SEGMENT_CMD;

    if (OMPI_SUCCESS != ompi_pack(cmd, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	return OMPI_ERROR;
    }

    tmp_bool = (int8_t)silent;
    if (OMPI_SUCCESS != ompi_pack(cmd, &tmp_bool, 1, MCA_GPR_OOB_PACK_BOOL)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack_string(cmd, segment)) {
	return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


int mca_gpr_base_pack_delete_object(ompi_buffer_t cmd, bool silent,
				    ompi_registry_mode_t mode,
			    char *segment, char **tokens)
{
    mca_gpr_cmd_flag_t command;
    char **tokptr;
    int32_t num_tokens;
    int i;
    int8_t tmp_bool;

    command = MCA_GPR_DELETE_OBJECT_CMD;

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


int mca_gpr_base_pack_index(ompi_buffer_t cmd, char *segment)
{
    mca_gpr_cmd_flag_t command;
    ompi_registry_mode_t mode;

    command = MCA_GPR_INDEX_CMD;

    if (OMPI_SUCCESS != ompi_pack(cmd, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	return OMPI_ERROR;
    }

    if (NULL == segment) {  /* no segment specified - want universe dict */
	mode = 0;
	if (OMPI_SUCCESS != ompi_pack(cmd, &mode, 1, MCA_GPR_OOB_PACK_MODE)) {
	    return OMPI_ERROR;
	}
    } else {
	mode = 1;
	if (OMPI_SUCCESS != ompi_pack(cmd, &mode, 1, MCA_GPR_OOB_PACK_MODE)) {
	    return OMPI_ERROR;
	}
	if (OMPI_SUCCESS != ompi_pack_string(cmd, segment)) {
	    return OMPI_ERROR;
	}
    }

    return OMPI_SUCCESS;
}
