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
 */

#include "ompi_config.h"

#include "mca/gpr/base/base.h"


int mca_gpr_base_unpack_delete_segment(ompi_buffer_t buffer)
{
    mca_gpr_cmd_flag_t command;
    int32_t response;

    if ((OMPI_SUCCESS != ompi_unpack(buffer, &command, 1, MCA_GPR_OOB_PACK_CMD))
	|| (MCA_GPR_DELETE_SEGMENT_CMD != command)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &response, 1, OMPI_INT32)) {
	return OMPI_ERROR;
    } else {
	return (int)response;
    }
}


int mca_gpr_base_unpack_delete_object(ompi_buffer_t buffer)
{
    mca_gpr_cmd_flag_t command;
    int32_t response;

    if ((OMPI_SUCCESS != ompi_unpack(buffer, &command, 1, MCA_GPR_OOB_PACK_CMD))
	|| (MCA_GPR_DELETE_OBJECT_CMD != command)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &response, 1, OMPI_INT32)) {
	    return OMPI_ERROR;
    } else {
	    return (int)response;
    }
}


int mca_gpr_base_unpack_index(ompi_buffer_t buffer, ompi_list_t *return_list)
{
    mca_gpr_cmd_flag_t command;
    int32_t num_responses;
    ompi_registry_index_value_t *newptr;
    char *string1;
    int i;

    if ((OMPI_SUCCESS != ompi_unpack(buffer, &command, 1, MCA_GPR_OOB_PACK_CMD))
	|| (MCA_GPR_INDEX_CMD != command)) {
	return OMPI_ERROR;
    }

    if ((OMPI_SUCCESS != ompi_unpack(buffer, &num_responses, 1, OMPI_INT32)) ||
	(0 >= num_responses)) {
	return OMPI_ERROR;
    }

    for (i=0; i<num_responses; i++) {
	if (0 > ompi_unpack_string(buffer, &string1)) {
	    return OMPI_ERROR;
	}
	newptr = OBJ_NEW(ompi_registry_index_value_t);
	newptr->token = strdup(string1);
	ompi_list_append(return_list, &newptr->item);
    }

    return OMPI_SUCCESS;
}
