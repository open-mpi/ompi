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
 * The Open MPI general purpose registry - base unpack functions.
 *
 */

/*
 * includes
 */

#include "ompi_config.h"

#include "mca/gpr/base/base.h"

int mca_gpr_base_unpack_put(ompi_buffer_t buffer)
{
    mca_gpr_cmd_flag_t command;
    int32_t response;

    if ((OMPI_SUCCESS != ompi_unpack(buffer, &command, 1, MCA_GPR_OOB_PACK_CMD))
	|| (MCA_GPR_PUT_CMD != command)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &response, 1, OMPI_INT32)) {
	return OMPI_ERROR;
    } else {
	return (int)response;
    }

}


int mca_gpr_base_unpack_get(ompi_buffer_t buffer, ompi_list_t *returned_list)
{
    mca_gpr_cmd_flag_t command;
    int32_t object_size, num_responses;
    ompi_registry_value_t *newptr;
    ompi_registry_object_t *object;
    int i;

    if ((OMPI_SUCCESS != ompi_unpack(buffer, &command, 1, MCA_GPR_OOB_PACK_CMD))
	|| (MCA_GPR_GET_CMD != command)) {
	return OMPI_ERROR;
    }

    if ((OMPI_SUCCESS != ompi_unpack(buffer, &num_responses, 1, OMPI_INT32)) ||
	(0 >= num_responses)) {
	return OMPI_ERROR;
    }

    for (i=0; i<num_responses; i++) {
	if (OMPI_SUCCESS != ompi_unpack(buffer, &object_size, 1, MCA_GPR_OOB_PACK_OBJECT_SIZE)) {
	    return OMPI_ERROR;
	}
	object = (ompi_registry_object_t)malloc(object_size);
	if (OMPI_SUCCESS != ompi_unpack(buffer, object, object_size, OMPI_BYTE)) {
	    free(object);
	    return OMPI_ERROR;
	}
	newptr = OBJ_NEW(ompi_registry_value_t);
	newptr->object_size = object_size;
	newptr->object = object;
	ompi_list_append(returned_list, &newptr->item);
    }

    return OMPI_SUCCESS;
}
