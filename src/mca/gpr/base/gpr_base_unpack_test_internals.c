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

int mca_gpr_base_unpack_test_internals(ompi_buffer_t buffer, ompi_list_t *test_results)
{
    char **string1=NULL, **string2=NULL;
    int i;
    int32_t num_responses;
    ompi_registry_internal_test_results_t *newptr=NULL;
    mca_gpr_cmd_flag_t command;

    if ((OMPI_SUCCESS != ompi_unpack(buffer, &command, 1, MCA_GPR_OOB_PACK_CMD))
	|| (MCA_GPR_TEST_INTERNALS_CMD != command)) {
	return OMPI_ERROR;
    }

    if ((OMPI_SUCCESS != ompi_unpack(buffer, &num_responses, 1, OMPI_INT32)) ||
	(0 >= num_responses)) {
	return OMPI_ERROR;
    }

    for (i=0; i<num_responses; i++) {
	if (0 > ompi_unpack_string(buffer, string1)) {
	    return OMPI_ERROR;
	}
	if (0 > ompi_unpack_string(buffer, string2)) {
	    return OMPI_ERROR;
	}
	newptr = OBJ_NEW(ompi_registry_internal_test_results_t);
	newptr->test = strdup(*string1);
	newptr->message = strdup(*string2);
	ompi_list_append(test_results, &newptr->item);
    }

    return OMPI_SUCCESS;
}
