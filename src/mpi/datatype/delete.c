/*
 * $HEADER$
 */

/** @file datatype deletion function */

#include "lam_config.h"
#include "lam/constants.h"
#include "mpi.h"
#include "datatype.h"

/**
 * Delete a LAM/MPI datatype (actually, just mark it for deletion)
 *
 * @param type       datatype
 * @return           LAM_SUCCESS on success, LAM_ERROR otherwise
 *
 * This is the central location for creation of data types in LAM/MPI.
 * All MPI_Type_create functions rely upon this to do the actual type
 * creation.
 */
int lam_datatype_delete(lam_datatype_t *type)
{
    return LAM_SUCCESS;
}
