/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "adio.h"
#include "mpi.h"

#if defined(HAVE_MPI_STATUS_SET_ELEMENTS_X)
/* Not quite correct, but much closer for MPI2 */
/* TODO: still needs to handle partial datatypes and situations where the mpi
 * implementation fills status with something other than bytes (globus2 might
 * do this) */
int MPIR_Status_set_bytes(MPI_Status * status, MPI_Datatype datatype, MPI_Count nbytes)
{
    MPL_UNREFERENCED_ARG(datatype);
    /* it's ok that ROMIO stores number-of-bytes in status, not
     * count-of-copies, as long as MPI_GET_COUNT knows what to do */
    if (status != MPI_STATUS_IGNORE)
        MPI_Status_set_elements_x(status, MPI_BYTE, nbytes);
    return MPI_SUCCESS;
}
#endif
