/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"
#include "lam/lfc/lam_list.h"
#include "mpi/info/info.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_c2f = PMPI_Info_c2f
#endif

/**
 * Converts info into a fortan handle
 *
 * @param info The C handle which has to be converted 
 * @retval Fortran handle corresponding to info.
 *
 * If info is a valid C handle, the MPI_Info_c2f returns a valid 
 * fortran handle to the same MPI_INFO object.
 */
MPI_Fint MPI_Info_c2f(MPI_Info info) {
    return (MPI_Fint)0;
}
