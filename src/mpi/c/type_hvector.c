/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_hvector = PMPI_Type_hvector
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int
MPI_Type_hvector(int count,
                 int blocklength,
                 MPI_Aint stride,
		 MPI_Datatype oldtype,
                 MPI_Datatype *newtype)
{
    return MPI_Type_create_hvector(count,
                                   blocklength,
                                   stride,
                                   oldtype,
                                   newtype);
}
