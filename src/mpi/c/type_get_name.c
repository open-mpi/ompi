/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include <string.h>
#include "datatype/datatype.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_get_name = PMPI_Type_get_name
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int
MPI_Type_get_name(MPI_Datatype type, char *type_name, int *resultlen)
{
   *resultlen = strlen(type->name);
   strncpy( type_name, type->name, *resultlen );
   return MPI_SUCCESS;
}
