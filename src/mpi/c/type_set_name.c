/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include <string.h>
#include "datatype/datatype.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_set_name = PMPI_Type_set_name
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int
MPI_Type_set_name (MPI_Datatype type, char *type_name)
{
   int length;

   /* TODO in C the maximum length is MPI_MAX_OBJECT_NAME - 1
    * when in FORTRAN is MPI_MAX_OBJECT_NAME
    */
   length = strlen( type_name );
   if( length >= MPI_MAX_OBJECT_NAME )
      length = MPI_MAX_OBJECT_NAME - 1;
   strncpy( type->name, type_name, length );
   type->name[length + 1] = '\0';
   return MPI_SUCCESS;
}
