/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_struct = PMPI_Type_struct
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Type_struct";


OMPI_EXPORT
int MPI_Type_struct(int count,
                    int array_of_blocklengths[],
                    MPI_Aint array_of_displacements[],
                    MPI_Datatype array_of_types[],
                    MPI_Datatype *newtype)
{
  int i;

  if ( MPI_PARAM_CHECK ) {
    OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    if (NULL == newtype || NULL == array_of_types) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_TYPE,
                                      FUNC_NAME );
    } else if (count < 0) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COUNT,
                                    FUNC_NAME );
    } else if (NULL == array_of_blocklengths ||
               NULL == array_of_displacements) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                    FUNC_NAME );
    }
    for (i = 0; i < count; ++i) {
      if (NULL == array_of_types[i] || 
          MPI_DATATYPE_NULL == array_of_types[i]) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_TYPE,
                                      FUNC_NAME );
      } else if (array_of_blocklengths[i] < 0) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                      FUNC_NAME );
      }
    }
  }

  return MPI_Type_create_struct(count,
                                array_of_blocklengths,
                                array_of_displacements,
                                array_of_types,
                                newtype);
}
