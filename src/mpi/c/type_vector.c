/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "runtime/runtime.h"
#include "mpi/c/bindings.h"
#include "datatype/datatype.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_vector = PMPI_Type_vector
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static char FUNC_NAME[] = "MPI_Type_vector";

int
MPI_Type_vector(int count,
                int blocklength,
                int stride,
                MPI_Datatype oldtype,
                MPI_Datatype *newtype)
{
   int rc;

   if( MPI_PARAM_CHECK ) {
      if( OMPI_MPI_INVALID_STATE ) {
         OMPI_ERRHANDLER_RETURN( MPI_ERR_INTERN, MPI_COMM_WORLD,
                                MPI_ERR_INTERN, FUNC_NAME );
      }
      if( count < 0 ) {
         OMPI_ERRHANDLER_RETURN( MPI_ERR_COUNT, MPI_COMM_WORLD,
                                MPI_ERR_COUNT, FUNC_NAME );
      }
      if( blocklength < 0) {
         OMPI_ERRHANDLER_RETURN( MPI_ERR_ARG, MPI_COMM_WORLD,
                                MPI_ERR_ARG, FUNC_NAME );
      }
   }

   rc = ompi_ddt_create_vector ( count, blocklength, stride, oldtype, newtype );
   OMPI_ERRHANDLER_CHECK(rc, MPI_COMM_WORLD, rc, FUNC_NAME );

   {
      int* a_i[3];
      a_i[0] = &count;
      a_i[1] = &blocklength;
      a_i[2] = &stride;

      ompi_ddt_set_args( *newtype, 3, a_i, 0, NULL, 1, &oldtype, MPI_COMBINER_VECTOR );
   }
   return MPI_SUCCESS;
}
