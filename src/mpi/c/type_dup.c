/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "datatype/datatype.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_dup = PMPI_Type_dup
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Type_dup";

int
MPI_Type_dup (MPI_Datatype type,
              MPI_Datatype *newtype)
{
   int rc;

   if( MPI_PARAM_CHECK ) {
      if( OMPI_MPI_INVALID_STATE ) {
         OMPI_ERRHANDLER_RETURN( MPI_ERR_INTERN, MPI_COMM_WORLD,
                                MPI_ERR_INTERN, FUNC_NAME );
      }
   }

   
   if( (rc = ompi_ddt_duplicate( type, newtype )) != MPI_SUCCESS ) {
      ompi_ddt_destroy( newtype );
      OMPI_ERRHANDLER_RETURN( MPI_ERR_INTERN, MPI_COMM_WORLD,
                             MPI_ERR_INTERN, FUNC_NAME );
   }

   ompi_ddt_set_args( *newtype, 0, NULL, 0, NULL, 1, &type, MPI_COMBINER_DUP );

   return MPI_SUCCESS;
}
