/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "datatype/datatype.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_dup = PMPI_Type_dup
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static char FUNC_NAME[] = "MPI_Type_dup";

int
MPI_Type_dup (MPI_Datatype type,
              MPI_Datatype *newtype)
{
   int rc;

   if( MPI_PARAM_CHECK ) {
      if( LAM_MPI_INVALID_STATE ) {
         LAM_ERRHANDLER_RETURN( MPI_ERR_INTERN, (lam_communicator_t*)NULL,
                                MPI_ERR_INTERN, FUNC_NAME );
      }
   }

   
   if( (rc = lam_ddt_duplicate( type, newtype )) != MPI_SUCCESS ) {
      lam_ddt_destroy( newtype );
      LAM_ERRHANDLER_RETURN( MPI_ERR_INTERN, (lam_communicator_t*)NULL,
                             MPI_ERR_INTERN, FUNC_NAME );
   }

   lam_ddt_set_args( *newtype, 0, NULL, 0, NULL, 1, &type, MPI_COMBINER_DUP );

   return MPI_SUCCESS;
}
