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
#pragma weak MPI_Type_get_contents = PMPI_Type_get_contents
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Type_get_contents";

int
MPI_Type_get_contents(MPI_Datatype mtype,
                      int max_integers,
                      int max_addresses,
                      int max_datatypes,
                      int array_of_integers[],
                      MPI_Aint array_of_addresses[],
                      MPI_Datatype array_of_datatypes[])
{
   int rc;

   if( MPI_PARAM_CHECK ) {
      if( OMPI_MPI_INVALID_STATE ) {
         OMPI_ERRHANDLER_RETURN( MPI_ERR_INTERN, MPI_COMM_WORLD,
                                MPI_ERR_INTERN, FUNC_NAME );
      }
   }

   rc = ompi_ddt_get_args( mtype, 1, &max_integers, array_of_integers,
                          &max_addresses, array_of_addresses,
                          &max_datatypes, array_of_datatypes, NULL );
   if( rc != MPI_SUCCESS ) {
      OMPI_ERRHANDLER_RETURN( MPI_ERR_INTERN, MPI_COMM_WORLD,
                             MPI_ERR_INTERN, FUNC_NAME );
   }

   /* TODO: we have to return a copy of the datatypes not the original datatypes */
   return MPI_SUCCESS;
}
