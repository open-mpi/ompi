/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Get_count = PMPI_Get_count
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Get_count";


int MPI_Get_count(MPI_Status *status, MPI_Datatype datatype, int *count) 
{
   int size, rc;

   if (MPI_PARAM_CHECK) {
      OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
      OMPI_CHECK_DATATYPE_FOR_RECV(rc, datatype, 1);

      OMPI_ERRHANDLER_CHECK(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
   }

   if( ompi_ddt_type_size( datatype, &size ) == MPI_SUCCESS ) {
      if( size == 0 ) {
         *count = 0;
         return MPI_SUCCESS;
      }
      
      *count = status->_count / size;
      if( ((*count) * size) == status->_count )
         return MPI_SUCCESS;
      *count = MPI_UNDEFINED;
   }
   return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
}
