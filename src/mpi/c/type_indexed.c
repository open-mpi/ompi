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
#pragma weak MPI_Type_indexed = PMPI_Type_indexed
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static char FUNC_NAME[] = "MPI_Type_indexed";

int
MPI_Type_indexed(int count,
                 int array_of_blocklengths[],
                 int array_of_displacements[],
                 MPI_Datatype oldtype,
                 MPI_Datatype *newtype)
{
   int rc, i;

   if( MPI_PARAM_CHECK ) {
      if( OMPI_MPI_INVALID_STATE ) {
         OMPI_ERRHANDLER_RETURN( MPI_ERR_INTERN, (ompi_communicator_t*)NULL,
                                MPI_ERR_INTERN, FUNC_NAME );
      }
      if( count < 0 ) {
         OMPI_ERRHANDLER_RETURN( MPI_ERR_COUNT, (ompi_communicator_t*)NULL,
                                MPI_ERR_COUNT, FUNC_NAME );
      }
      for( i = 0; i < count; i++ ) {
         if( array_of_blocklengths[i] < 0 ) {
            OMPI_ERRHANDLER_RETURN( MPI_ERR_ARG, (ompi_communicator_t*)NULL,
                                   MPI_ERR_ARG, FUNC_NAME );
         }
      }
   }

   rc = ompi_ddt_create_indexed ( count, array_of_blocklengths, 
                                 array_of_displacements,
                                 oldtype, newtype );
   if( rc != MPI_SUCCESS ) {
      ompi_ddt_destroy( newtype );
      OMPI_ERRHANDLER_RETURN( rc, (ompi_communicator_t*)NULL,
                             rc, FUNC_NAME );
   }

   {
      int* a_i[3];

      a_i[0] = &count;
      a_i[1] = array_of_blocklengths;
      a_i[2] = array_of_displacements;

      ompi_ddt_set_args( *newtype, 2 * count + 1, a_i, 0, NULL, 1, &oldtype,
                        MPI_COMBINER_INDEXED );
   }

   return MPI_SUCCESS;
}
