/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "runtime/runtime.h"
#include "mpi/c/bindings.h"
#include "datatype/datatype.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_create_hvector = PMPI_Type_create_hvector
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static char FUNC_NAME[] = "MPI_Type_create_hvector";

int
MPI_Type_create_hvector(int count,
                        int blocklength,
                        MPI_Aint stride,
                        MPI_Datatype oldtype,
                        MPI_Datatype *newtype)
{
   int rc;

   if( MPI_PARAM_CHECK ) {
      if( LAM_MPI_INVALID_STATE ) {
         LAM_ERRHANDLER_RETURN( MPI_ERR_INTERN, (lam_communicator_t*)NULL,
                                MPI_ERR_INTERN, FUNC_NAME );
      }
      if( count < 0 ) {
         LAM_ERRHANDLER_RETURN( MPI_ERR_COUNT, (lam_communicator_t*)NULL,
                                MPI_ERR_COUNT, FUNC_NAME );
      }
      if( blocklength < 0) {
         LAM_ERRHANDLER_RETURN( MPI_ERR_ARG, (lam_communicator_t*)NULL,
                                MPI_ERR_ARG, FUNC_NAME );
      }
   }

   rc = lam_ddt_create_hvector ( count, blocklength, stride, oldtype, newtype );
   LAM_ERRHANDLER_CHECK(rc, (lam_communicator_t*)NULL, rc, FUNC_NAME );

   {
      int* a_i[3];
      a_i[0] = &count;
      a_i[1] = &blocklength;
      a_i[2] = (int*)&stride;

      lam_ddt_set_args( *newtype, 3, a_i, 0, NULL, 1, &oldtype, MPI_COMBINER_HVECTOR );
   }
   return MPI_SUCCESS;
}
