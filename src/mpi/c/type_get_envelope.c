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
#pragma weak MPI_Type_get_envelope = PMPI_Type_get_envelope
#endif

static char FUNC_NAME[] = "MPI_Type_get_envelope";

int
MPI_Type_get_envelope(MPI_Datatype type,
                      int *num_integers,
                      int *num_addresses,
                      int *num_datatypes,
                      int *combiner)
{
   int rc;

   if( MPI_PARAM_CHECK ) {
      if( LAM_MPI_INVALID_STATE ) {
         LAM_ERRHANDLER_RETURN( MPI_ERR_INTERN, (lam_communicator_t*)NULL,
                                MPI_ERR_INTERN, FUNC_NAME );
      }
   }

   rc = lam_ddt_get_args( type, 0, num_integers, NULL, num_addresses, NULL, num_datatypes,
                          NULL, combiner );

   LAM_ERRHANDLER_RETURN( rc, (lam_communicator_t*)NULL, rc, FUNC_NAME );
}
