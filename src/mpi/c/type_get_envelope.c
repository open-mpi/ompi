/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "datatype/datatype.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_get_envelope = PMPI_Type_get_envelope
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Type_get_envelope";


OMPI_EXPORT
int MPI_Type_get_envelope(MPI_Datatype type,
                          int *num_integers,
                          int *num_addresses,
                          int *num_datatypes,
                          int *combiner)
{
   int rc;

   if( MPI_PARAM_CHECK ) {
      OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
      if (NULL == type || MPI_DATATYPE_NULL == type) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_TYPE,
                                      FUNC_NAME );
      } else if (NULL == num_integers || NULL == num_addresses ||
                 NULL == num_datatypes || NULL == combiner) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                      FUNC_NAME );
      }
   }

   rc = ompi_ddt_get_args( type, 0, num_integers, NULL, num_addresses, NULL, 
                           num_datatypes, NULL, combiner );
   OMPI_ERRHANDLER_RETURN( rc, MPI_COMM_WORLD, rc, FUNC_NAME );
}
