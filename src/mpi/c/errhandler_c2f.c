/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Errhandler_c2f = PMPI_Errhandler_c2f
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Errhandler_c2f";


OMPI_EXPORT
MPI_Fint MPI_Errhandler_c2f(MPI_Errhandler errhandler)
{
  /* Error checking */

  if (MPI_PARAM_CHECK) {
    OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

	/* mapping an invalid handle to a null handle */
	/* also checks errhandler type matches */
	/* not invoking an error handler */
    if (NULL == errhandler ||
        OMPI_ERRHANDLER_TYPE_COMM != errhandler->eh_mpi_object_type) {
		errhandler = MPI_ERRHANDLER_NULL;
    }
  }


  return (MPI_Fint) errhandler->eh_f_to_c_index;
}
