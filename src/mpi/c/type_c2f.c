/*
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "datatype/datatype.h"
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_c2f = PMPI_Type_c2f
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Type_c2f";


MPI_Fint MPI_Type_c2f(MPI_Datatype datatype)
{
  if (MPI_PARAM_CHECK) {
    OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

	/* mapping an invalid handle to a null handle */
	/* not invoking an error handler */
    if (NULL == datatype) {
		datatype = MPI_DATATYPE_NULL;
    }
  }

  /* Simple */

  return (MPI_Fint)(datatype->d_f_to_c_index);
}
