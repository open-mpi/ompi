/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "class/ompi_object.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"
#include "datatype/datatype.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Pack_size = PMPI_Pack_size
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Pack_size";

int MPI_Pack_size(int incount, MPI_Datatype datatype, MPI_Comm comm,
                  int *size) 
{
    int ret;
    ompi_convertor_t *local_convertor;

    if (MPI_PARAM_CHECK) {
      OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
      if (MPI_COMM_NULL == comm) {
	return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, FUNC_NAME);
      } else if (NULL == size) {
	return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, FUNC_NAME);
      } else if (MPI_DATATYPE_NULL == datatype) {
	return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_TYPE, FUNC_NAME);
      }
    }

    ompi_convertor_init_for_send(local_convertor, 0, datatype, 
				incount, NULL, 0);
    ret = ompi_convertor_get_packed_size(local_convertor, size);
    OBJ_RELEASE(local_convertor);

    OMPI_ERRHANDLER_RETURN((ret >= 0), comm, MPI_ERR_UNKNOWN, FUNC_NAME);
}
