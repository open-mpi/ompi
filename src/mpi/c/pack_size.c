/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "lfc/lam_object.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"
#include "datatype/datatype.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Pack_size = PMPI_Pack_size
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static char FUNC_NAME[] = "MPI_Pack_size";

int MPI_Pack_size(int incount, MPI_Datatype datatype, MPI_Comm comm,
                  int *size) 
{
    int ret;
    lam_convertor_t *local_convertor;

    if (MPI_PARAM_CHECK) {
      if (MPI_COMM_NULL == comm) {
	return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, FUNC_NAME);
      }

      if (NULL == size) {
	return LAM_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, FUNC_NAME);
      }
    
      if (MPI_DATATYPE_NULL == datatype) {
	return LAM_ERRHANDLER_INVOKE(comm, MPI_ERR_TYPE, FUNC_NAME);
      }
    }

    lam_convertor_init_for_send(local_convertor, 0, datatype, 
				incount, 0, NULL);
    ret = lam_convertor_get_packed_size(local_convertor, size);
    OBJ_RELEASE(local_convertor);

    LAM_ERRHANDLER_RETURN((ret >= 0), comm, MPI_ERR_UNKNOWN, FUNC_NAME);
}
