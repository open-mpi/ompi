/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "datatype/datatype.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"
#include "class/ompi_object.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Unpack = PMPI_Unpack
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static char FUNC_NAME[] = "MPI_Unpack";


int MPI_Unpack(void *inbuf, int insize, int *position,
               void *outbuf, int outcount, MPI_Datatype datatype,
               MPI_Comm comm) 
{
    int size, rc;
    ompi_convertor_t *local_convertor;
    struct iovec outvec;

    if (MPI_PARAM_CHECK) {
      if (MPI_COMM_NULL == comm) {
	return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM,
                                     "MPI_Unpack");
      }
      
      if ((NULL == inbuf) || (NULL == outbuf) || (NULL == position)) {
	return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, FUNC_NAME);
      }
    
      if (outcount < 0) {
	return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_COUNT, FUNC_NAME);
      }

      if (MPI_DATATYPE_NULL == datatype) {
	return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_TYPE, FUNC_NAME);
      }
    }

    local_convertor = ompi_convertor_get_copy(local_convertor);
    ompi_convertor_init_for_recv(local_convertor, 0, datatype, outcount, 
				inbuf, 0);
    
    /* how long is the data ? Can we put it in the user buffer */
    ompi_convertor_get_packed_size(local_convertor, &size);
    if ((outcount - (*position)) < size) {
      return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, FUNC_NAME);
    }

    /* Prepare the iovec withh all informations */
    outvec.iov_base = (char*) inbuf + (*position);

    /* If the position is not ZERO we already start
     * the packing for this datatype.
     */
    outvec.iov_len = insize - (*position);

    /* Do the actual unpacking */
    rc = ompi_convertor_unpack(local_convertor, &outvec, 1);
    *position += local_convertor->bConverted;

    /* Release the convertor. For Open MPI you should simply use
     * OBJ_RELEASE.
     */
    OBJ_RELEASE(local_convertor);

    OMPI_ERRHANDLER_RETURN(rc, comm, MPI_ERR_UNKNOWN, FUNC_NAME);
}
