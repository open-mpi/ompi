/*
 * $HEADER$
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

static const char FUNC_NAME[] = "MPI_Unpack";


int MPI_Unpack(void *inbuf, int insize, int *position,
               void *outbuf, int outcount, MPI_Datatype datatype,
               MPI_Comm comm) 
{
  int rc, freeAfter;
  ompi_convertor_t *local_convertor;
  struct iovec outvec;
  unsigned int size, iov_count;

  if (MPI_PARAM_CHECK) {
    OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    if (ompi_comm_invalid(comm)) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM,
				    FUNC_NAME);
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

  local_convertor = OBJ_NEW(ompi_convertor_t);
  ompi_convertor_init_for_recv(local_convertor, 0, datatype, outcount, 
			       outbuf, 0, NULL /* never allocate memory */);
    
  /* Check for truncation */

  ompi_convertor_get_packed_size(local_convertor, &size);
  if( (*position + size) > (unsigned int)insize ) {
    OBJ_RELEASE(local_convertor);
    return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_TRUNCATE, FUNC_NAME);
  }

  /* Prepare the iovec with all informations */

  outvec.iov_base = (char*) inbuf + (*position);
  outvec.iov_len = insize - (*position);

  /* Do the actual unpacking */
  iov_count = 1;
  rc = ompi_convertor_unpack( local_convertor, &outvec, &iov_count,
			      &size, &freeAfter );
  *position += local_convertor->bConverted;
  OBJ_RELEASE(local_convertor);

  /* All done.  Note that the convertor returns 1 upon success, not
     OMPI_SUCCESS. */

  OMPI_ERRHANDLER_RETURN((rc == 1) ? OMPI_SUCCESS : OMPI_ERROR,
			 comm, MPI_ERR_UNKNOWN, FUNC_NAME);
}
