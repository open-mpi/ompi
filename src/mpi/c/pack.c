/* -*- Mode: C; c-basic-offset:4 ; -*- */

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
#pragma weak MPI_Pack = PMPI_Pack
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Pack";


int MPI_Pack(void *inbuf, int incount, MPI_Datatype datatype,
             void *outbuf, int outsize, int *position, MPI_Comm comm)
{
  int rc, freeAfter;
  ompi_convertor_t *local_convertor;
  struct iovec invec;
  unsigned int size, iov_count;

  if (MPI_PARAM_CHECK) {
    OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    if (MPI_COMM_NULL == comm) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, FUNC_NAME);
    } else if ((NULL == inbuf) || (NULL == outbuf) || (NULL == position)) {
      return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, FUNC_NAME);
    } else if (incount < 0) {
      return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_COUNT, FUNC_NAME);
    } else if (outsize < 0) {
      return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, FUNC_NAME);
    } else if (MPI_DATATYPE_NULL == datatype) {
      return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_TYPE, FUNC_NAME);
    }
  }

  local_convertor = OBJ_NEW(ompi_convertor_t);
  ompi_convertor_init_for_send(local_convertor, 0, datatype, incount, 
			       inbuf, 0, NULL /*never allocate memory*/);

  /* Check for truncation */

  ompi_convertor_get_packed_size(local_convertor, &size);
  if (*position + size > outsize) {
    OBJ_RELEASE(local_convertor);
    return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_TRUNCATE, FUNC_NAME);
  }

  /* Prepare the iovec with all informations */

  invec.iov_base = (char*) outbuf + (*position);
  invec.iov_len = outsize - (*position);

  /* Do the actual packing */

  iov_count = 1;
  rc = ompi_convertor_pack( local_convertor, &invec, &iov_count,
			    &size, &freeAfter );
  *position += local_convertor->bConverted;
  OBJ_RELEASE(local_convertor);

  /* All done.  Note that the convertor returns 1 upon success, not
     OMPI_SUCCESS. */
    
  OMPI_ERRHANDLER_RETURN((rc == 1) ? OMPI_SUCCESS : OMPI_ERROR,
			 comm, MPI_ERR_UNKNOWN, FUNC_NAME);
}
