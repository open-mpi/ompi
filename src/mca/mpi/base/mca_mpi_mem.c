/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>
#include <stdlib.h>

#include "lam/mem/malloc.h"
#include "mpi.h"
#include "mca/mca.h"
#include "mca/mpi/base/base.h"


int mca_mpi_alloc_mem(MPI_Aint size, MPI_Info info, void *baseptr)
{
  void *temp;

  /* Error checks */

  if (0 == size)
    return MPI_SUCCESS;
  else if (size < 0)
    return LAM_ERROR;

  /* Do the alloc */

  temp = LAM_MALLOC(size);
  if (NULL == temp)
    return LAM_ERROR;

  /* All done */

  *((void **) baseptr) = temp;
  return MPI_SUCCESS;
}


int mca_mpi_free_mem(void *baseptr)
{
  if (NULL != baseptr)
    LAM_FREE(baseptr);

  return MPI_SUCCESS;
}

