/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>
#include <stdlib.h>

#include "mpi.h"
#include "mca/mca.h"
#include "mca/base/base.h"


int mca_base_alloc_mem(MPI_Aint size, MPI_Info info, void *baseptr)
{
  void *temp;

  /* Error checks */

  if (0 == size)
    return MPI_SUCCESS;
  else if (size < 0)
    return LAM_ERROR;

  /* Do the alloc */

  temp = malloc(size);
  if (NULL == temp)
    return LAM_ERROR;

  /* All done */

  *((void **) baseptr) = temp;
  return MPI_SUCCESS;
}


int mca_base_free_mem(void *baseptr)
{
  if (NULL != baseptr)
    free(baseptr);

  return MPI_SUCCESS;
}

