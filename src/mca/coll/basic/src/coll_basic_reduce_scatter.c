/*
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_basic.h"

#include <stdio.h>
#include <errno.h>

#include "mpi.h"
#include "include/constants.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/coll_tags.h"
#include "coll_basic.h"


/*
 *	reduce_scatter
 *
 *	Function:	- reduce then scatter
 *	Accepts:	- same as MPI_Reduce_scatter()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_reduce_scatter_intra(void *sbuf, void *rbuf, int *rcounts,
                                        struct ompi_datatype_t *dtype,
                                        struct ompi_op_t *op,
                                        struct ompi_communicator_t *comm)
{
  int i;
  int err;
  int rank;
  int size;
  int count;
  long true_lb, true_extent, lb, extent;
  int *disps = NULL;
  char *free_buffer = NULL;
  char *pml_buffer = NULL;

  /* Initialize */

  rank = ompi_comm_rank(comm);
  size = ompi_comm_size(comm);

  /* Initialize reduce & scatterv info at the root (rank 0). */

  for (i = 0, count = 0; i < size; ++i) {
    if (rcounts[i] < 0) {
      return EINVAL;
    }
    count += rcounts[i];
  }

  if (0 == rank) {
    disps = malloc((unsigned) size * sizeof(int));
    if (NULL == disps) {
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* There is lengthy rationale about how this malloc works in
       coll_basic_reduce.c */

    ompi_ddt_get_extent(dtype, &lb, &extent);
    ompi_ddt_get_true_extent(dtype, &true_lb, &true_extent);

    free_buffer = malloc(true_extent + (count - 1) * extent);
    if (NULL == free_buffer) {
      free(disps);
      return OMPI_ERR_OUT_OF_RESOURCE;
    }
    pml_buffer = free_buffer - lb;

    disps[0] = 0;
    for (i = 0; i < (size - 1); ++i) {
      disps[i + 1] = disps[i] + rcounts[i];
    }
  }

  /* reduction */

  err = comm->c_coll.coll_reduce(sbuf, pml_buffer, count, dtype, op, 0, comm);

  /* scatter */

  if (MPI_SUCCESS == err) {
    err = comm->c_coll.coll_scatterv(pml_buffer, rcounts, disps, dtype,
                                     rbuf, rcounts[rank], dtype, 0, comm);
  }

  /* All done */

  if (NULL != disps) {
    free(disps);
  }
  if (NULL != free_buffer) {
    free(free_buffer);
  }

  return err;
}


/*
 *	reduce_scatter_inter
 *
 *	Function:	- reduce/scatter operation
 *	Accepts:	- same arguments as MPI_Reduce_scatter()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_reduce_scatter_inter(void *sbuf, void *rbuf, int *rcounts,
                                        struct ompi_datatype_t *dtype,
                                        struct ompi_op_t *op,
                                        struct ompi_communicator_t *comm)
{
  return OMPI_ERR_NOT_IMPLEMENTED;
}
