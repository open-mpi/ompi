/*
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_basic.h"

#include "mpi.h"
#include "include/constants.h"
#include "communicator/communicator.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/coll_tags.h"
#include "coll_basic.h"


/*
 *	allgatherv_intra
 *
 *	Function:	- allgather using other MPI collectives
 *	Accepts:	- same as MPI_Allgatherv()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_allgatherv_intra(void *sbuf, int scount, 
                                    struct ompi_datatype_t *sdtype, 
                                    void * rbuf, int *rcounts, int *disps, 
                                    struct ompi_datatype_t *rdtype, 
                                    struct ompi_communicator_t *comm)
{
  int i, size;
  int err;

  /* Collect all values at each process, one at a time. */

  size = ompi_comm_size(comm);
  for (i = 0; i < size; ++i) {
    err = comm->c_coll.coll_gatherv(sbuf, scount, sdtype, rbuf,
                                    rcounts, disps, rdtype, i, comm);
    if (MPI_SUCCESS != err) {
      printf("allgatherv barfed: errcode %d\n", err);
      return err;
    }
  }

  return MPI_SUCCESS;
}


/*
 *	allgatherv_inter
 *
 *	Function:	- allgather using other MPI collectives
 *	Accepts:	- same as MPI_Allgatherv()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_allgatherv_inter(void *sbuf, int scount, 
                                    struct ompi_datatype_t *sdtype, 
                                    void * rbuf, int *rcounts, int *disps, 
                                    struct ompi_datatype_t *rdtype, 
                                    struct ompi_communicator_t *comm)
{
  /* Need to implement this */

  return OMPI_ERR_NOT_IMPLEMENTED;
}
