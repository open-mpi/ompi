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
 *	allgather_intra
 *
 *	Function:	- allgather using other MPI collections
 *	Accepts:	- same as MPI_Allgather()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_allgather_intra(void *sbuf, int scount, 
                                   struct ompi_datatype_t *sdtype, void *rbuf, 
                                   int rcount, struct ompi_datatype_t *rdtype, 
                                   struct ompi_communicator_t *comm)
{
  int size;
  int err;
  
  /* Gather and broadcast. */

  size = ompi_comm_size(comm);

  err = comm->c_coll.coll_gather(sbuf, scount, sdtype, rbuf, rcount, 
                                 rdtype, 0, comm);
  if (MPI_SUCCESS != err)
    return err;
  
  err = comm->c_coll.coll_bcast(rbuf, rcount * size, rdtype, 0, comm);
  return err;
}


/*
 *	allgather_inter
 *
 *	Function:	- allgather using other MPI collections
 *	Accepts:	- same as MPI_Allgather()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_allgather_inter(void *sbuf, int scount, 
                                   struct ompi_datatype_t *sdtype, 
                                   void *rbuf, int rcount, 
                                   struct ompi_datatype_t *rdtype, 
                                   struct ompi_communicator_t *comm)
{
  /* Need to implement this */

  return OMPI_ERR_NOT_IMPLEMENTED;
}
