/*
 * $HEADER$
 */

#include "lam_config.h"
#include "coll_basic.h"

#include "constants.h"
#include "mpi.h"
#include "communicator/communicator.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/coll_tags.h"
#include "coll_basic.h"


/*
 *	allgather
 *
 *	Function:	- allgather using other MPI collections
 *	Accepts:	- same as MPI_Allgather()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_allgather(void *sbuf, int scount, 
                             MPI_Datatype sdtype, void *rbuf, 
                             int rcount, MPI_Datatype rdtype, 
                             MPI_Comm comm)
{
  int size;
  int err;
  
  /* Gather and broadcast. */

  size = lam_comm_size(comm);

  err = comm->c_coll.coll_gather_intra(sbuf, scount, sdtype, rbuf, rcount, 
                                       rdtype, 0, comm);
  if (MPI_SUCCESS != err)
    return err;
  
  err = comm->c_coll.coll_bcast_intra(rbuf, rcount * size, rdtype, 
                                      0, comm);
  return err;
}
