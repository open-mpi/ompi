/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "communicator/communicator.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"
#include "coll_sm.h"


/*
 *	alltoall_intra
 *
 *	Function:	- MPI_Alltoall 
 *	Accepts:	- same as MPI_Alltoall()
 *	Returns:	- MPI_SUCCESS or an MPI error code
 */
int
mca_coll_sm_alltoall(void *sbuf, int scount,
		     struct ompi_datatype_t *sdtype, void *rbuf,
		     int rcount, struct ompi_datatype_t *rdtype,
		     struct ompi_communicator_t *comm)
{
    return MPI_SUCCESS;
}
