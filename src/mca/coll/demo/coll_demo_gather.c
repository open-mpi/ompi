/*
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_demo.h"

#include "mpi.h"
#include "include/constants.h"
#include "util/output.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"
#include "coll_demo.h"

/*
 *	gather_intra
 *
 *	Function:	- demo gather operation
 *	Accepts:	- same arguments as MPI_Gather()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_gather_intra(void *sbuf, int scount, 
                               struct ompi_datatype_t *sdtype, 
                               void *rbuf, int rcount, 
                               struct ompi_datatype_t *rdtype, 
                               int root, struct ompi_communicator_t *comm)
{
  ompi_output_verbose(10, mca_coll_base_output, "In demo gather_intra");
  return comm->c_coll_basic_module->coll_gather(sbuf, scount, sdtype,
                                                rbuf, rcount, rdtype,
                                                root, comm);
}


/*
 *	gather_inter
 *
 *	Function:	- demo gather operation
 *	Accepts:	- same arguments as MPI_Gather()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_gather_inter(void *sbuf, int scount,
                               struct ompi_datatype_t *sdtype, 
                               void *rbuf, int rcount, 
                               struct ompi_datatype_t *rdtype, 
                               int root, struct ompi_communicator_t *comm)
{
  ompi_output_verbose(10, mca_coll_base_output, "In demo gather_inter");
  return comm->c_coll_basic_module->coll_gather(sbuf, scount, sdtype,
                                                rbuf, rcount, rdtype,
                                                root, comm);
}
