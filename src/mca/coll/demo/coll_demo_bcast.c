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
 *	bcast_intra
 *
 *	Function:	- broadcast using O(N) algorithm
 *	Accepts:	- same arguments as MPI_Bcast()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_bcast_intra(void *buff, int count,
                              struct ompi_datatype_t *datatype, int root,
                              struct ompi_communicator_t *comm)
{
  ompi_output_verbose(10, mca_coll_base_output, "In demo bcast_intra");
  return comm->c_coll_basic_module->coll_bcast(buff, count, datatype,
                                               root, comm);
}


/*
 *	bcast_inter
 *
 *	Function:	- broadcast using O(N) algorithm
 *	Accepts:	- same arguments as MPI_Bcast()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_bcast_inter(void *buff, int count,
                              struct ompi_datatype_t *datatype, int root,
                              struct ompi_communicator_t *comm)
{
  ompi_output_verbose(10, mca_coll_base_output, "In demo bcast_inter");
  return comm->c_coll_basic_module->coll_bcast(buff, count, datatype,
                                               root, comm);
}
