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
 *	allgather_intra
 *
 *	Function:	- allgather using other MPI collections
 *	Accepts:	- same as MPI_Allgather()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_allgather_intra(void *sbuf, int scount, 
                                  struct ompi_datatype_t *sdtype, void *rbuf, 
                                  int rcount, struct ompi_datatype_t *rdtype, 
                                  struct ompi_communicator_t *comm)
{
  ompi_output_verbose(10, mca_coll_base_output, "In demo allgather_intra");
  return comm->c_coll_basic_module->coll_allgather(sbuf, scount, sdtype, rbuf,
                                                   rcount, rdtype, comm);
}


/*
 *	allgather_inter
 *
 *	Function:	- allgather using other MPI collections
 *	Accepts:	- same as MPI_Allgather()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_allgather_inter(void *sbuf, int scount, 
                                  struct ompi_datatype_t *sdtype, 
                                  void *rbuf, int rcount, 
                                  struct ompi_datatype_t *rdtype, 
                                  struct ompi_communicator_t *comm)
{
  ompi_output_verbose(10, mca_coll_base_output, "In demo allgather_inter");
  return comm->c_coll_basic_module->coll_allgather(sbuf, scount, sdtype, rbuf,
                                                   rcount, rdtype, comm);
}
