/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
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
 *	alltoallv_intra
 *
 *	Function:	- MPI_Alltoallv for non-ompid RPIs
 *	Accepts:	- same as MPI_Alltoallv()
 *	Returns:	- MPI_SUCCESS or an MPI error code
 */
int
mca_coll_demo_alltoallv_intra(void *sbuf, int *scounts, int *sdisps,
                              struct ompi_datatype_t *sdtype,
                              void *rbuf, int *rcounts, int *rdisps,
                              struct ompi_datatype_t *rdtype, 
                              struct ompi_communicator_t *comm)
{
  ompi_output_verbose(10, mca_coll_base_output, "In demo alltoallv_intra");
  return comm->c_coll_basic_module->coll_alltoallv(sbuf, scounts, sdisps,
                                                   sdtype, rbuf, rcounts,
                                                   rdisps, rdtype, comm);
}


/*
 *	alltoallv_inter
 *
 *	Function:	- MPI_Alltoallv for non-lamd RPIs
 *	Accepts:	- same as MPI_Alltoallv()
 *	Returns:	- MPI_SUCCESS or an MPI error code
 */
int
mca_coll_demo_alltoallv_inter(void *sbuf, int *scounts, int *sdisps,
                              struct ompi_datatype_t *sdtype, void *rbuf,
                              int *rcounts, int *rdisps,
                              struct ompi_datatype_t *rdtype, 
                              struct ompi_communicator_t *comm)
{
  ompi_output_verbose(10, mca_coll_base_output, "In demo alltoallv_inter");
  return comm->c_coll_basic_module->coll_alltoallv(sbuf, scounts, sdisps,
                                                   sdtype, rbuf, rcounts,
                                                   rdisps, rdtype, comm);
}
