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
 *	alltoallw_intra
 *
 *	Function:	- MPI_Alltoallw for non-ompid RPIs
 *	Accepts:	- same as MPI_Alltoallw()
 *	Returns:	- MPI_SUCCESS or an MPI error code
 */
int mca_coll_demo_alltoallw_intra(void *sbuf, int *scounts, int *sdisps,
                                  struct ompi_datatype_t **sdtypes, 
                                  void *rbuf, int *rcounts, int *rdisps,
                                  struct ompi_datatype_t **rdtypes, 
                                  struct ompi_communicator_t *comm)
{
  ompi_output_verbose(10, mca_coll_base_output, "In demo alltoallw_intra");
  return comm->c_coll_basic_module->coll_alltoallw(sbuf, scounts, sdisps,
                                                   sdtypes, rbuf, rcounts,
                                                   rdisps, rdtypes, comm);
}


/*
 *	alltoallw_inter
 *
 *	Function:	- MPI_Alltoallw for non-lamd RPIs
 *	Accepts:	- same as MPI_Alltoallw()
 *	Returns:	- MPI_SUCCESS or an MPI error code
 */
int mca_coll_demo_alltoallw_inter(void *sbuf, int *scounts, int *sdisps,
                                  struct ompi_datatype_t **sdtypes,
                                  void *rbuf, int *rcounts, int *rdisps,
                                  struct ompi_datatype_t **rdtypes,
                                  struct ompi_communicator_t *comm)
{
  ompi_output_verbose(10, mca_coll_base_output, "In demo alltoallw_inter");
  return comm->c_coll_basic_module->coll_alltoallw(sbuf, scounts, sdisps,
                                                   sdtypes, rbuf, rcounts,
                                                   rdisps, rdtypes, comm);
}
