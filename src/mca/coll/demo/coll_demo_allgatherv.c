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
 *	allgatherv_intra
 *
 *	Function:	- allgather using other MPI collectives
 *	Accepts:	- same as MPI_Allgatherv()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_allgatherv_intra(void *sbuf, int scount, 
                                   struct ompi_datatype_t *sdtype, 
                                   void * rbuf, int *rcounts, int *disps, 
                                   struct ompi_datatype_t *rdtype, 
                                   struct ompi_communicator_t *comm)
{
  ompi_output_verbose(10, mca_coll_base_output, "In demo allgatherv_intra");
  return comm->c_coll_basic_module->coll_allgatherv(sbuf, scount, sdtype,
                                                    rbuf, rcounts, disps,
                                                    rdtype, comm);
}


/*
 *	allgatherv_inter
 *
 *	Function:	- allgather using other MPI collectives
 *	Accepts:	- same as MPI_Allgatherv()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_allgatherv_inter(void *sbuf, int scount, 
                                    struct ompi_datatype_t *sdtype, 
                                    void * rbuf, int *rcounts, int *disps, 
                                    struct ompi_datatype_t *rdtype, 
                                    struct ompi_communicator_t *comm)
{
  ompi_output_verbose(10, mca_coll_base_output, "In demo allgatherv_inter");
  return comm->c_coll_basic_module->coll_allgatherv(sbuf, scount, sdtype,
                                                    rbuf, rcounts, disps,
                                                    rdtype, comm);
}
