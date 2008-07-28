/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "orte/util/show_help.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "coll_demo.h"


/*
 *	alltoallw_intra
 *
 *	Function:	- MPI_Alltoallw
 *	Accepts:	- same as MPI_Alltoallw()
 *	Returns:	- MPI_SUCCESS or an MPI error code
 */
int mca_coll_demo_alltoallw_intra(void *sbuf, int *scounts, int *sdisps,
                                  struct ompi_datatype_t **sdtypes, 
                                  void *rbuf, int *rcounts, int *rdisps,
                                  struct ompi_datatype_t **rdtypes, 
                                  struct ompi_communicator_t *comm,
                                  mca_coll_base_module_t *module)
{
    mca_coll_demo_module_t *demo_module = (mca_coll_demo_module_t*) module;
    opal_output_verbose(10, mca_coll_base_output, "In demo alltoallw_intra");
    return demo_module->underlying.coll_alltoallw(sbuf, scounts, sdisps,
                                                  sdtypes, rbuf, rcounts,
                                                  rdisps, rdtypes, comm,
                                                  demo_module->underlying.coll_alltoallw_module);
}


/*
 *	alltoallw_inter
 *
 *	Function:	- MPI_Alltoallw
 *	Accepts:	- same as MPI_Alltoallw()
 *	Returns:	- MPI_SUCCESS or an MPI error code
 */
int mca_coll_demo_alltoallw_inter(void *sbuf, int *scounts, int *sdisps,
                                  struct ompi_datatype_t **sdtypes,
                                  void *rbuf, int *rcounts, int *rdisps,
                                  struct ompi_datatype_t **rdtypes,
                                  struct ompi_communicator_t *comm,
                                  mca_coll_base_module_t *module)
{
    mca_coll_demo_module_t *demo_module = (mca_coll_demo_module_t*) module;
    opal_output_verbose(10, mca_coll_base_output, "In demo alltoallw_inter");
    return demo_module->underlying.coll_alltoallw(sbuf, scounts, sdisps,
                                                  sdtypes, rbuf, rcounts,
                                                  rdisps, rdtypes, comm,
                                                  demo_module->underlying.coll_alltoallw_module);
}
