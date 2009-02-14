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
#include "opal/util/output.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
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
                              struct ompi_communicator_t *comm,
                              mca_coll_base_module_t *module)
{
    mca_coll_demo_module_t *demo_module = (mca_coll_demo_module_t*) module;
    opal_output_verbose(10, mca_coll_base_output, "In demo alltoallv_intra");
    return demo_module->underlying.coll_alltoallv(sbuf, scounts, sdisps,
                                                  sdtype, rbuf, rcounts,
                                                  rdisps, rdtype, comm,
                                                  demo_module->underlying.coll_alltoallv_module);
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
                              struct ompi_communicator_t *comm,
                              mca_coll_base_module_t *module)
{
    mca_coll_demo_module_t *demo_module = (mca_coll_demo_module_t*) module;
    opal_output_verbose(10, mca_coll_base_output, "In demo alltoallv_inter");
    return demo_module->underlying.coll_alltoallv(sbuf, scounts, sdisps,
                                                  sdtype, rbuf, rcounts,
                                                  rdisps, rdtype, comm,
                                                  demo_module->underlying.coll_alltoallv_module);
}
