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
 *	allgatherv_intra
 *
 *	Function:	- allgather
 *	Accepts:	- same as MPI_Allgatherv()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_allgatherv_intra(void *sbuf, int scount, 
                                   struct ompi_datatype_t *sdtype, 
                                   void * rbuf, int *rcounts, int *disps, 
                                   struct ompi_datatype_t *rdtype, 
                                   struct ompi_communicator_t *comm,
                                   mca_coll_base_module_t *module)
{
    mca_coll_demo_module_t *demo_module = (mca_coll_demo_module_t*) module;
    opal_output_verbose(10, mca_coll_base_output, "In demo allgatherv_intra");
    return demo_module->underlying.coll_allgatherv(sbuf, scount, sdtype,
                                                   rbuf, rcounts, disps,
                                                   rdtype, comm,
                                                   demo_module->underlying.coll_allgatherv_module);
}


/*
 *	allgatherv_inter
 *
 *	Function:	- allgather
 *	Accepts:	- same as MPI_Allgatherv()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_allgatherv_inter(void *sbuf, int scount, 
                                    struct ompi_datatype_t *sdtype, 
                                    void * rbuf, int *rcounts, int *disps, 
                                    struct ompi_datatype_t *rdtype, 
                                   struct ompi_communicator_t *comm,
                                   mca_coll_base_module_t *module)
{
    mca_coll_demo_module_t *demo_module = (mca_coll_demo_module_t*) module;
    opal_output_verbose(10, mca_coll_base_output, "In demo allgatherv_inter");
    return demo_module->underlying.coll_allgatherv(sbuf, scount, sdtype,
                                                   rbuf, rcounts, disps,
                                                   rdtype, comm,
                                                   demo_module->underlying.coll_allgatherv_module);
}
