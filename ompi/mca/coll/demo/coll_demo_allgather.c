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
 *	allgather_intra
 *
 *	Function:	- allgather
 *	Accepts:	- same as MPI_Allgather()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_allgather_intra(void *sbuf, int scount, 
                                  struct ompi_datatype_t *sdtype, void *rbuf, 
                                  int rcount, struct ompi_datatype_t *rdtype, 
                                  struct ompi_communicator_t *comm,
                                  struct mca_coll_base_module_1_1_0_t *module)
{
    mca_coll_demo_module_t *demo_module = (mca_coll_demo_module_t*) module;
    opal_output_verbose(10, mca_coll_base_output, "In demo allgather_intra");
    return demo_module->underlying.coll_allgather(sbuf, scount, sdtype, rbuf,
                                                  rcount, rdtype, comm,
                                                  demo_module->underlying.coll_allgather_module);
}


/*
 *	allgather_inter
 *
 *	Function:	- allgather
 *	Accepts:	- same as MPI_Allgather()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_allgather_inter(void *sbuf, int scount, 
                                  struct ompi_datatype_t *sdtype, 
                                  void *rbuf, int rcount, 
                                  struct ompi_datatype_t *rdtype, 
                                  struct ompi_communicator_t *comm,
                                  struct mca_coll_base_module_1_1_0_t *module)
{
    mca_coll_demo_module_t *demo_module = (mca_coll_demo_module_t*) module;
    opal_output_verbose(10, mca_coll_base_output, "In demo allgather_inter");
    return demo_module->underlying.coll_allgather(sbuf, scount, sdtype, rbuf,
                                                  rcount, rdtype, comm,
                                                  demo_module->underlying.coll_allgather_module);
}
