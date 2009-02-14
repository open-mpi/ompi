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
 *	gatherv_intra
 *
 *	Function:	- gatherv
 *	Accepts:	- same arguments as MPI_Gatherv()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_gatherv_intra(void *sbuf, int scount, 
                                struct ompi_datatype_t *sdtype,
                                void *rbuf, int *rcounts, int *disps,
                                struct ompi_datatype_t *rdtype, int root,
                                struct ompi_communicator_t *comm,
                                mca_coll_base_module_t *module)
{
    mca_coll_demo_module_t *demo_module = (mca_coll_demo_module_t*) module;
    opal_output_verbose(10, mca_coll_base_output, "In demo gatherv_intra");
    return demo_module->underlying.coll_gatherv(sbuf, scount, sdtype,
                                                rbuf, rcounts, disps,
                                                rdtype, root, comm,
                                                demo_module->underlying.coll_gatherv_module);
}


/*
 *	gatherv_inter
 *
 *	Function:	- gatherv
 *	Accepts:	- same arguments as MPI_Gatherv()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_gatherv_inter(void *sbuf, int scount,
                                struct ompi_datatype_t *sdtype,
                                void *rbuf, int *rcounts, int *disps,
                                struct ompi_datatype_t *rdtype, int root,
                                struct ompi_communicator_t *comm,
                                mca_coll_base_module_t *module)
{
    mca_coll_demo_module_t *demo_module = (mca_coll_demo_module_t*) module;
    opal_output_verbose(10, mca_coll_base_output, "In demo gatherv_inter");
    return demo_module->underlying.coll_gatherv(sbuf, scount, sdtype,
                                                rbuf, rcounts, disps,
                                                rdtype, root, comm,
                                                demo_module->underlying.coll_gatherv_module);
}
