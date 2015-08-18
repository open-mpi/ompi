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
 *	bcast_intra
 *
 *	Function:	- broadcast
 *	Accepts:	- same arguments as MPI_Bcast()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_bcast_intra(void *buff, int count,
                              struct ompi_datatype_t *datatype, int root,
                              struct ompi_communicator_t *comm,
                              mca_coll_base_module_t *module)
{
    mca_coll_demo_module_t *demo_module = (mca_coll_demo_module_t*) module;
    opal_output_verbose(10, ompi_coll_base_framework.framework_output, "In demo bcast_intra");
    return demo_module->underlying.coll_bcast(buff, count, datatype,
                                              root, comm,
                                              demo_module->underlying.coll_bcast_module);
}


/*
 *	bcast_inter
 *
 *	Function:	- broadcast
 *	Accepts:	- same arguments as MPI_Bcast()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_bcast_inter(void *buff, int count,
                              struct ompi_datatype_t *datatype, int root,
                              struct ompi_communicator_t *comm,
                              mca_coll_base_module_t *module)
{
    mca_coll_demo_module_t *demo_module = (mca_coll_demo_module_t*) module;
    opal_output_verbose(10, ompi_coll_base_framework.framework_output, "In demo bcast_inter");
    return demo_module->underlying.coll_bcast(buff, count, datatype,
                                              root, comm,
                                              demo_module->underlying.coll_bcast_module);
}
