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
 *	reduce_intra
 *
 *	Function:	- reduction
 *	Accepts:	- same as MPI_Reduce()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_reduce_intra(void *sbuf, void *rbuf, int count,
                               struct ompi_datatype_t *dtype, 
                               struct ompi_op_t *op,
                               int root, struct ompi_communicator_t *comm,
                               mca_coll_base_module_t *module)
{
    mca_coll_demo_module_t *demo_module = (mca_coll_demo_module_t*) module;
    opal_output_verbose(10, mca_coll_base_output, "In demo reduce_intra");
    return demo_module->underlying.coll_reduce(sbuf, rbuf, count, dtype,
                                               op, root, comm,
                                               demo_module->underlying.coll_reduce_module);
}


/*
 *	reduce_inter
 *
 *	Function:	- reduction 
 *	Accepts:	- same as MPI_Reduce()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_reduce_inter(void *sbuf, void *rbuf, int count,
                               struct ompi_datatype_t *dtype, 
                               struct ompi_op_t *op,
                               int root, struct ompi_communicator_t *comm,
                               mca_coll_base_module_t *module)
{
    mca_coll_demo_module_t *demo_module = (mca_coll_demo_module_t*) module;
    opal_output_verbose(10, mca_coll_base_output, "In demo reduce_inter");
    return demo_module->underlying.coll_reduce(sbuf, rbuf, count, dtype,
                                               op, root, comm,
                                               demo_module->underlying.coll_reduce_module);
}
