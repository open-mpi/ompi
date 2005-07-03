/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include "include/constants.h"
#include "opal/util/output.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"
#include "coll_demo.h"

/*
 *	gather_intra
 *
 *	Function:	- gather
 *	Accepts:	- same arguments as MPI_Gather()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_gather_intra(void *sbuf, int scount, 
                               struct ompi_datatype_t *sdtype, 
                               void *rbuf, int rcount, 
                               struct ompi_datatype_t *rdtype, 
                               int root, struct ompi_communicator_t *comm)
{
    opal_output_verbose(10, mca_coll_base_output, "In demo gather_intra");
    return comm->c_coll_basic_module->coll_gather(sbuf, scount, sdtype,
                                                  rbuf, rcount, rdtype,
                                                  root, comm);
}


/*
 *	gather_inter
 *
 *	Function:	- demo gather operation
 *	Accepts:	- same arguments as MPI_Gather()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_gather_inter(void *sbuf, int scount,
                               struct ompi_datatype_t *sdtype, 
                               void *rbuf, int rcount, 
                               struct ompi_datatype_t *rdtype, 
                               int root, struct ompi_communicator_t *comm)
{
    opal_output_verbose(10, mca_coll_base_output, "In demo gather_inter");
    return comm->c_coll_basic_module->coll_gather(sbuf, scount, sdtype,
                                                  rbuf, rcount, rdtype,
                                                  root, comm);
}
