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
 * Copyright (c) 2024      NVIDIA Corporation.  All rights reserved.
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
 *	allgather_intra
 *
 *	Function:	- allgather
 *	Accepts:	- same as MPI_Allgather()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_allgather_intra(const void *sbuf, size_t scount,
                                  struct ompi_datatype_t *sdtype, void *rbuf,
                                  size_t rcount, struct ompi_datatype_t *rdtype,
                                  struct ompi_communicator_t *comm,
                                  mca_coll_base_module_t *module)
{
    mca_coll_demo_module_t *demo_module = (mca_coll_demo_module_t*) module;
    opal_output_verbose(10, ompi_coll_base_framework.framework_output, "In demo allgather_intra");
    return demo_module->c_coll.coll_allgather(sbuf, scount, sdtype, rbuf,
                                              rcount, rdtype, comm,
                                              demo_module->c_coll.coll_allgather_module);
}


/*
 *	allgather_inter
 *
 *	Function:	- allgather
 *	Accepts:	- same as MPI_Allgather()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_allgather_inter(const void *sbuf, size_t scount,
                                  struct ompi_datatype_t *sdtype,
                                  void *rbuf, size_t rcount,
                                  struct ompi_datatype_t *rdtype,
                                  struct ompi_communicator_t *comm,
                                  mca_coll_base_module_t *module)
{
    mca_coll_demo_module_t *demo_module = (mca_coll_demo_module_t*) module;
    opal_output_verbose(10, ompi_coll_base_framework.framework_output, "In demo allgather_inter");
    return demo_module->c_coll.coll_allgather(sbuf, scount, sdtype, rbuf,
                                              rcount, rdtype, comm,
                                              demo_module->c_coll.coll_allgather_module);
}
