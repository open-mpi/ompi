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
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/mca/topo/topo.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Cart_get = PMPI_Cart_get
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Cart_get";


int MPI_Cart_get(MPI_Comm comm, int maxdims, int *dims,
                 int *periods, int *coords) 
{
    /* local variables */
    mca_topo_base_module_cart_get_fn_t func;
    int err;

    /* check the arguments */
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (MPI_COMM_NULL == comm || OMPI_COMM_IS_INTER(comm)) {
            return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_COMM,
                                          FUNC_NAME);
        }
        if (!OMPI_COMM_IS_CART(comm)) {
            return OMPI_ERRHANDLER_INVOKE (comm, MPI_ERR_TOPOLOGY,
                                          FUNC_NAME);
        }
        if ((0 > maxdims) || (0 < maxdims && 
                              ((NULL == dims) || (NULL == periods) ||
                               (NULL == coords)))) {
            return OMPI_ERRHANDLER_INVOKE (comm, MPI_ERR_ARG,
                                          FUNC_NAME);
        }
    }
    /* get the function pointer to do the right thing */
    func = comm->c_topo->topo_cart_get;

    /* all arguments are checked and now call the back end function */
    if ( MPI_SUCCESS != 
            (err = func(comm, maxdims, dims, periods, coords))) {
        return OMPI_ERRHANDLER_INVOKE(comm, err, FUNC_NAME);
    }
    
    /* All done */
    return MPI_SUCCESS;
}
