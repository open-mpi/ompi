/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "mca/topo/topo.h"
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Cart_map = PMPI_Cart_map
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Cart_map(MPI_Comm comm, int ndims, int *dims,
                int *periods, int *newrank) {
    int err;
    mca_topo_base_cart_map_fn_t func;

    /* check the arguments */
    if (MPI_PARAM_CHECK) {
        if (MPI_COMM_NULL == comm) {
            return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_COMM,
                                          "MPI_Cart_map");
        }
        if (OMPI_COMM_IS_INTER(comm)) { 
            return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_COMM,
                                          "MPI_Cart_map");
        }
        if(!OMPI_COMM_IS_CART(comm)) {
            return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_TOPOLOGY,
                                          "MPI_Cart_map");
        }
        if ((NULL == dims) || (NULL == periods) || (NULL == newrank)) {
            return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_ARG,
                                          "MPI_Cart_map");
        }
    }

    /* get the function pointer on this communicator */
    func = comm->c_topo.topo_cart_map;
    if (NULL == func) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_OTHER, 
                                     "MPI_Cart_map");
    }
    /* call the function */
    if ( MPI_SUCCESS != 
            (err = func(comm, ndims, dims, periods, newrank))) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, err, "MPI_Cart_map");
    }

    return MPI_SUCCESS;
}
