/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "mca/topo/topo.h"
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Cart_map = PMPI_Cart_map
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Cart_map(MPI_Comm comm, int ndims, int *dims,
                int *periods, int *newrank) {
    int err;
    mca_topo_base_cart_map_fn_t func;

    /* check the arguments */
    if (MPI_PARAM_CHECK) {
        if (MPI_COMM_NULL == comm) {
            return LAM_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_COMM,
                                          "MPI_Cart_map");
        }
        if (LAM_COMM_IS_INTER(comm)) { 
            return LAM_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_COMM,
                                          "MPI_Cart_map");
        }
        if(!LAM_COMM_IS_CART(comm)) {
            return LAM_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_TOPOLOGY,
                                          "MPI_Cart_map");
        }
        if ((NULL == dims) || (NULL == periods) || (NULL == newrank)) {
            return LAM_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_ARG,
                                          "MPI_Cart_map");
        }
    }

    /* get the function pointer on this communicator */
    func = comm->c_topo.topo_cart_map;
    if (NULL == func) {
        return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_OTHER, 
                                     "MPI_Cart_map");
    }
    /* call the function */
    if ( MPI_SUCCESS != 
            (err = func(comm, ndims, dims, periods, newrank))) {
        return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, err, "MPI_Cart_map");
    }

    return MPI_SUCCESS;
}
