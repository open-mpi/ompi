/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"
#include "mca/topo/topo.h"
#include "errhandler/errhandler.h"
#include "group/group.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Cart_coords = PMPI_Cart_coords
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Cart_coords(MPI_Comm comm, int rank, int maxdims, int *coords) {
    int err;
    mca_topo_base_cart_coords_fn_t func;

    /* check the arguments */
    if (MPI_PARAM_CHECK) {
        if (MPI_COMM_NULL == comm) {
            return LAM_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_COMM,
                                          "MPI_Cart_coords");
        }
        if (LAM_COMM_IS_INTER(comm)) { 
            return LAM_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_COMM,
                                          "MPI_Cart_coords");
        }
        if (!LAM_COMM_IS_CART(comm)) {
            return LAM_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_TOPOLOGY,
                                          "MPI_Cart_coords");
        }
        if ( (0 > maxdims) || ((0 < maxdims) && (NULL == coords))) {
            return LAM_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_ARG,
                                          "MPI_Cart_coords");
        }
        if ((0 > rank) || (rank > lam_group_size(comm->c_local_group))) {
            return LAM_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_RANK,
                                          "MPI_Cart_coords");
        }
    }

    /* get the function pointer on this communicator */
    func = comm->c_topo.topo_cart_coords;
    if (NULL == func) {
        return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_OTHER, 
                                     "MPI_Cart_coords");
    }
    /* call the function */
    if ( MPI_SUCCESS != 
            (err = func(comm, rank, maxdims, coords))) {
        return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, err, "MPI_Cart_coords");
    }

    /* all done */
    return MPI_SUCCESS;
}
