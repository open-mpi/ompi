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

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Cart_sub = PMPI_Cart_sub
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif


int MPI_Cart_sub(MPI_Comm comm, int *remain_dims, MPI_Comm *new_comm) {
    int err;
    mca_topo_base_cart_sub_fn_t func;

    /* check the arguments */
    if (MPI_PARAM_CHECK) {
        if (MPI_COMM_NULL == comm) {
            return LAM_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_COMM,
                                          "MPI_Cart_sub");
        }
        if (LAM_COMM_IS_INTER(comm)) { 
            return LAM_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_COMM,
                                          "MPI_Cart_sub");
        }
        if (!LAM_COMM_IS_CART(comm)) {
            return LAM_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_TOPOLOGY,
                                          "MPI_Cart_sub");
        }
        if (NULL == remain_dims || NULL == new_comm) {
            return LAM_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_ARG,
                                          "MPI_Cart_sub");
        }
    }

    /* get the function pointer on this communicator */
    func = comm->c_topo.topo_cart_sub;
    if (NULL == func) {
        return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_OTHER, 
                                     "MPI_Cart_sub");
    }
    /* call the function */
    if ( MPI_SUCCESS != 
            (err = func(comm, remain_dims, new_comm))) {
        return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, err, "MPI_Cart_sub");
    }

    /* all done */
    return MPI_SUCCESS;
}
