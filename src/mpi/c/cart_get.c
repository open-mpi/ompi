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
#pragma weak MPI_Cart_get = PMPI_Cart_get
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Cart_get(MPI_Comm comm, int maxdims, int *dims,
                 int *periods, int *coords) {
    /* local variables */
    mca_topo_base_cart_get_fn_t func;
    int err;

    /* check the arguments */
    if (MPI_PARAM_CHECK) {
        if (MPI_COMM_NULL == comm || LAM_COMM_IS_INTER(comm)) {
            return LAM_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_COMM,
                                          "MPI_Cart_get");
        }
        if (!LAM_COMM_IS_CART(comm)) {
            return LAM_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_TOPOLOGY,
                                          "MPI_Cart_get");
        }
        if ((0 > maxdims) || (0 < maxdims && 
                        ((NULL == dims) || (NULL == periods) || (NULL == coords)))) {
            return LAM_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_ARG,
                                          "MPI_Cart_get");
        }
    }
    /* get the function pointer to do the right thing */
    func = comm->c_topo.topo_cart_get;
    if (NULL == func) {
        return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_OTHER, 
                                     "MPI_Cart_get");
    }

    /* all arguments are checked and now call the back end function */
    if ( MPI_SUCCESS != 
            (err = func(comm, maxdims, dims, periods, coords))) {
        return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, err, "MPI_Cart_get");
    }
    
    /* All done */
    return MPI_SUCCESS;
}
