/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"
#include "mca/topo/topo.h"
#include "errhandler/errhandler.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Cart_create = PMPI_Cart_create
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Cart_create(MPI_Comm old_comm, int ndims, int *dims,
                    int *periods, int reorder, MPI_Comm *comm_cart) {

    int err;
    mca_topo_base_cart_create_fn_t func;

    /* check the arguments */
    if (MPI_PARAM_CHECK) {
        if (MPI_COMM_NULL == old_comm) {
            return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_COMM,
                                          "MPI_Cart_create");
        }
        if (OMPI_COMM_IS_INTER(old_comm)) {
            return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_COMM,
                                          "MPI_Cart_create");
        }
        if (1 > ndims) {
            return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_ARG,
                                          "MPI_Cart_create");
        }
        if (NULL == dims || NULL == periods || NULL == comm_cart) {
            return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_ARG,
                                          "MPI_Cart_create");
        }
    }
    /* get the function pointer to do the right thing */
    func = old_comm->c_topo.topo_cart_create;
    if (NULL == func) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_OTHER, 
                                     "MPI_Cart_create");
    }

    /* call the function */
    if ( MPI_SUCCESS != 
            (err = func(old_comm, ndims, dims, periods, reorder, comm_cart))) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, err, "MPI_Cart_create");
    }
    
    /* All done */
    return MPI_SUCCESS;
}
