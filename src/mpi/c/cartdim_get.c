/*
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Cartdim_get = PMPI_Cartdim_get
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif


static const char FUNC_NAME[] = "MPI_Cartdim_get";


int MPI_Cartdim_get(MPI_Comm comm, int *ndims) 
{
    mca_topo_base_module_cartdim_get_fn_t func;
    int err;

    if (MPI_PARAM_CHECK) {
       OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
       if (ompi_comm_invalid(comm)) {
           return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_COMM,
                                        FUNC_NAME);
       }
       if (OMPI_COMM_IS_INTER(comm)) { 
           return OMPI_ERRHANDLER_INVOKE (comm, MPI_ERR_COMM,
                                         FUNC_NAME);
       }
       if (!OMPI_COMM_IS_CART(comm)) {
           return OMPI_ERRHANDLER_INVOKE (comm, MPI_ERR_TOPOLOGY,
                                          FUNC_NAME);
       }
       if (NULL == ndims) {
           return OMPI_ERRHANDLER_INVOKE (comm, MPI_ERR_ARG,
                                          FUNC_NAME);
       }
    }

    /* get the function pointer on this communicator */
    func = comm->c_topo->topo_cartdim_get;

    /* call the function */
    if ( MPI_SUCCESS != 
              (err = func(comm, ndims))) {
         return OMPI_ERRHANDLER_INVOKE(comm, err, FUNC_NAME);
    }

    return MPI_SUCCESS;
}
