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
    bool re_order = false;

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
        if (0 > reorder || 1 < reorder) {
            return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_ARG,
                                          "MPI_Cart_create");
        }

        /* check if the number of processes on the grid are corrct */
        {
           int i;
           int *p = dims;
           int count_nodes = 1;
           int parent_procs = ompi_comm_size(old_comm);

           for (i=0; i < ndims; i++) {
               count_nodes *= *p;
               p++;
           }

           if (parent_procs < count_nodes) {
               return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_ARG,
                                              "MPI_Cart_create");
           }
        }
    }

    /* everything seems to be alright with the communicator, we can go 
     * ahead and select a topology module for this purpose and create 
     * the new cartesian communicator
     */

    re_order = (1 == reorder)? true :false;

    err = ompi_topo_create (old_comm,
                            ndims,
                            dims,
                            periods,
                            re_order,
                            comm_cart,
                            OMPI_COMM_CART);

    /* check the error status */
    if (MPI_SUCCESS != err) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, err, "MPI_Cart_create");
    }
    
    /* All done */
    return MPI_SUCCESS;
}
