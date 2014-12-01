/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include <stdio.h>
#include "mpi.h"

int main(int argc, char* argv[])
{
    int rank, size;
    MPI_Comm comm;
    int ierr;
    
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    printf("Hello, World, I am %d of %d\n", rank, size);

    if (MPI_SUCCESS != MPI_Errhandler_set(MPI_COMM_WORLD, MPI_ERRORS_RETURN)) {
        fprintf(stderr, "Errhandler set failed\n");
        MPI_Finalize();
        return 1;
    }
    if (MPI_SUCCESS != MPI_Comm_dup(MPI_COMM_WORLD, &comm)) {
        fprintf(stderr, "Unable to dup communicator\n");
        MPI_Finalize();
        return 1;
    }
    if (MPI_SUCCESS != MPI_Errhandler_set(comm, MPI_ERRORS_ARE_FATAL)) {
        fprintf(stderr, "Errhandler set on new communicator failed\n");
        MPI_Finalize();
        return 1;
    }

    /* send to someone that doesn't exist to trip the errhandler */
    if (0 == rank) {
        /* delay for a bit first */
        sleep(1);
    }
    MPI_Send(&ierr, 1, MPI_INT, size, 0, comm);

    MPI_Finalize();
    return 0;
}
