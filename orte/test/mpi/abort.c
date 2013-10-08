/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include <stdio.h>
#include <stdlib.h>

#include "mpi.h"

int main(int argc, char* argv[])
{
    int rank, size;
    int errcode;

    if (1 < argc) {
        errcode = strtol(argv[1], NULL, 10);
    } else {
        errcode = 2;
    }

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    printf("Hello, World, I am %d of %d\n", rank, size);

    if (1 == rank) {
        MPI_Abort(MPI_COMM_WORLD, errcode);
    } else {
        errcode = 0;
    }

    MPI_Finalize();
    return errcode;
}
