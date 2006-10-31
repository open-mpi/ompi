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
    char hostname[512];
    void *appnum;
    int flag;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_get_attr(MPI_COMM_WORLD, MPI_APPNUM, &appnum, &flag);

    gethostname(hostname, 512);
    printf("Hello, World, I am %d of %d on host %s from app number %d\n",
                            rank, size, hostname, *(int*)appnum);

    MPI_Finalize();
    return 0;
}
