#include <stdio.h>
#include <unistd.h>
#include "mpi.h"

const int count = 1234;
int buffer[1234] = {0};

int main(int argc, char *argv[])
{
    int rank, size, i;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);


    for (i=0; i < 1000; i++) {
        fprintf(stderr, "%d: Executing Bcast #%d\n", rank, i);
        MPI_Bcast(buffer, count, MPI_INT, 0, MPI_COMM_WORLD);
        if (0 != rank) {
            sleep(1);
        }
    }

    MPI_Finalize();
    return 0;
}

