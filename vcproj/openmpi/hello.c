/* 
 * $HEADER$
 */

#include <mpi.h>
#include <stdio.h>

int main(int argc, char **argv) {
    int rank;
    int size;
    MPI_Init(&argc, &argv);
    printf("Hello World!!\n");
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    printf ("I am rank %d of %d\n", rank, size);
    MPI_Finalize();
    return 0;
}
