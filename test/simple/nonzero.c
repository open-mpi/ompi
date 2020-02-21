#include <stdlib.h>
#include <unistd.h>
#include <mpi.h>

int main(int argc, char **argv)
{
    int rank;

    if(argc < 2) {
        return 0;
    }
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    int i = atoi(argv[1]);

    MPI_Finalize();

    if (i != rank) {
        sleep(1);
    }
    return i;
}
