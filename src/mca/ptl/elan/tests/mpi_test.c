#include <stdio.h>
#include "mpi.h"
#include "test_util.h"

int main (int argc, char ** argv)
{
    int proc, nproc;
    /* Get some environmental variables set for Open MPI, OOB */
    env_init_for_elan();

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &proc);
    MPI_Comm_size(MPI_COMM_WORLD, &nproc);
    MPI_Finalize();
    return 0;
}
