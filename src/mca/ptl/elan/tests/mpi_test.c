#include <stdio.h>
#include "mpi.h"
#include "test_util.h"

int main (int argc, char ** argv)
{
    char hostname[32];
    int proc, nproc;

    /* Get some environmental variables set for Open MPI, OOB */
    env_init_for_elan();
    gethostname(hostname, 32);

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &proc);
    MPI_Comm_size(MPI_COMM_WORLD, &nproc);
    fprintf(stdout, "[%s:%s:%d] done with init \n",
	    hostname, __FUNCTION__, __LINE__);
    fflush(stdout);
    MPI_Finalize();
    return 0;
}
