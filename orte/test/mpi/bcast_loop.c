#include <mpi.h>
#include <stdio.h>
	
int main(int argc, char* argv[])
{
    int myid, nprocs, tag;
    int i, m, nt;
    MPI_Status status;
    double workarray1[561], workarray2[561];
    const int numm = 50000;
    const int numt = 142;

    MPI_Init(NULL, NULL);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &myid);

    for (m = 0; m < numm; ++m) {
        if (0 == (m % 1000)) {
            printf("rank %d, m = %d\n", myid, m);
        }
        for (nt = 0; nt <= numt; ++nt) {
            if (0 == myid) {
                for (i = 0; i < 561; ++i) {
                    workarray1[i] = numm * numt * i;
                    workarray2[i] = numm * numt * (i + 1);
                }
            }
            MPI_Bcast(workarray1, 561, MPI_DOUBLE, 0, MPI_COMM_WORLD);
            MPI_Bcast(workarray2, 561, MPI_DOUBLE, 0, MPI_COMM_WORLD);
        }
    }
    MPI_Finalize();

    return 0;
}

