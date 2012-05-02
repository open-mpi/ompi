#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
    long count = 0;
    int i = 8, j;
    int self;
    int do_barrier = 0;
int k;
double pi;
    
    if (getenv("DO_BARRIER")) {
        do_barrier = 1;
    }
    
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &self);
    while (1) {
#if 0
for (k=0; k < (7-self)*1000; k++) {
pi = 3.14159 * 18.0 / 35.3;
}
#endif
        MPI_Reduce(&i, &j, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);
        if (do_barrier) {
            MPI_Barrier(MPI_COMM_WORLD);
        }
        if (0 == (++count % 10000)) {
            fprintf(stderr, "%d still going at %ld\n", self, count);
        }
    }
    MPI_Finalize();

    return 0;
}
