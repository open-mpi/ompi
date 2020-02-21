#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>
#include <mpi.h>

#define ORTE_IOF_BASE_MSG_MAX   2048

int main(int argc, char *argv[])
{
    int i, rank, size, next, prev, tag = 201;
    int pos, msgsize, nbytes;
    bool done;
    char *msg;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    fprintf(stderr, "Rank %d has cleared MPI_Init\n", rank);

    next = (rank + 1) % size;
    prev = (rank + size - 1) % size;
    msg = malloc(ORTE_IOF_BASE_MSG_MAX);
    pos = 0;
    nbytes = 0;

    if (0 == rank) {
        while (0 != (msgsize = read(0, msg, ORTE_IOF_BASE_MSG_MAX))) {
            fprintf(stderr, "Rank %d: sending blob %d\n", rank, pos);
            if (msgsize > 0) {
                MPI_Bcast(msg, ORTE_IOF_BASE_MSG_MAX, MPI_BYTE, 0, MPI_COMM_WORLD);
            }
            ++pos;
            nbytes += msgsize;
        }
        fprintf(stderr, "Rank %d: sending termination blob %d\n", rank, pos);
        memset(msg, 0, ORTE_IOF_BASE_MSG_MAX);
        MPI_Bcast(msg, ORTE_IOF_BASE_MSG_MAX, MPI_BYTE, 0, MPI_COMM_WORLD);
        MPI_Barrier(MPI_COMM_WORLD);
    } else {
        while (1) {
            MPI_Bcast(msg, ORTE_IOF_BASE_MSG_MAX, MPI_BYTE, 0, MPI_COMM_WORLD);
            fprintf(stderr, "Rank %d: recvd blob %d\n", rank, pos);
            ++pos;
            done = true;
            for (i=0; i < ORTE_IOF_BASE_MSG_MAX; i++) {
                if (0 != msg[i]) {
                    done = false;
                    break;
                }
            }
            if (done) {
                break;
            }
        }
        fprintf(stderr, "Rank %d: recv done\n", rank);
        MPI_Barrier(MPI_COMM_WORLD);
    }

    fprintf(stderr, "Rank %d has completed bcast\n", rank);
    MPI_Finalize();
    return 0;
}
