#include <stdlib.h>
#include <stdio.h>
#include <mpi.h>

#define SIZE 20
#define POS 10
#define INITIAL_VALUE 10

int main(int argc, char *argv[])
{
    int i, rank, size, next, prev, tag = 201;
    int array_size = SIZE;
    int pos = POS;
    int *send_array;
    int *recv_array;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    fprintf(stderr, "Rank %d has cleared MPI_Init\n", rank);

    next = (rank + 1) % size;
    prev = (rank + size - 1) % size;
    send_array = malloc(sizeof(int) * SIZE);
    recv_array = malloc(sizeof(int) * SIZE);

    for (i = 0; i < array_size; ++i) {
        send_array[i] = 17;
        recv_array[i] = -1;
    }

    if (0 == rank) {
        send_array[pos] = INITIAL_VALUE;
        MPI_Send(send_array, array_size, MPI_INT, next, tag,
                 MPI_COMM_WORLD);
    }

    while (1) {
        recv_array[pos] = -1;
        MPI_Recv(recv_array, array_size, MPI_INT, prev, tag,
                 MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        send_array[pos] = recv_array[pos];
        if (rank == 0) {
            --send_array[pos];
        }
        MPI_Send(send_array, array_size, MPI_INT, next, tag, MPI_COMM_WORLD);
        if (0 == send_array[pos]) {
            break;
        }
    }

    if (rank == 0) {
        MPI_Recv(recv_array, array_size, MPI_INT, prev, tag,
                 MPI_COMM_WORLD, MPI_STATUS_IGNORE);
    }

    fprintf(stderr, "Rank %d has completed ring\n", rank);
    MPI_Barrier(MPI_COMM_WORLD);
    fprintf(stderr, "Rank %d has completed MPI_Barrier\n", rank);
    MPI_Finalize();
    return 0;
}
