/*
 * Copyright (c) 2019-2020 IBM Corporation. All rights reserved.
 * $COPYRIGHT$
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

/* intended for use with pingpong or similar programs
 * look at time between a send and the next recv
 *
 * bin i goes up to (2^(i/8 + 2)) * (8 + i%8)/8 ns
 * so every 80 bins represent 1024x, so by 500 bins the times are huge
 */

#define NBINS 500

long long ns[NBINS];
int bucket[NBINS];
double tprev = -1;
double tnow;

void ptable();

int
MPI_Init(int *argc, char **argv[]) {
    int rv, i;
    rv = PMPI_Init(argc, argv);
    for (i=0; i<NBINS; ++i) {
        ns[i] = (1<<(i/8 + 2)) * (8 + i%8) / 8;
        bucket[i] = 0;
    }
    return(rv);
}

int
MPI_Finalize() {
    int i, myrank, nranks;

    MPI_Comm_rank(MPI_COMM_WORLD, &myrank);
    MPI_Comm_size(MPI_COMM_WORLD, &nranks);
    for (i=0; i<nranks; ++i) {
        MPI_Barrier(MPI_COMM_WORLD);
        if (myrank == i) {
            printf("========[R%d]======================================\n",
                myrank);
            ptable();
            fflush(stdout);
        }
        sleep(1);
        MPI_Barrier(MPI_COMM_WORLD);
    }

    return PMPI_Finalize();
}

int
MPI_Send(const void *buf, int count, MPI_Datatype dt, int peer, int tag,
    MPI_Comm comm)
{
    tprev = MPI_Wtime();
    return PMPI_Send(buf, count, dt, peer, tag, comm);
}

int
MPI_Recv(void *buf, int count, MPI_Datatype dt, int peer, int tag,
    MPI_Comm comm, MPI_Status *status)
{
    int rv, i;
    double t;
    rv = PMPI_Recv(buf, count, dt, peer, tag, comm, status);
    tnow = MPI_Wtime();
    t = (tnow - tprev) * 1000000000;
    i = 0;
    while (i<NBINS && t>ns[i]) { ++i; }
    ++bucket[i];
    return rv;
}

void
ptable() {
    int i, startidx, endidx, xsize, maxbucket, nx, j;

    startidx = 0; /* incr to first non-0 bucket */
    endidx = 0; /* becomes last non-0 bucket */
    maxbucket = 0;

    for (i=0; i<NBINS; ++i) {
        if (startidx == 0 && bucket[i]) { startidx = i; }
        if (bucket[i]) { endidx = i; }
        if (bucket[i] > maxbucket) { maxbucket = bucket[i]; }
    }
    xsize = maxbucket / 68;
    if (xsize == 0) { xsize = 1; }

    for (i=startidx; i<endidx; ++i) {
        nx = bucket[i] / xsize + 1;

        if (ns[i] < 10000) { printf("%4dn ", ns[i]); }
        else if (ns[i]/1024 < 10000) { printf("%4du ", ns[i]/1024); }
        else if (ns[i]/1024/1024 < 10000) {
            printf("%4dm ", ns[i]/1024/1024);
        }
        else {
            printf("%4d  ", ns[i]/1024/1024/1024);
        }
        for (j=0; j<nx; ++j) {
            printf("x");
        }
        printf("\n");
    }
}
