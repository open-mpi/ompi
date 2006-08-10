/*
 * Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
 *                         Use is subject to license terms.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <netdb.h>
#include <unistd.h>

#include <mpi.h>

#define ITERATIONS 1000000

static void allocate_comms(void);
static void deallocate_comms(void);
static MPI_Comm communicator_a, communicator_b, communicator_c;

int
main(int argc, char *argv[])
{
    int		rank;                         /* COMM_WORLD rank of process */
    int		np;	                      /* number of processes in job */
    int         i;
    int         unslept;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &np);

    for (i=0; i < ITERATIONS; i++) {
	/* allocate our communicators */
	allocate_comms();

	/* simulate doing work */
	unslept = sleep(5);

	/* deallocate communicators forgetting one of the communicators */
	deallocate_comms();
    }
    MPI_Finalize();
    return 0;
}

void allocate_comms(void) {
    MPI_Comm_dup(MPI_COMM_WORLD, &communicator_a);
    MPI_Comm_dup(MPI_COMM_WORLD, &communicator_b);
    MPI_Comm_dup(MPI_COMM_WORLD, &communicator_c);
}

void deallocate_comms(void) {
    MPI_Comm_free(&communicator_a);
    MPI_Comm_free(&communicator_c);
}
