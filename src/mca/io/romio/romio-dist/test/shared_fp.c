/* -*- Mode: C; c-basic-offset:4 ; -*- */
#include "mpi.h"
#include "mpio.h"  /* not necessary with MPICH 1.1.1 or HPMPI 1.4 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define COUNT 1024

/* tests shared file pointer functions */

int main(int argc, char **argv)
{
    int *buf, i, rank, nprocs, len, sum, global_sum;
    char *filename;
    MPI_File fh;
    MPI_Status status;

    MPI_Init(&argc,&argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

/* process 0 takes the file name as a command-line argument and 
   broadcasts it to other processes */
    if (!rank) {
	i = 1;
	while ((i < argc) && strcmp("-fname", *argv)) {
	    i++;
	    argv++;
	}
	if (i >= argc) {
	    fprintf(stderr, "\n*#  Usage: simple -fname filename\n\n");
	    MPI_Abort(MPI_COMM_WORLD, 1);
	}
	argv++;
	len = strlen(*argv);
	filename = (char *) malloc(len+10);
	strcpy(filename, *argv);
	MPI_Bcast(&len, 1, MPI_INT, 0, MPI_COMM_WORLD);
	MPI_Bcast(filename, len+10, MPI_CHAR, 0, MPI_COMM_WORLD);
    }
    else {
	MPI_Bcast(&len, 1, MPI_INT, 0, MPI_COMM_WORLD);
	filename = (char *) malloc(len+10);
	MPI_Bcast(filename, len+10, MPI_CHAR, 0, MPI_COMM_WORLD);
    }
    
    buf = (int *) malloc(COUNT * sizeof(int));

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);

    for (i=0; i<COUNT; i++) buf[i] = COUNT*rank + i;

    MPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_CREATE | MPI_MODE_RDWR,
		   MPI_INFO_NULL, &fh);

    MPI_File_write_shared(fh, buf, COUNT, MPI_INT, &status);

    for (i=0; i<COUNT; i++) buf[i] = 0;

    MPI_Barrier(MPI_COMM_WORLD);

    MPI_File_seek_shared(fh, 0, MPI_SEEK_SET);

    MPI_File_read_shared(fh, buf, COUNT, MPI_INT, &status);

    MPI_File_close(&fh);

    sum = 0;
    for (i=0; i<COUNT; i++) sum += buf[i];

    MPI_Allreduce(&sum, &global_sum, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

    if (global_sum != (((COUNT*nprocs - 1)*(COUNT*nprocs))/2))
	fprintf(stderr, "Error: sum %d, global_sum %d, %d\n", sum, global_sum,(((COUNT*nprocs - 1)*(COUNT*nprocs))/2));
    
    free(buf);
    free(filename);

    if (!rank) fprintf(stderr, "Done\n");

    MPI_Finalize();
    return 0; 
}
