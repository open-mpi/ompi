/* -*- Mode: C; c-basic-offset:4 ; -*- */
#include "mpi.h"
#include "mpio.h"  /* not necessary with MPICH 1.1.1 or HPMPI 1.4 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define SIZE (65536)

/* Checks if the status objects is filled correctly by I/O functions */ 

int main(int argc, char **argv)
{
    int *buf, i, rank, nints, len, count, elements;
    char *filename, *tmp;
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
    
    buf = (int *) malloc(SIZE);
    nints = SIZE/sizeof(int);

    /* each process opens a separate file called filename.'myrank' */
    tmp = (char *) malloc(len+10);
    strcpy(tmp, filename);
    sprintf(filename, "%s.%d", tmp, rank);

    MPI_File_open(MPI_COMM_SELF, filename, MPI_MODE_CREATE | MPI_MODE_RDWR,
		   MPI_INFO_NULL, &fh);
    MPI_File_write(fh, buf, nints, MPI_INT, &status);

    MPI_Get_count(&status, MPI_INT, &count);
    MPI_Get_elements(&status, MPI_INT, &elements);
    if (!rank) {
	printf("count = %d, should be %d\n", count, nints);
	printf("elements = %d, should be %d\n", elements, nints);
    }

    MPI_File_close(&fh);

    free(buf);
    free(filename);
    free(tmp);

    MPI_Finalize();
    return 0; 
}
