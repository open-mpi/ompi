/* -*- Mode: C; c-basic-offset:4 ; -*- */
#include "mpi.h"
#include "mpio.h"  /* not necessary with MPICH 1.1.1 or HPMPI 1.4 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* tests if error message is printed correctly */

int main(int argc, char **argv)
{
    int i, rank, len, err;
    char *filename, *tmp;
    MPI_File fh;
    char string[MPI_MAX_ERROR_STRING];

    MPI_Init(&argc,&argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if (!rank) {
	fprintf(stderr, "Tests if errors are reported correctly...\n");
	fprintf(stderr, "Should say \"Invalid displacement argument\"\n\n");
    }

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
    
    /* each process opens a separate file called filename.'myrank' */
    tmp = (char *) malloc(len+10);
    strcpy(tmp, filename);
    sprintf(filename, "%s.%d", tmp, rank);

    err = MPI_File_open(MPI_COMM_SELF, filename, MPI_MODE_CREATE+MPI_MODE_RDWR,
		        MPI_INFO_NULL, &fh);
    err = MPI_File_set_view(fh, -1, MPI_BYTE, MPI_BYTE, "native", 
                            MPI_INFO_NULL);
    /* disp is deliberately passed as -1 */

    if (err != MPI_SUCCESS) {
	MPI_Error_string(err, string, &len);
	if (!rank) fprintf(stderr, "%s\n", string);
    }

    MPI_File_close(&fh);

    free(filename);
    free(tmp);
    MPI_Finalize();
    return 0; 
}
