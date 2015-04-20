/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*  
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */
#include "mpi.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define COUNT (200)
#undef TIMING

void handle_error(int errcode, const char *str);

void handle_error(int errcode, const char *str) 
{
	char msg[MPI_MAX_ERROR_STRING];
	int resultlen;
	MPI_Error_string(errcode, msg, &resultlen);
	fprintf(stderr, "%s: %s\n", str, msg);
	MPI_Abort(MPI_COMM_WORLD, 1);
}

/* tests shared file pointer functions */

int main(int argc, char **argv)
{
    int *buf, i, rank, nprocs, len, sum;
    int global_sum;
    int errs=0, toterrs, errcode;
    char *filename;
    MPI_File fh;
    MPI_Status status;

    MPI_Init(&argc,&argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    double wr_stime, wr_etime, wr_time, wr_sumtime;
    double rd_stime, rd_etime, rd_time, rd_sumtime;

/* process 0 takes the file name as a command-line argument and 
   broadcasts it to other processes */
    if (!rank) {
	i = 1;
	while ((i < argc) && strcmp("-fname", *argv)) {
	    i++;
	    argv++;
	}
	if (i >= argc) {
	    fprintf(stderr, "\n*#  Usage: shared_fp -fname filename\n\n");
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

    errcode = MPI_File_open(MPI_COMM_WORLD, filename, 
		    MPI_MODE_CREATE | MPI_MODE_RDWR, MPI_INFO_NULL, &fh);
    if (errcode != MPI_SUCCESS) {
	    handle_error(errcode, "MPI_File_open");
    }

    wr_stime = MPI_Wtime();

    errcode = MPI_File_write_ordered(fh, buf, COUNT, MPI_INT, &status);
    if (errcode != MPI_SUCCESS) {
	    handle_error(errcode, "MPI_File_write_shared");
    }
    wr_etime = MPI_Wtime();

    for (i=0; i<COUNT; i++) buf[i] = 0;

    MPI_Barrier(MPI_COMM_WORLD);

    rd_stime = MPI_Wtime();
    errcode = MPI_File_seek_shared(fh, 0, MPI_SEEK_SET);
    if (errcode != MPI_SUCCESS) {
	    handle_error(errcode, "MPI_File_seek_shared");
    }

    errcode = MPI_File_read_ordered(fh, buf, COUNT, MPI_INT, &status);
    if (errcode != MPI_SUCCESS) {
	    handle_error(errcode, "MPI_File_read_shared");
    }

    rd_etime = MPI_Wtime();
    MPI_File_close(&fh);

    sum = 0;
    for (i=0; i<COUNT; i++) sum += buf[i];

    MPI_Allreduce(&sum, &global_sum, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

    wr_time = wr_etime - wr_stime;
    rd_time = rd_etime - rd_stime;

    MPI_Allreduce(&wr_time, &wr_sumtime, 1, 
        MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);
    MPI_Allreduce(&rd_time, &rd_sumtime, 1, 
        MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);

    if (global_sum != (((COUNT*nprocs - 1)*(COUNT*nprocs))/2)) {
	errs++;
	fprintf(stderr, "Error: sum %d, global_sum %d, %d\n", 
		sum, global_sum,(((COUNT*nprocs - 1)*(COUNT*nprocs))/2));
    }
    
    free(buf);
    free(filename);

    MPI_Allreduce( &errs, &toterrs, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD );
    if (rank == 0) {
	if( toterrs > 0) {
	    fprintf( stderr, "Found %d errors\n", toterrs );
	}
	else {
	    fprintf( stdout, " No Errors\n" );
#ifdef TIMING
            fprintf( stderr, "nprocs: %d bytes: %d write: %f read %f\n", 
                 nprocs, COUNT*sizeof(int), wr_sumtime, rd_sumtime);
#endif
	}
    }

    MPI_Finalize();
    return 0; 
}
