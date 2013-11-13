/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*  
 *  (C) 2007 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

/* Look for regressions in aggregator code.  A more simple access pattern than
 * aggregation1 */

#include <mpi.h>

#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#include <sys/types.h>
#include <unistd.h>

#include <string.h>

#define BUFSIZE 512

static void handle_error(int errcode, char *str)
{
        char msg[MPI_MAX_ERROR_STRING];
        int resultlen;
        MPI_Error_string(errcode, msg, &resultlen);
        fprintf(stderr, "%s: %s\n", str, msg);
        MPI_Abort(MPI_COMM_WORLD, 1);
}

int main(int argc, char ** argv) 
{
    MPI_Info info = MPI_INFO_NULL;
    MPI_File fh;
    MPI_Offset off=0;
    MPI_Status status;
    int errcode;
    int i, rank, errs=0, toterrs, buffer[BUFSIZE], buf2[BUFSIZE];

    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    MPI_Info_create(&info);
    MPI_Info_set(info, "romio_cb_write", "enable");
    MPI_Info_set(info, "cb_nodes", "1");

    for (i=0; i<BUFSIZE; i++) {
        buffer[i] = 10000+rank;
    }
    off = rank*sizeof(buffer);

    errcode = MPI_File_open(MPI_COMM_WORLD, argv[1], 
		MPI_MODE_WRONLY|MPI_MODE_CREATE, info, &fh);
    if (errcode != MPI_SUCCESS) handle_error(errcode, "MPI_File_open");
    errcode = MPI_File_write_at_all(fh, off, buffer, BUFSIZE, 
		MPI_INT,  &status);
    if (errcode != MPI_SUCCESS) handle_error(errcode, "MPI_File_write_at_all");
    errcode = MPI_File_close(&fh);
    if (errcode != MPI_SUCCESS) handle_error(errcode, "MPI_File_close");

    errcode = MPI_File_open(MPI_COMM_WORLD, argv[1], 
		MPI_MODE_RDONLY, info, &fh);
    if (errcode != MPI_SUCCESS) handle_error(errcode, "MPI_File_open");
    errcode = MPI_File_read_at_all(fh, off, buf2, BUFSIZE, 
		MPI_INT,  &status);
    if (errcode != MPI_SUCCESS) handle_error(errcode, "MPI_File_read_at_all");
    errcode = MPI_File_close(&fh);
    if (errcode != MPI_SUCCESS) handle_error(errcode, "MPI_File_close");

    for (i=0; i<BUFSIZE; i++) {
        if (buf2[i] != 10000+rank)
	    errs++;
    }
    MPI_Allreduce( &errs, &toterrs, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD );
    if (rank == 0) {
	if( toterrs > 0) {
	    fprintf( stderr, "Found %d errors\n", toterrs );
	}
	else {
	    fprintf( stdout, " No Errors\n" );
	}
    }
    MPI_Info_free(&info);
    MPI_Finalize();

    return 0;
}
