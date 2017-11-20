/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2015 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include <mpi.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static void handle_error(int errcode, const char *str)
{
	char msg[MPI_MAX_ERROR_STRING];
	int resultlen;
	MPI_Error_string(errcode, msg, &resultlen);
	fprintf(stderr, "%s: %s\n", str, msg);
	MPI_Abort(MPI_COMM_WORLD, 1);
}

#define CHECK(fn) {int errcode; errcode = (fn); if (errcode != MPI_SUCCESS) handle_error(errcode, #fn); }

static int hint_check(MPI_Info info_used, const char * key, const char *expected) {
    char value[MPI_MAX_INFO_VAL+1];
    int flag;

    CHECK(MPI_Info_get(info_used, key, MPI_MAX_INFO_VAL, value, &flag));
    if (strcmp(expected, value) ){
	    fprintf(stderr, "expected value \"%s\" for key \"%s\" got \"%s\"\n",
		    expected, key, value);
	    return 1;
    }
    return 0;
}

int main(int argc, char ** argv)
{
    setenv("ROMIO_HINTS", argv[1], 1);
    MPI_File fh;
    MPI_Info info_used, info_mine;
    int nr_errors=0;

    MPI_Init(&argc, &argv);
    MPI_Info_create(&info_mine);
    MPI_Info_set(info_mine, "romio_cb_read", "disable");
    CHECK(MPI_File_open(MPI_COMM_WORLD, argv[1], MPI_MODE_RDONLY, info_mine, &fh));
    CHECK(MPI_File_get_info(fh, &info_used));

    nr_errors += hint_check(info_used, "ind_rd_buffer_size", "49");
    nr_errors += hint_check(info_used, "romio_no_indep_rw", "true");

    if (nr_errors == 0) printf(" No Errors\n");

    CHECK(MPI_Info_free(&info_mine));
    CHECK(MPI_Info_free(&info_used));
    CHECK(MPI_File_close(&fh));
    MPI_Finalize();
    return nr_errors;
}
