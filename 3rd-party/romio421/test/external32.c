/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include <stdlib.h>
#include <stdio.h>
#include "mpi.h"
#include <assert.h>

#define TEST_LE 0x1
#define TEST_BE 0x2
#define TEST_FILENAME "test.datarep"

#define CHECK(fn) {int errcode; errcode = (fn); if (errcode != MPI_SUCCESS) handle_error(errcode, NULL); }

static void handle_error(int errcode, char *str)
{
    char msg[MPI_MAX_ERROR_STRING];
    int resultlen;
    MPI_Error_string(errcode, msg, &resultlen);
    fprintf(stderr, "%s: (%d) %s\n", str, errcode, msg);
    MPI_Abort(MPI_COMM_WORLD, 1);
}


#define MAX_LEN 4
static void is_little_or_big_endian(FILE * fileh_std, const char *datarep, char *c_le, int len)
{
    int i, j, is_le = 1, is_be = 1;
    char c[MAX_LEN];

    assert(len <= MAX_LEN);

    for (j = 0; j < len; j++) {
        if (feof(fileh_std)) {
            printf("unexpected eof, aborted\n");
            return;
        }
        fscanf(fileh_std, "%c", &c[j]);
    }

    for (i = 0; i < len; i++) {
        is_le = is_le && (c[i] == c_le[i]);
        is_be = is_be && (c[i] == c_le[len - 1 - i]);
    }
    printf("%s datarep is ", datarep);
    switch ((is_le ? TEST_LE : 0x0) | (is_be ? TEST_BE : 0x0)) {
        case TEST_LE:
            printf("LITTLE ENDIAN\n");
            break;
        case TEST_BE:
            printf("BIG ENDIAN\n");
            break;
        case TEST_LE | TEST_BE:
            printf("LITTLE or BIG ENDIAN\n");
            break;
        default:
            printf("unknown\n");
            break;
    }
}

/* This test checks if datareps given are little- or big-endian */
int main(int argc, char *argv[])
{
    int sample_i = 123456789, i, j;
    char sample_i_le[4] = { 0x15, 0xcd, 0x5b, 0x07 };
    const char *datarep[3] = { "native", "external32", "internal" };
    MPI_File fileh;
    int rank;
    FILE *fileh_std;

    if (sizeof(int) != 4) {
        printf("non-supported sizeof(int)=%ld\n", sizeof(int));
        return (-1);
    }

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    /* For each datarep */
    for (i = 0; i < 3; i++) {

        /* Open file */
        CHECK(MPI_File_open(MPI_COMM_WORLD, TEST_FILENAME,
                            MPI_MODE_RDWR | MPI_MODE_CREATE, MPI_INFO_NULL, &fileh));

        /* Set view */
        CHECK(MPI_File_set_view(fileh, 0, MPI_INT, MPI_INT, datarep[i], MPI_INFO_NULL));

        /* Write into file */
        CHECK(MPI_File_write_at(fileh, (MPI_Offset) rank, (void *) &sample_i, 1,
                                MPI_INT, MPI_STATUS_IGNORE));

        /* Close file */
        CHECK(MPI_File_close(&fileh));

        /* Check if your datarep is little or big endian */
        MPI_Barrier(MPI_COMM_WORLD);
        if (rank == 0) {
            fileh_std = fopen(TEST_FILENAME, "r");
            is_little_or_big_endian(fileh_std, datarep[i], sample_i_le, 4);
            fclose(fileh_std);
        }

        /* Delete file */
        if (rank == 0) {
            CHECK(MPI_File_delete(TEST_FILENAME, MPI_INFO_NULL));
        }
    }

    MPI_Finalize();

    return 0;
}
