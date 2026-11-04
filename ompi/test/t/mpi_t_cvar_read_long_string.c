/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 *
 * Regression test: reading a string control variable whose value is
 * longer than 2048 characters.
 *
 * MPI_T_cvar_handle_alloc() reported a count of 2048 for every string
 * control variable, and MPI_T_cvar_read() copied the whole value, so a
 * tool that allocated a buffer of that count -- as the MPI standard
 * tells it to -- overflowed it when the value was longer.  For example,
 * "make check" sets the component path to every build directory that
 * holds components, several kilobytes.
 *
 * Set a string control variable to a long value through the environment,
 * then check that the count covers it, and that reading it into a buffer
 * of exactly that count returns the whole value without writing past the
 * buffer.
 */

#include "mpi_t_lifecycle.h"

#include <string.h>

/* A string control variable that nothing uses unless MPI is initialized
   with mpi_show_mca_params set, so a long, bogus value is harmless */
#define CVAR_NAME "mpi_show_mca_params_file"
#define VALUE_LEN 10000
#define GUARD_LEN 64
#define GUARD_BYTE 0xa5

int main(void)
{
    static char value[VALUE_LEN + 1];
    int provided, index, count, failures = 0;
    MPI_T_cvar_handle handle;
    unsigned char *buf;

    memset(value, 'x', VALUE_LEN);
    value[VALUE_LEN] = '\0';
    setenv("OMPI_MCA_" CVAR_NAME, value, 1);

    STEP_OR_SKIP(MPI_T_init_thread(MPI_THREAD_SINGLE, &provided), "T_init");
    if (MPI_SUCCESS != MPI_T_cvar_get_index(CVAR_NAME, &index)) {
        printf("SKIP: there is no control variable named %s\n", CVAR_NAME);
        MPI_T_finalize();
        return 77;
    }
    STEP(MPI_T_cvar_handle_alloc(index, NULL, &handle, &count), "cvar_handle_alloc");

    if (count < VALUE_LEN + 1) {
        printf("FAIL: the count is %d, but the value needs %d characters\n", count,
               VALUE_LEN + 1);
        ++failures;
    }

    /* A buffer of exactly the reported count, followed by a guard */
    buf = malloc((size_t) count + GUARD_LEN);
    if (NULL == buf) {
        printf("SKIP: could not allocate %d bytes\n", count + GUARD_LEN);
        return 77;
    }
    memset(buf, GUARD_BYTE, (size_t) count + GUARD_LEN);

    STEP(MPI_T_cvar_read(handle, buf), "cvar_read");

    for (int i = 0; i < GUARD_LEN; ++i) {
        if (GUARD_BYTE != buf[count + i]) {
            printf("FAIL: MPI_T_cvar_read wrote past the end of a buffer of %d characters\n",
                   count);
            ++failures;
            break;
        }
    }
    if (NULL == memchr(buf, '\0', (size_t) count)) {
        printf("FAIL: the value read is not null-terminated within %d characters\n", count);
        ++failures;
    } else if (0 != strcmp((const char *) buf, value)) {
        printf("FAIL: the value read (%zu characters) is not the value set (%d characters)\n",
               strlen((const char *) buf), VALUE_LEN);
        ++failures;
    }

    free(buf);
    STEP(MPI_T_cvar_handle_free(&handle), "cvar_handle_free");
    STEP(MPI_T_finalize(), "T_finalize");

    if (0 != failures) {
        return 1;
    }
    printf("SUCCESS: read a %d-character string control variable\n", VALUE_LEN);
    return 0;
}
