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
 *
 * Then check a value that grew after its handle was allocated: write a
 * value longer than the count with MPI_T_cvar_write(), and check that
 * reading it into a buffer of exactly the count truncates it to count - 1
 * characters plus a null, without writing past the buffer.
 */

#include "mpi_t_lifecycle.h"

#include <string.h>

/* A string control variable that nothing uses unless MPI is initialized
   with mpi_show_mca_params set, so a long, bogus value is harmless */
#define CVAR_NAME "mpi_show_mca_params_file"
#define VALUE_LEN 10000
#define GUARD_LEN 64
#define GUARD_BYTE 0xa5

/* A settable string control variable that is only parsed while MPI is
   being initialized, so changing it for a moment is harmless */
#define SETTABLE_CVAR_NAME "opal_signal"
/* How much longer than the count the grown value is: less than the
   guard, so that a read that does not truncate lands in the guard */
#define GROWN_EXTRA (GUARD_LEN / 2)

/* Check that a guard region of GUARD_LEN bytes is intact */
static int guard_intact(const unsigned char *guard)
{
    for (int i = 0; i < GUARD_LEN; ++i) {
        if (GUARD_BYTE != guard[i]) {
            return 0;
        }
    }
    return 1;
}

/* Returns the number of failures */
static int check_truncation(void)
{
    int index, count, rc, failures = 0;
    MPI_T_cvar_handle handle;
    char *orig = NULL, *grown = NULL;
    unsigned char *buf = NULL;

    if (MPI_SUCCESS != MPI_T_cvar_get_index(SETTABLE_CVAR_NAME, &index)) {
        printf("SKIP: there is no control variable named %s; not testing truncation\n",
               SETTABLE_CVAR_NAME);
        return 0;
    }
    STEP(MPI_T_cvar_handle_alloc(index, NULL, &handle, &count),
         "cvar_handle_alloc (" SETTABLE_CVAR_NAME ")");

    orig = malloc((size_t) count);
    grown = malloc((size_t) count + GROWN_EXTRA + 1);
    buf = malloc((size_t) count + GUARD_LEN);
    if (NULL == orig || NULL == grown || NULL == buf) {
        printf("SKIP: could not allocate memory; not testing truncation\n");
        goto out;
    }
    STEP(MPI_T_cvar_read(handle, orig), "cvar_read (original value)");

    memset(grown, 'y', (size_t) count + GROWN_EXTRA);
    grown[count + GROWN_EXTRA] = '\0';
    printf("step: cvar_write (%d characters)\n", count + GROWN_EXTRA);
    fflush(stdout);
    rc = MPI_T_cvar_write(handle, grown);
    if (MPI_SUCCESS != rc) {
        printf("FAIL: MPI_T_cvar_write of %s returned %d\n", SETTABLE_CVAR_NAME, rc);
        ++failures;
        goto out;
    }

    /* A buffer of exactly the count, followed by a guard */
    memset(buf, GUARD_BYTE, (size_t) count + GUARD_LEN);
    printf("step: cvar_read (grown value)\n");
    fflush(stdout);
    rc = MPI_T_cvar_read(handle, buf);
    if (MPI_SUCCESS != rc) {
        printf("FAIL: MPI_T_cvar_read of the grown value returned %d\n", rc);
        ++failures;
    } else {
        if (!guard_intact(buf + count)) {
            printf("FAIL: MPI_T_cvar_read wrote a grown value past the end of a buffer of %d "
                   "characters\n", count);
            ++failures;
        }
        if ('\0' != buf[count - 1] || 0 != memcmp(buf, grown, (size_t) count - 1)) {
            printf("FAIL: the grown value was not truncated to %d characters plus a null\n",
                   count - 1);
            ++failures;
        }
    }

    /* Put the original value back */
    rc = MPI_T_cvar_write(handle, orig);
    if (MPI_SUCCESS != rc) {
        printf("FAIL: MPI_T_cvar_write of the original value returned %d\n", rc);
        ++failures;
    }

out:
    free(buf);
    free(grown);
    free(orig);
    STEP(MPI_T_cvar_handle_free(&handle), "cvar_handle_free (" SETTABLE_CVAR_NAME ")");
    return failures;
}

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

    if (!guard_intact(buf + count)) {
        printf("FAIL: MPI_T_cvar_read wrote past the end of a buffer of %d characters\n", count);
        ++failures;
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

    failures += check_truncation();

    STEP(MPI_T_finalize(), "T_finalize");

    if (0 != failures) {
        return 1;
    }
    printf("SUCCESS: read long and grown string control variables\n");
    return 0;
}
