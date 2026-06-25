/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Shared helpers for the single-process OMPIO MPI_File info tests.
 *
 * These tests are singletons: each calls MPI_Init and performs all of
 * its file I/O on MPI_COMM_SELF, so they need no launcher and can run as
 * part of 'make check'.  They exercise the OMPIO MPI_Info hint reporting
 * required by MPI-5.0 section 15.2.8 and added for Open MPI issue #13367
 * ("MPI_File_get_info fails to return hints as required by MPI
 * standard"): MPI_File_get_info must return every supported hint (its
 * implementation default, any accepted user value, and any value set by
 * the implementation), and must not return unknown or ignored user
 * hints.
 *
 * The helpers below are 'static inline' so that an unused helper in any
 * one test does not trigger -Wunused-function.
 */

#ifndef OMPIO_FILE_INFO_COMMON_H
#define OMPIO_FILE_INFO_COMMON_H

#include "mpi.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* Running tally of assertions for a single test program. */
typedef struct {
    int checks;     /* total assertions made */
    int failures;   /* assertions that failed */
} ofi_result_t;

/*
 * Look up a key in an info object.  Returns 1 and copies the value into
 * 'value' (a buffer of at least MPI_MAX_INFO_VAL + 1 bytes) if present,
 * otherwise returns 0.
 */
static inline int ofi_info_get(MPI_Info info, const char *key, char *value)
{
    int flag = 0;
    int buflen = MPI_MAX_INFO_VAL;

    value[0] = '\0';
    MPI_Info_get_string(info, key, &buflen, value, &flag);
    return flag;
}

/* Check that an MPI call returned MPI_SUCCESS. */
static inline void ofi_check_rc(ofi_result_t *res, int rc, const char *call)
{
    char errstr[MPI_MAX_ERROR_STRING];
    int len = 0;

    ++res->checks;
    if (MPI_SUCCESS != rc) {
        ++res->failures;
        MPI_Error_string(rc, errstr, &len);
        printf("FAIL: %s returned MPI error %d (%.*s)\n", call, rc, len, errstr);
    }
}

/* Assert that 'key' is present (with any value). */
static inline void ofi_expect_key(ofi_result_t *res, MPI_Info info,
                                  const char *key)
{
    char value[MPI_MAX_INFO_VAL + 1];

    ++res->checks;
    if (!ofi_info_get(info, key, value)) {
        ++res->failures;
        printf("FAIL: expected hint '%s' to be present\n", key);
    } else {
        printf("PASS: hint '%s' present (= '%s')\n", key, value);
    }
}

/* Assert that 'key' is present and equal to 'expected'. */
static inline void ofi_expect_value(ofi_result_t *res, MPI_Info info,
                                    const char *key, const char *expected)
{
    char value[MPI_MAX_INFO_VAL + 1];

    ++res->checks;
    if (!ofi_info_get(info, key, value)) {
        ++res->failures;
        printf("FAIL: expected hint '%s' = '%s', but it was absent\n",
               key, expected);
    } else if (0 != strcmp(value, expected)) {
        ++res->failures;
        printf("FAIL: expected hint '%s' = '%s', got '%s'\n",
               key, expected, value);
    } else {
        printf("PASS: hint '%s' = '%s'\n", key, value);
    }
}

/* Assert that 'key' is not reported at all. */
static inline void ofi_expect_no_key(ofi_result_t *res, MPI_Info info,
                                     const char *key)
{
    char value[MPI_MAX_INFO_VAL + 1];

    ++res->checks;
    if (ofi_info_get(info, key, value)) {
        ++res->failures;
        printf("FAIL: hint '%s' should not be reported, but got '%s'\n",
               key, value);
    } else {
        printf("PASS: hint '%s' correctly not reported\n", key);
    }
}

/*
 * Assert that 'key' is either absent or, if present, not equal to the
 * 'rejected' value.  Used for hints (such as cb_nodes) whose internal
 * sentinel value must never be reported to the application.
 */
static inline void ofi_expect_absent_or_not_value(ofi_result_t *res,
                                                  MPI_Info info,
                                                  const char *key,
                                                  const char *rejected)
{
    char value[MPI_MAX_INFO_VAL + 1];

    ++res->checks;
    if (ofi_info_get(info, key, value) && 0 == strcmp(value, rejected)) {
        ++res->failures;
        printf("FAIL: hint '%s' must never be reported as '%s'\n",
               key, rejected);
    } else {
        printf("PASS: hint '%s' not reported as '%s'\n", key, rejected);
    }
}

/*
 * Assert that the info object reports at least one hint, and that every
 * reported hint has a non-empty value.  Issue #13367 was that
 * MPI_File_get_info returned zero hints.
 */
static inline void ofi_expect_nonempty(ofi_result_t *res, MPI_Info info)
{
    int nkeys = 0;
    int i;

    ++res->checks;
    if (MPI_SUCCESS != MPI_Info_get_nkeys(info, &nkeys) || nkeys <= 0) {
        ++res->failures;
        printf("FAIL: MPI_File_get_info returned %d hints, expected at least 1\n",
               nkeys);
        return;
    }
    printf("PASS: MPI_File_get_info returned %d hint(s)\n", nkeys);

    for (i = 0; i < nkeys; ++i) {
        char key[MPI_MAX_INFO_KEY + 1];
        char value[MPI_MAX_INFO_VAL + 1];

        ++res->checks;
        if (MPI_SUCCESS != MPI_Info_get_nthkey(info, i, key)) {
            ++res->failures;
            printf("FAIL: MPI_Info_get_nthkey(%d) failed\n", i);
            continue;
        }
        if (!ofi_info_get(info, key, value) || '\0' == value[0]) {
            ++res->failures;
            printf("FAIL: reported hint '%s' has an empty value\n", key);
        } else {
            printf("PASS: reported hint '%s' = '%s'\n", key, value);
        }
    }
}

/*
 * Build a unique, single-process scratch filename.  The pid keeps
 * concurrent test programs (and any accidental multi-rank launch) from
 * colliding.  The file itself is always opened MPI_MODE_DELETE_ON_CLOSE.
 */
static inline void ofi_make_filename(char *buf, size_t buflen, const char *tag)
{
    const char *dir = getenv("TMPDIR");

    if (NULL == dir || '\0' == dir[0]) {
        dir = "/tmp";
    }
    snprintf(buf, buflen, "%s/ompio-file-info-%s-%ld.dat",
             dir, tag, (long) getpid());
}

/*
 * Open a scratch file on MPI_COMM_SELF for read/write, creating it and
 * arranging for it to be removed on close.  Returns the MPI_File_open
 * return code; on success the file uses MPI_ERRORS_RETURN.
 */
static inline int ofi_open(MPI_File *fh, const char *filename, MPI_Info info)
{
    int rc = MPI_File_open(MPI_COMM_SELF, filename,
                           MPI_MODE_CREATE | MPI_MODE_RDWR
                           | MPI_MODE_DELETE_ON_CLOSE,
                           info, fh);
    if (MPI_SUCCESS == rc) {
        MPI_File_set_errhandler(*fh, MPI_ERRORS_RETURN);
    }
    return rc;
}

/* Print the final result line and return the process exit status. */
static inline int ofi_finish(const char *name, const ofi_result_t *res)
{
    if (0 == res->failures) {
        printf("%s: all %d check(s) passed\n", name, res->checks);
        return EXIT_SUCCESS;
    }
    printf("%s: %d of %d check(s) failed\n", name, res->failures, res->checks);
    return EXIT_FAILURE;
}

#endif /* OMPIO_FILE_INFO_COMMON_H */
