/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Manual invocation:
 *
 *   mpirun -n 2 --mca io ompio ./ompio_file_info
 *
 * The memory-allocation-kind checks request a supported kind in-process
 * (see main()), so they run without the launcher having to forward the
 * MCA parameter.
 *
 * Optional filesystem-specific checks:
 *
 *   OMPIO_FILE_INFO_LUSTRE_DIR=/mnt/lustre \
 *       mpirun -n 2 --mca io ompio ./ompio_file_info
 *   OMPIO_FILE_INFO_GPFS_DIR=/gpfs \
 *       mpirun -n 2 --mca io ompio ./ompio_file_info
 */

#include "mpi.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static int errors = 0;

enum {
    SECTION_SKIPPED = 0,
    SECTION_RAN = 1
};

static void section_start(const char *name, int rank)
{
    if (0 == rank) {
        printf("ompio_file_info: %s: start\n", name);
        fflush(stdout);
    }
}

static void section_finish(const char *name, int errors_before, int ran, int rank)
{
    int local_failed = (errors_before != errors) ? 1 : 0;
    int failed = 0;

    MPI_Allreduce(&local_failed, &failed, 1, MPI_INT, MPI_MAX, MPI_COMM_WORLD);

    if (0 == rank) {
        if (SECTION_SKIPPED == ran) {
            printf("ompio_file_info: %s: SKIP\n", name);
        } else if (failed) {
            printf("ompio_file_info: %s: FAIL\n", name);
        } else {
            printf("ompio_file_info: %s: PASS\n", name);
        }
        fflush(stdout);
    }
}

static void check_rc(int rc, const char *call, int rank)
{
    if (MPI_SUCCESS != rc) {
        fprintf(stderr, "[%d] %s failed with MPI error %d\n", rank, call, rc);
        ++errors;
    }
}

static int info_has_key(MPI_Info info, const char *key, char *value, int value_len)
{
    int flag = 0;
    int buflen = value_len;

    value[0] = '\0';
    MPI_Info_get_string(info, key, &buflen, value, &flag);
    return flag;
}

static void expect_key(MPI_Info info, const char *key, int rank)
{
    char value[MPI_MAX_INFO_VAL + 1];

    if (!info_has_key(info, key, value, MPI_MAX_INFO_VAL)) {
        fprintf(stderr, "[%d] expected key %s was not returned\n", rank, key);
        ++errors;
    }
}

static void expect_value(MPI_Info info, const char *key, const char *expected, int rank)
{
    char value[MPI_MAX_INFO_VAL + 1];

    if (!info_has_key(info, key, value, MPI_MAX_INFO_VAL)) {
        fprintf(stderr, "[%d] expected key %s was not returned\n", rank, key);
        ++errors;
        return;
    }

    if (0 != strcmp(value, expected)) {
        fprintf(stderr, "[%d] expected %s=%s, got %s\n", rank, key, expected, value);
        ++errors;
    }
}

static void expect_no_key(MPI_Info info, const char *key, int rank)
{
    char value[MPI_MAX_INFO_VAL + 1];

    if (info_has_key(info, key, value, MPI_MAX_INFO_VAL)) {
        fprintf(stderr, "[%d] unexpected key %s=%s was returned\n", rank, key, value);
        ++errors;
    }
}

static void expect_absent_or_not_value(MPI_Info info, const char *key,
                                       const char *rejected, int rank)
{
    char value[MPI_MAX_INFO_VAL + 1];

    if (info_has_key(info, key, value, MPI_MAX_INFO_VAL)
        && 0 == strcmp(value, rejected)) {
        fprintf(stderr, "[%d] unexpected %s=%s was returned\n", rank, key, value);
        ++errors;
    }
}

static void expect_same_optional_value(MPI_Info info, const char *key,
                                       int expected_flag, const char *expected,
                                       int rank)
{
    if (expected_flag) {
        expect_value(info, key, expected, rank);
    } else {
        expect_no_key(info, key, rank);
    }
}

static void make_filename_in_dir(char *filename, size_t filename_len,
                                 const char *dir, const char *tag, int rank)
{
    const char *jobid;
    char jobid_buf[32];

    if (0 == rank) {
        jobid = getenv("PMIX_NAMESPACE");
        if (NULL == jobid) {
            snprintf(jobid_buf, sizeof(jobid_buf), "%ld", (long) getpid());
            jobid = jobid_buf;
        }

        snprintf(filename, filename_len, "%s/ompio-file-info-%s-%s.dat",
                 dir, tag, jobid);
    }

    MPI_Bcast(filename, (int) filename_len, MPI_CHAR, 0, MPI_COMM_WORLD);
}

static void make_filename(char *filename, size_t filename_len, int rank)
{
    const char *tmpdir;

    tmpdir = getenv("TMPDIR");
    if (NULL == tmpdir) {
        tmpdir = "/tmp";
    }

    make_filename_in_dir(filename, filename_len, tmpdir, "generic", rank);
}

static void check_default_info(const char *filename, int rank)
{
    MPI_File fh;
    MPI_Info info_used;
    int rc;

    rc = MPI_File_open(MPI_COMM_WORLD, filename,
                       MPI_MODE_CREATE | MPI_MODE_RDWR | MPI_MODE_DELETE_ON_CLOSE,
                       MPI_INFO_NULL, &fh);
    check_rc(rc, "MPI_File_open", rank);
    if (MPI_SUCCESS != rc) {
        return;
    }

    rc = MPI_File_get_info(fh, &info_used);
    check_rc(rc, "MPI_File_get_info", rank);
    if (MPI_SUCCESS == rc) {
        expect_key(info_used, "cb_buffer_size", rank);
        expect_absent_or_not_value(info_used, "cb_nodes", "-1", rank);
        expect_key(info_used, "collective_buffering", rank);
        MPI_Info_free(&info_used);
    }

    rc = MPI_File_close(&fh);
    check_rc(rc, "MPI_File_close", rank);
}

static void check_user_info(const char *filename, int rank)
{
    MPI_File fh;
    MPI_Info info;
    MPI_Info info_used;
    MPI_Datatype filetype = MPI_DATATYPE_NULL;
    char cb_nodes_value[MPI_MAX_INFO_VAL + 1];
    char collective_buffering_value[MPI_MAX_INFO_VAL + 1];
    int cb_nodes_flag = 0;
    int collective_buffering_flag = 0;
    int filetype_ready = 0;
    int rc;

    rc = MPI_Info_create(&info);
    check_rc(rc, "MPI_Info_create", rank);
    if (MPI_SUCCESS != rc) {
        return;
    }

    rc = MPI_Info_set(info, "cb_buffer_size", "12345");
    check_rc(rc, "MPI_Info_set", rank);
    rc = MPI_Info_set(info, "unknown_ompio_hint", "ignore-me");
    check_rc(rc, "MPI_Info_set", rank);

    rc = MPI_File_open(MPI_COMM_WORLD, filename,
                       MPI_MODE_CREATE | MPI_MODE_RDWR | MPI_MODE_DELETE_ON_CLOSE,
                       info, &fh);
    check_rc(rc, "MPI_File_open", rank);
    MPI_Info_free(&info);
    if (MPI_SUCCESS != rc) {
        return;
    }
    rc = MPI_File_set_errhandler(fh, MPI_ERRORS_RETURN);
    check_rc(rc, "MPI_File_set_errhandler", rank);

    rc = MPI_File_get_info(fh, &info_used);
    check_rc(rc, "MPI_File_get_info", rank);
    if (MPI_SUCCESS == rc) {
        expect_value(info_used, "cb_buffer_size", "12345", rank);
        expect_no_key(info_used, "unknown_ompio_hint", rank);
        MPI_Info_free(&info_used);
    }

    rc = MPI_Info_create(&info);
    check_rc(rc, "MPI_Info_create", rank);
    if (MPI_SUCCESS == rc) {
        rc = MPI_Info_set(info, "cb_buffer_size", "23456");
        check_rc(rc, "MPI_Info_set", rank);
        rc = MPI_Info_set(info, "unknown_ompio_hint", "still-ignore-me");
        check_rc(rc, "MPI_Info_set", rank);

        rc = MPI_File_set_info(fh, info);
        check_rc(rc, "MPI_File_set_info", rank);
        MPI_Info_free(&info);
    }

    rc = MPI_File_get_info(fh, &info_used);
    check_rc(rc, "MPI_File_get_info", rank);
    if (MPI_SUCCESS == rc) {
        expect_value(info_used, "cb_buffer_size", "23456", rank);
        cb_nodes_flag = info_has_key(info_used, "cb_nodes", cb_nodes_value,
                                     MPI_MAX_INFO_VAL);
        if (cb_nodes_flag && 0 == strcmp(cb_nodes_value, "-1")) {
            fprintf(stderr, "[%d] unexpected cb_nodes=-1 was returned\n", rank);
            ++errors;
        }
        collective_buffering_flag = info_has_key(info_used, "collective_buffering",
                                                 collective_buffering_value,
                                                 MPI_MAX_INFO_VAL);
        if (!collective_buffering_flag) {
            fprintf(stderr, "[%d] expected key collective_buffering was not returned\n", rank);
            ++errors;
        }
        expect_no_key(info_used, "unknown_ompio_hint", rank);
        MPI_Info_free(&info_used);
    }

    rc = MPI_Info_create(&info);
    check_rc(rc, "MPI_Info_create", rank);
    if (MPI_SUCCESS == rc) {
        rc = MPI_Info_set(info, "unknown_ompio_hint", "unknown-only");
        check_rc(rc, "MPI_Info_set", rank);

        rc = MPI_File_set_info(fh, info);
        check_rc(rc, "MPI_File_set_info", rank);
        MPI_Info_free(&info);
    }

    rc = MPI_File_get_info(fh, &info_used);
    check_rc(rc, "MPI_File_get_info", rank);
    if (MPI_SUCCESS == rc) {
        expect_value(info_used, "cb_buffer_size", "23456", rank);
        expect_same_optional_value(info_used, "cb_nodes", cb_nodes_flag,
                                   cb_nodes_value, rank);
        expect_same_optional_value(info_used, "collective_buffering",
                                   collective_buffering_flag,
                                   collective_buffering_value, rank);
        expect_no_key(info_used, "unknown_ompio_hint", rank);
        MPI_Info_free(&info_used);
    }

    rc = MPI_Type_contiguous(2, MPI_BYTE, &filetype);
    check_rc(rc, "MPI_Type_contiguous", rank);
    if (MPI_SUCCESS == rc) {
        rc = MPI_Type_commit(&filetype);
        check_rc(rc, "MPI_Type_commit", rank);
        if (MPI_SUCCESS == rc) {
            filetype_ready = 1;
        }
    }

    rc = MPI_Info_create(&info);
    check_rc(rc, "MPI_Info_create", rank);
    if (MPI_SUCCESS == rc) {
        if (filetype_ready) {
            rc = MPI_Info_set(info, "cb_nodes", "1");
            check_rc(rc, "MPI_Info_set", rank);
            rc = MPI_Info_set(info, "collective_buffering", "false");
            check_rc(rc, "MPI_Info_set", rank);
            rc = MPI_Info_set(info, "unknown_ompio_hint", "set-view-ignore-me");
            check_rc(rc, "MPI_Info_set", rank);

            rc = MPI_File_set_view(fh, 0, MPI_BYTE, filetype, "native", info);
            check_rc(rc, "MPI_File_set_view", rank);
        }
        MPI_Info_free(&info);
    }

    rc = MPI_File_get_info(fh, &info_used);
    check_rc(rc, "MPI_File_get_info", rank);
    if (MPI_SUCCESS == rc) {
        expect_value(info_used, "cb_buffer_size", "23456", rank);
        expect_value(info_used, "cb_nodes", "1", rank);
        expect_value(info_used, "collective_buffering", "false", rank);
        expect_no_key(info_used, "unknown_ompio_hint", rank);
        MPI_Info_free(&info_used);
    }

    rc = MPI_Info_create(&info);
    check_rc(rc, "MPI_Info_create", rank);
    if (MPI_SUCCESS == rc) {
        if (filetype_ready) {
            rc = MPI_Info_set(info, "unknown_ompio_hint", "unknown-only-set-view");
            check_rc(rc, "MPI_Info_set", rank);

            rc = MPI_File_set_view(fh, 0, MPI_BYTE, filetype, "native", info);
            check_rc(rc, "MPI_File_set_view", rank);
        }
        MPI_Info_free(&info);
    }

    rc = MPI_File_get_info(fh, &info_used);
    check_rc(rc, "MPI_File_get_info", rank);
    if (MPI_SUCCESS == rc) {
        expect_value(info_used, "cb_buffer_size", "23456", rank);
        expect_value(info_used, "cb_nodes", "1", rank);
        expect_value(info_used, "collective_buffering", "false", rank);
        expect_no_key(info_used, "unknown_ompio_hint", rank);
        MPI_Info_free(&info_used);
    }

    rc = MPI_Info_create(&info);
    check_rc(rc, "MPI_Info_create", rank);
    if (MPI_SUCCESS == rc) {
        rc = MPI_Info_set(info, "cb_buffer_size", "34567");
        check_rc(rc, "MPI_Info_set", rank);
        rc = MPI_Info_set(info, "cb_nodes", "2");
        check_rc(rc, "MPI_Info_set", rank);
        rc = MPI_Info_set(info, "collective_buffering", "true");
        check_rc(rc, "MPI_Info_set", rank);

        rc = MPI_File_set_view(fh, 0, MPI_INT, MPI_BYTE, "native", info);
        if (MPI_SUCCESS == rc) {
            fprintf(stderr, "[%d] MPI_File_set_view unexpectedly succeeded\n", rank);
            ++errors;
        }
        MPI_Info_free(&info);
    }

    rc = MPI_File_get_info(fh, &info_used);
    check_rc(rc, "MPI_File_get_info", rank);
    if (MPI_SUCCESS == rc) {
        expect_value(info_used, "cb_buffer_size", "23456", rank);
        expect_value(info_used, "cb_nodes", "1", rank);
        expect_value(info_used, "collective_buffering", "false", rank);
        expect_no_key(info_used, "unknown_ompio_hint", rank);
        MPI_Info_free(&info_used);
    }

    if (MPI_DATATYPE_NULL != filetype) {
        rc = MPI_Type_free(&filetype);
        check_rc(rc, "MPI_Type_free", rank);
    }

    rc = MPI_File_close(&fh);
    check_rc(rc, "MPI_File_close", rank);
}

static void check_sharedfp_info(const char *filename, int rank)
{
    MPI_File fh;
    MPI_Info info;
    MPI_Info info_used;
    char value[MPI_MAX_INFO_VAL + 1];
    int rc;

    rc = MPI_Info_create(&info);
    check_rc(rc, "MPI_Info_create", rank);
    if (MPI_SUCCESS != rc) {
        return;
    }

    rc = MPI_Info_set(info, "OMPIO_SHAREDFP_RELAXED_ORDERING", "true");
    check_rc(rc, "MPI_Info_set", rank);
    rc = MPI_Info_set(info, "unknown_sharedfp_hint", "ignore-me");
    check_rc(rc, "MPI_Info_set", rank);

    rc = MPI_File_open(MPI_COMM_WORLD, filename,
                       MPI_MODE_CREATE | MPI_MODE_RDWR | MPI_MODE_DELETE_ON_CLOSE,
                       info, &fh);
    check_rc(rc, "MPI_File_open", rank);
    MPI_Info_free(&info);
    if (MPI_SUCCESS != rc) {
        return;
    }

    rc = MPI_File_get_info(fh, &info_used);
    check_rc(rc, "MPI_File_get_info", rank);
    if (MPI_SUCCESS == rc) {
        /*
         * This hint is public only if sharedfp/individual wins selection.
         * Some builds prefer another sharedfp component even with the hint,
         * so absence is a valid result; presence must still preserve the
         * accepted user value.
         */
        if (info_has_key(info_used, "OMPIO_SHAREDFP_RELAXED_ORDERING",
                         value, MPI_MAX_INFO_VAL)
            && 0 != strcmp(value, "true")) {
            fprintf(stderr, "[%d] expected OMPIO_SHAREDFP_RELAXED_ORDERING=true, got %s\n",
                    rank, value);
            ++errors;
        }
        expect_no_key(info_used, "unknown_sharedfp_hint", rank);
        MPI_Info_free(&info_used);
    }

    rc = MPI_File_close(&fh);
    check_rc(rc, "MPI_File_close", rank);
}

/*
 * MPI-5.0 sections 11.4.3 and 14.2.8: MPI_File_get_info must report
 * "mpi_memory_alloc_kinds", and for a file derived from the World Model
 * its value must be identical to the value reported by MPI_COMM_GET_INFO
 * on MPI_COMM_WORLD.  When the user asserts a supported set of kinds via
 * "mpi_assert_memory_alloc_kinds" at open time, that key must round-trip
 * identically alongside "mpi_memory_alloc_kinds".
 *
 * Open MPI only populates "mpi_memory_alloc_kinds" when the kinds were
 * requested, so main() requests "system" (always supported) before
 * MPI_Init.  If the key is still not reported (for example, the MCA
 * parameter was pinned to a different value in the launch environment),
 * there is no reference value to check against and the section is
 * treated as skipped.  The value is compared against whatever
 * MPI_COMM_WORLD reports rather than a hard-coded spelling, since the
 * library may return additional supported kinds.
 */
static int check_memkind_info(const char *filename, int rank)
{
    MPI_File fh;
    MPI_Info info;
    MPI_Info info_used;
    char world_kinds[MPI_MAX_INFO_VAL + 1];
    int rc;

    rc = MPI_Comm_get_info(MPI_COMM_WORLD, &info_used);
    check_rc(rc, "MPI_Comm_get_info", rank);
    if (MPI_SUCCESS != rc) {
        return SECTION_RAN;
    }
    if (!info_has_key(info_used, "mpi_memory_alloc_kinds", world_kinds,
                      MPI_MAX_INFO_VAL)) {
        MPI_Info_free(&info_used);
        return SECTION_SKIPPED;
    }
    MPI_Info_free(&info_used);

    /*
     * A file opened with MPI_INFO_NULL must still report
     * mpi_memory_alloc_kinds, identical to the World Model value, and the
     * key must survive both set_info (which cannot update or delete it)
     * and set_view (which rebuilds the reported hints from a staged copy).
     */
    rc = MPI_File_open(MPI_COMM_WORLD, filename,
                       MPI_MODE_CREATE | MPI_MODE_RDWR | MPI_MODE_DELETE_ON_CLOSE,
                       MPI_INFO_NULL, &fh);
    check_rc(rc, "MPI_File_open", rank);
    if (MPI_SUCCESS != rc) {
        return SECTION_RAN;
    }
    rc = MPI_File_set_errhandler(fh, MPI_ERRORS_RETURN);
    check_rc(rc, "MPI_File_set_errhandler", rank);

    rc = MPI_File_get_info(fh, &info_used);
    check_rc(rc, "MPI_File_get_info", rank);
    if (MPI_SUCCESS == rc) {
        expect_value(info_used, "mpi_memory_alloc_kinds", world_kinds, rank);
        MPI_Info_free(&info_used);
    }

    rc = MPI_Info_create(&info);
    check_rc(rc, "MPI_Info_create", rank);
    if (MPI_SUCCESS == rc) {
        rc = MPI_Info_set(info, "cb_buffer_size", "65536");
        check_rc(rc, "MPI_Info_set", rank);
        rc = MPI_File_set_info(fh, info);
        check_rc(rc, "MPI_File_set_info", rank);
        MPI_Info_free(&info);
    }
    rc = MPI_File_get_info(fh, &info_used);
    check_rc(rc, "MPI_File_get_info", rank);
    if (MPI_SUCCESS == rc) {
        expect_value(info_used, "mpi_memory_alloc_kinds", world_kinds, rank);
        MPI_Info_free(&info_used);
    }

    rc = MPI_File_set_view(fh, 0, MPI_BYTE, MPI_BYTE, "native", MPI_INFO_NULL);
    check_rc(rc, "MPI_File_set_view", rank);
    rc = MPI_File_get_info(fh, &info_used);
    check_rc(rc, "MPI_File_get_info", rank);
    if (MPI_SUCCESS == rc) {
        expect_value(info_used, "mpi_memory_alloc_kinds", world_kinds, rank);
        MPI_Info_free(&info_used);
    }

    rc = MPI_File_close(&fh);
    check_rc(rc, "MPI_File_close", rank);

    /*
     * Assert a supported subset of kinds at open time.  Because "system"
     * is supported, the implementation recognizes the assertion and
     * MPI_File_get_info must return mpi_assert_memory_alloc_kinds
     * identical to the supplied value, with mpi_memory_alloc_kinds still
     * reported.
     */
    rc = MPI_Info_create(&info);
    check_rc(rc, "MPI_Info_create", rank);
    if (MPI_SUCCESS == rc) {
        rc = MPI_Info_set(info, "mpi_assert_memory_alloc_kinds", "system");
        check_rc(rc, "MPI_Info_set", rank);
        rc = MPI_File_open(MPI_COMM_WORLD, filename,
                           MPI_MODE_CREATE | MPI_MODE_RDWR | MPI_MODE_DELETE_ON_CLOSE,
                           info, &fh);
        check_rc(rc, "MPI_File_open", rank);
        MPI_Info_free(&info);
        if (MPI_SUCCESS == rc) {
            rc = MPI_File_set_errhandler(fh, MPI_ERRORS_RETURN);
            check_rc(rc, "MPI_File_set_errhandler", rank);

            rc = MPI_File_get_info(fh, &info_used);
            check_rc(rc, "MPI_File_get_info", rank);
            if (MPI_SUCCESS == rc) {
                expect_value(info_used, "mpi_assert_memory_alloc_kinds",
                             "system", rank);
                /*
                 * A recognized assertion narrows the reported kinds to the
                 * asserted "system"; verifying only presence would miss a
                 * regression that left the broader World Model value
                 * (which also contains "mpi") in place.
                 */
                expect_value(info_used, "mpi_memory_alloc_kinds", "system",
                             rank);
                MPI_Info_free(&info_used);
            }

            rc = MPI_File_close(&fh);
            check_rc(rc, "MPI_File_close", rank);
        }
    }

    return SECTION_RAN;
}

static int get_optional_dir(const char *env_name, char *dir, size_t dir_len, int rank)
{
    const char *value = NULL;
    int enabled = 0;

    if (0 == rank) {
        value = getenv(env_name);
        if (NULL != value && '\0' != value[0]) {
            enabled = 1;
            snprintf(dir, dir_len, "%s", value);
        }
    }

    MPI_Bcast(&enabled, 1, MPI_INT, 0, MPI_COMM_WORLD);
    if (!enabled) {
        return 0;
    }

    MPI_Bcast(dir, (int) dir_len, MPI_CHAR, 0, MPI_COMM_WORLD);
    return 1;
}

/*
 * Lustre and GPFS components require matching filesystems and, in many
 * builds, external development headers.  Keep their coverage in this manual
 * test, but gate it on an explicit directory so the generic OMPIO test still
 * runs everywhere.
 */
static void check_lustre_open_info(const char *filename, const char *size_key,
                                   const char *factor_key, int rank)
{
    MPI_File fh;
    MPI_Info info;
    MPI_Info info_used;
    int rc;

    rc = MPI_Info_create(&info);
    check_rc(rc, "MPI_Info_create", rank);
    if (MPI_SUCCESS != rc) {
        return;
    }

    rc = MPI_Info_set(info, size_key, "1048576");
    check_rc(rc, "MPI_Info_set", rank);
    rc = MPI_Info_set(info, factor_key, "2");
    check_rc(rc, "MPI_Info_set", rank);
    rc = MPI_Info_set(info, "unknown_lustre_hint", "ignore-me");
    check_rc(rc, "MPI_Info_set", rank);

    rc = MPI_File_open(MPI_COMM_WORLD, filename,
                       MPI_MODE_CREATE | MPI_MODE_RDWR | MPI_MODE_DELETE_ON_CLOSE,
                       info, &fh);
    check_rc(rc, "MPI_File_open", rank);
    MPI_Info_free(&info);
    if (MPI_SUCCESS != rc) {
        return;
    }

    rc = MPI_File_get_info(fh, &info_used);
    check_rc(rc, "MPI_File_get_info", rank);
    if (MPI_SUCCESS == rc) {
        expect_value(info_used, size_key, "1048576", rank);
        expect_value(info_used, factor_key, "2", rank);
        if (0 == strcmp(size_key, "stripe_size")) {
            expect_no_key(info_used, "striping_unit", rank);
        } else {
            expect_no_key(info_used, "stripe_size", rank);
        }
        if (0 == strcmp(factor_key, "stripe_width")) {
            expect_no_key(info_used, "striping_factor", rank);
        } else {
            expect_no_key(info_used, "stripe_width", rank);
        }
        expect_no_key(info_used, "unknown_lustre_hint", rank);
        MPI_Info_free(&info_used);
    }

    rc = MPI_File_close(&fh);
    check_rc(rc, "MPI_File_close", rank);
}

static int check_lustre_info(int rank)
{
    char dir[MPI_MAX_INFO_VAL + 1];
    char filename[MPI_MAX_INFO_VAL + 128];

    if (!get_optional_dir("OMPIO_FILE_INFO_LUSTRE_DIR", dir, sizeof(dir), rank)) {
        return SECTION_SKIPPED;
    }

    make_filename_in_dir(filename, sizeof(filename), dir, "lustre-alias", rank);
    check_lustre_open_info(filename, "stripe_size", "stripe_width", rank);

    make_filename_in_dir(filename, sizeof(filename), dir, "lustre-canonical", rank);
    check_lustre_open_info(filename, "striping_unit", "striping_factor", rank);

    return SECTION_RAN;
}

static int check_gpfs_info(int rank)
{
    char dir[MPI_MAX_INFO_VAL + 1];
    char filename[MPI_MAX_INFO_VAL + 128];
    MPI_File fh;
    MPI_Info info;
    MPI_Info info_used;
    int rc;

    if (!get_optional_dir("OMPIO_FILE_INFO_GPFS_DIR", dir, sizeof(dir), rank)) {
        return SECTION_SKIPPED;
    }

    make_filename_in_dir(filename, sizeof(filename), dir, "gpfs", rank);

    rc = MPI_Info_create(&info);
    check_rc(rc, "MPI_Info_create", rank);
    if (MPI_SUCCESS != rc) {
        return SECTION_RAN;
    }

    rc = MPI_Info_set(info, "useSIOXLib", "false");
    check_rc(rc, "MPI_Info_set", rank);
    rc = MPI_Info_set(info, "unknown_gpfs_hint", "ignore-me");
    check_rc(rc, "MPI_Info_set", rank);

    rc = MPI_File_open(MPI_COMM_WORLD, filename,
                       MPI_MODE_CREATE | MPI_MODE_RDWR | MPI_MODE_DELETE_ON_CLOSE,
                       info, &fh);
    check_rc(rc, "MPI_File_open", rank);
    MPI_Info_free(&info);
    if (MPI_SUCCESS != rc) {
        return SECTION_RAN;
    }

    rc = MPI_File_get_info(fh, &info_used);
    check_rc(rc, "MPI_File_get_info", rank);
    if (MPI_SUCCESS == rc) {
        expect_value(info_used, "useSIOXLib", "false", rank);
        expect_no_key(info_used, "unknown_gpfs_hint", rank);
        MPI_Info_free(&info_used);
    }

    rc = MPI_File_close(&fh);
    check_rc(rc, "MPI_File_close", rank);

    return SECTION_RAN;
}

int main(int argc, char *argv[])
{
    char filename[MPI_MAX_INFO_VAL + 128];
    int global_errors;
    int errors_before;
    int rank;
    int ran;

    /*
     * Request a supported memory allocation kind before MPI is
     * initialized; Open MPI reads this when it builds the World Model
     * instance, and each rank sets it for its own instance.  This lets
     * the memkind section run without requiring the launcher to forward
     * the MCA parameter (e.g. mpirun -x).
     */
    setenv("OMPI_MCA_mpi_memory_alloc_kinds", "system", 1);

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    make_filename(filename, sizeof(filename), rank);

    errors_before = errors;
    section_start("default info", rank);
    check_default_info(filename, rank);
    section_finish("default info", errors_before, SECTION_RAN, rank);

    errors_before = errors;
    section_start("open, set_info, and set_view info", rank);
    check_user_info(filename, rank);
    section_finish("open, set_info, and set_view info", errors_before,
                   SECTION_RAN, rank);

    errors_before = errors;
    section_start("sharedfp info", rank);
    check_sharedfp_info(filename, rank);
    section_finish("sharedfp info", errors_before, SECTION_RAN, rank);

    errors_before = errors;
    section_start("memory allocation kinds info", rank);
    ran = check_memkind_info(filename, rank);
    section_finish("memory allocation kinds info", errors_before, ran, rank);

    errors_before = errors;
    section_start("Lustre info", rank);
    ran = check_lustre_info(rank);
    section_finish("Lustre info", errors_before, ran, rank);

    errors_before = errors;
    section_start("GPFS info", rank);
    ran = check_gpfs_info(rank);
    section_finish("GPFS info", errors_before, ran, rank);

    MPI_Allreduce(&errors, &global_errors, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
    if (0 == rank) {
        printf("ompio_file_info: overall: %s\n",
               (0 == global_errors) ? "PASS" : "FAIL");
        fflush(stdout);
    }
    MPI_Finalize();

    return (0 == global_errors) ? EXIT_SUCCESS : EXIT_FAILURE;
}
