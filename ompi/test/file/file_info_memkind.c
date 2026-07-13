/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Single-process (no launcher required) test for the MPI-4.1
 * memory-allocation-kind info keys on MPI_File objects.
 *
 * MPI-5.0 section 11.4.3 requires that "mpi_memory_alloc_kinds" be
 * contained in the info object returned by MPI_FILE_GET_INFO, and that
 * for a file derived from the World Model its value be identical to the
 * value reported by MPI_COMM_GET_INFO on MPI_COMM_WORLD/MPI_COMM_SELF.
 * It further requires (sections 11.4.3 and 14.2.8) that when the user
 * asserts a supported set of kinds via "mpi_assert_memory_alloc_kinds"
 * at file-open time, MPI_FILE_GET_INFO return that key with a value
 * identical to the one the user supplied.
 *
 * Open MPI only populates "mpi_memory_alloc_kinds" when the kinds were
 * requested (via the OMPI_MCA_mpi_memory_alloc_kinds MCA parameter, read
 * when the World Model instance is built; see
 * ompi/communicator/comm_init.c).  This test therefore requests "system"
 * -- a kind that is always supported -- in the environment before
 * MPI_Init, then verifies that the file layer carries the keys through
 * open, set_info, and set_view.
 */

#include "ompio_file_info_common.h"

int main(int argc, char *argv[])
{
    const char *name = "file_info_memkind";
    finfo_result_t res = {0, 0};
    char filename[256];
    char world_kinds[MPI_MAX_INFO_VAL + 1];
    MPI_File fh;
    MPI_Info info;
    MPI_Info info_used;
    MPI_Info world_info;
    int rc;

    /*
     * Request a supported memory allocation kind before MPI is
     * initialized; Open MPI reads this when it constructs the World Model
     * instance.  setenv() with overwrite forces a deterministic value
     * regardless of the caller's environment.
     */
    setenv("OMPI_MCA_mpi_memory_alloc_kinds", "system", 1);

    MPI_Init(&argc, &argv);
    MPI_File_set_errhandler(MPI_FILE_NULL, MPI_ERRORS_RETURN);

    /*
     * Capture the World Model reference value.  MPI is free to report
     * additional kinds beyond the requested "system", so the file checks
     * below compare against whatever MPI_COMM_WORLD reports rather than a
     * hard-coded spelling.
     */
    rc = MPI_Comm_get_info(MPI_COMM_WORLD, &world_info);
    finfo_check_rc(&res, rc, "MPI_Comm_get_info(MPI_COMM_WORLD)");
    if (MPI_SUCCESS != rc) {
        /* A failed query is a real error, not a skip. */
        MPI_Finalize();
        return finfo_finish(name, &res);
    }
    if (!finfo_info_get(world_info, "mpi_memory_alloc_kinds", world_kinds)) {
        /*
         * The instance did not pick up the requested kinds (for example,
         * the MCA parameter was pinned to a different value in the
         * environment).  There is no reference value to compare against,
         * so mark the whole test skipped.  Automake's exit-status test
         * driver treats exit code 77 as SKIP; returning it keeps a
         * lost-coverage environment visible under 'make check' instead of
         * reporting a false PASS.
         */
        printf("SKIP: mpi_memory_alloc_kinds not reported on MPI_COMM_WORLD; "
               "nothing to test\n");
        MPI_Info_free(&world_info);
        MPI_Finalize();
        return 77;
    }
    printf("INFO: MPI_COMM_WORLD mpi_memory_alloc_kinds = '%s'\n", world_kinds);
    MPI_Info_free(&world_info);

    finfo_make_filename(filename, sizeof(filename), "memkind");

    /*
     * Case 1: a file opened with MPI_INFO_NULL must still report
     * mpi_memory_alloc_kinds, and the value must match the World Model
     * value.  It must also survive set_info (which cannot update or
     * delete the key) and set_view (which rebuilds the reported hint set
     * from a staged copy).
     */
    rc = finfo_open(&fh, filename, MPI_INFO_NULL);
    finfo_check_rc(&res, rc, "MPI_File_open(MPI_INFO_NULL)");
    if (MPI_SUCCESS == rc) {
        rc = MPI_File_get_info(fh, &info_used);
        finfo_check_rc(&res, rc, "MPI_File_get_info (after open)");
        if (MPI_SUCCESS == rc) {
            finfo_expect_value(&res, info_used, "mpi_memory_alloc_kinds",
                               world_kinds);
            MPI_Info_free(&info_used);
        }

        rc = MPI_Info_create(&info);
        finfo_check_rc(&res, rc, "MPI_Info_create (set_info)");
        if (MPI_SUCCESS == rc) {
            /* Update an unrelated hint; the memkind key must be untouched. */
            MPI_Info_set(info, "cb_buffer_size", "65536");
            rc = MPI_File_set_info(fh, info);
            finfo_check_rc(&res, rc, "MPI_File_set_info");
            MPI_Info_free(&info);
        }
        rc = MPI_File_get_info(fh, &info_used);
        finfo_check_rc(&res, rc, "MPI_File_get_info (after set_info)");
        if (MPI_SUCCESS == rc) {
            finfo_expect_value(&res, info_used, "mpi_memory_alloc_kinds",
                               world_kinds);
            MPI_Info_free(&info_used);
        }

        rc = MPI_File_set_view(fh, 0, MPI_BYTE, MPI_BYTE, "native",
                               MPI_INFO_NULL);
        finfo_check_rc(&res, rc, "MPI_File_set_view");
        rc = MPI_File_get_info(fh, &info_used);
        finfo_check_rc(&res, rc, "MPI_File_get_info (after set_view)");
        if (MPI_SUCCESS == rc) {
            finfo_expect_value(&res, info_used, "mpi_memory_alloc_kinds",
                               world_kinds);
            MPI_Info_free(&info_used);
        }

        rc = MPI_File_close(&fh);
        finfo_check_rc(&res, rc, "MPI_File_close");
    }

    /*
     * Case 2: assert a supported subset of kinds at open time.  Because
     * "system" is supported, the implementation recognizes the assertion
     * and MPI_File_get_info must return mpi_assert_memory_alloc_kinds
     * identical to the supplied value, alongside mpi_memory_alloc_kinds.
     */
    rc = MPI_Info_create(&info);
    finfo_check_rc(&res, rc, "MPI_Info_create (assert)");
    if (MPI_SUCCESS == rc) {
        MPI_Info_set(info, "mpi_assert_memory_alloc_kinds", "system");
        rc = finfo_open(&fh, filename, info);
        finfo_check_rc(&res, rc, "MPI_File_open(mpi_assert_memory_alloc_kinds)");
        MPI_Info_free(&info);
        if (MPI_SUCCESS == rc) {
            rc = MPI_File_get_info(fh, &info_used);
            finfo_check_rc(&res, rc, "MPI_File_get_info (assert)");
            if (MPI_SUCCESS == rc) {
                finfo_expect_value(&res, info_used,
                                   "mpi_assert_memory_alloc_kinds", "system");
                /*
                 * A recognized assertion narrows the reported kinds: the
                 * file must report exactly the asserted "system", not the
                 * broader World Model value (which also contains "mpi").
                 * Checking only presence would let a regression that left
                 * the parent value in place slip through.
                 */
                finfo_expect_value(&res, info_used, "mpi_memory_alloc_kinds",
                                   "system");
                MPI_Info_free(&info_used);
            }
            rc = MPI_File_close(&fh);
            finfo_check_rc(&res, rc, "MPI_File_close (assert)");
        }
    }

    MPI_Finalize();
    return finfo_finish(name, &res);
}
