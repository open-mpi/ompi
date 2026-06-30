/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Single-process (no launcher required) regression test for Open MPI
 * issue #13367: MPI_File_get_info on an OMPIO file opened with
 * MPI_INFO_NULL must return the implementation's supported, defaulted
 * file hints rather than an empty info object.
 *
 * MPI-5.0 section 15.2.8 requires the info object returned by
 * MPI_File_get_info to contain the current setting of all hints
 * associated with the file, including hints the implementation supports
 * with default values.
 */

#include "ompio_file_info_common.h"

int main(int argc, char *argv[])
{
    const char *name = "file_info_defaults";
    ofi_result_t res = {0, 0};
    char filename[256];
    MPI_File fh;
    MPI_Info info_used;
    int rc;

    MPI_Init(&argc, &argv);

    /* Make MPI_File_open report failures instead of aborting. */
    MPI_File_set_errhandler(MPI_FILE_NULL, MPI_ERRORS_RETURN);

    ofi_make_filename(filename, sizeof(filename), "defaults");

    rc = ofi_open(&fh, filename, MPI_INFO_NULL);
    ofi_check_rc(&res, rc, "MPI_File_open(MPI_INFO_NULL)");
    if (MPI_SUCCESS != rc) {
        MPI_Finalize();
        return ofi_finish(name, &res);
    }

    rc = MPI_File_get_info(fh, &info_used);
    ofi_check_rc(&res, rc, "MPI_File_get_info");
    if (MPI_SUCCESS == rc) {
        /*
         * The headline #13367 symptom: a file opened with MPI_INFO_NULL
         * reported zero hints.  Require a non-empty, well-formed result.
         */
        ofi_expect_nonempty(&res, info_used);

        /*
         * OMPIO core registers these hints with defaults, so they must
         * appear even though the user supplied no info.
         */
        ofi_expect_key(&res, info_used, "cb_buffer_size");
        ofi_expect_value(&res, info_used, "collective_buffering", "true");

        /*
         * cb_nodes defaults to the internal "-1" (auto) sentinel, which
         * must never be reported to the application: the hint is either
         * omitted or carries a real aggregator count.
         */
        ofi_expect_absent_or_not_value(&res, info_used, "cb_nodes", "-1");

        MPI_Info_free(&info_used);
    }

    rc = MPI_File_close(&fh);
    ofi_check_rc(&res, rc, "MPI_File_close");

    MPI_Finalize();
    return ofi_finish(name, &res);
}
