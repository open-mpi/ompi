/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Single-process (no launcher required) test for OMPIO MPI_Info hint
 * handling at MPI_File_open and MPI_File_set_info time.
 *
 * MPI-5.0 section 15.2.8 requires that an accepted user hint be reflected
 * by MPI_File_get_info, that an unknown/ignored hint not be reported, and
 * that MPI_File_set_info update the named hints while leaving previous and
 * default values for omitted (or ignored) hints in effect.
 */

#include "ompio_file_info_common.h"

int main(int argc, char *argv[])
{
    const char *name = "file_info_hints";
    ofi_result_t res = {0, 0};
    char filename[256];
    MPI_File fh;
    MPI_Info info;
    MPI_Info info_used;
    int rc;

    MPI_Init(&argc, &argv);
    MPI_File_set_errhandler(MPI_FILE_NULL, MPI_ERRORS_RETURN);

    ofi_make_filename(filename, sizeof(filename), "hints");

    /* Open with a supported hint plus an unknown one. */
    rc = MPI_Info_create(&info);
    ofi_check_rc(&res, rc, "MPI_Info_create");
    if (MPI_SUCCESS != rc) {
        MPI_Finalize();
        return ofi_finish(name, &res);
    }
    MPI_Info_set(info, "cb_buffer_size", "12345");
    MPI_Info_set(info, "unknown_ompio_hint", "ignore-me");

    rc = ofi_open(&fh, filename, info);
    ofi_check_rc(&res, rc, "MPI_File_open(user hints)");
    MPI_Info_free(&info);
    if (MPI_SUCCESS != rc) {
        MPI_Finalize();
        return ofi_finish(name, &res);
    }

    /* The accepted hint must be reported; the unknown one must not. */
    rc = MPI_File_get_info(fh, &info_used);
    ofi_check_rc(&res, rc, "MPI_File_get_info (after open)");
    if (MPI_SUCCESS == rc) {
        ofi_expect_value(&res, info_used, "cb_buffer_size", "12345");
        ofi_expect_value(&res, info_used, "collective_buffering", "true");
        ofi_expect_no_key(&res, info_used, "unknown_ompio_hint");
        MPI_Info_free(&info_used);
    }

    /* MPI_File_set_info updates a mutable hint; unknown keys are ignored. */
    rc = MPI_Info_create(&info);
    ofi_check_rc(&res, rc, "MPI_Info_create");
    if (MPI_SUCCESS == rc) {
        MPI_Info_set(info, "cb_buffer_size", "23456");
        MPI_Info_set(info, "unknown_ompio_hint", "still-ignore-me");
        rc = MPI_File_set_info(fh, info);
        ofi_check_rc(&res, rc, "MPI_File_set_info (update)");
        MPI_Info_free(&info);
    }

    rc = MPI_File_get_info(fh, &info_used);
    ofi_check_rc(&res, rc, "MPI_File_get_info (after set_info)");
    if (MPI_SUCCESS == rc) {
        ofi_expect_value(&res, info_used, "cb_buffer_size", "23456");
        /* A hint not named in set_info keeps its previous/default value. */
        ofi_expect_value(&res, info_used, "collective_buffering", "true");
        ofi_expect_no_key(&res, info_used, "unknown_ompio_hint");
        ofi_expect_absent_or_not_value(&res, info_used, "cb_nodes", "-1");
        MPI_Info_free(&info_used);
    }

    /* set_info with only an unknown key must leave prior hints untouched. */
    rc = MPI_Info_create(&info);
    ofi_check_rc(&res, rc, "MPI_Info_create");
    if (MPI_SUCCESS == rc) {
        MPI_Info_set(info, "unknown_ompio_hint", "unknown-only");
        rc = MPI_File_set_info(fh, info);
        ofi_check_rc(&res, rc, "MPI_File_set_info (unknown only)");
        MPI_Info_free(&info);
    }

    rc = MPI_File_get_info(fh, &info_used);
    ofi_check_rc(&res, rc, "MPI_File_get_info (after unknown-only set_info)");
    if (MPI_SUCCESS == rc) {
        ofi_expect_value(&res, info_used, "cb_buffer_size", "23456");
        ofi_expect_value(&res, info_used, "collective_buffering", "true");
        ofi_expect_no_key(&res, info_used, "unknown_ompio_hint");
        MPI_Info_free(&info_used);
    }

    rc = MPI_File_close(&fh);
    ofi_check_rc(&res, rc, "MPI_File_close");

    MPI_Finalize();
    return ofi_finish(name, &res);
}
