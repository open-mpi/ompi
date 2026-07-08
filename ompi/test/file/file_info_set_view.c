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
 * handling at MPI_File_set_view time.
 *
 * The info argument to MPI_File_set_view is routed through the same hint
 * subscription mechanism as open/set_info: view-related hints that OMPIO
 * accepts (here cb_nodes and collective_buffering) must be reflected by a
 * subsequent MPI_File_get_info, unknown keys must not be reported, and
 * hints that were in effect before the view change must be preserved.
 */

#include "ompio_file_info_common.h"

int main(int argc, char *argv[])
{
    const char *name = "file_info_set_view";
    finfo_result_t res = {0, 0};
    char filename[256];
    MPI_File fh;
    MPI_Info info;
    MPI_Info info_used;
    MPI_Datatype filetype;
    int rc;

    MPI_Init(&argc, &argv);
    MPI_File_set_errhandler(MPI_FILE_NULL, MPI_ERRORS_RETURN);

    finfo_make_filename(filename, sizeof(filename), "set-view");

    rc = finfo_open(&fh, filename, MPI_INFO_NULL);
    finfo_check_rc(&res, rc, "MPI_File_open(MPI_INFO_NULL)");
    if (MPI_SUCCESS != rc) {
        MPI_Finalize();
        return finfo_finish(name, &res);
    }

    /* A simple committed filetype is enough to set a view. */
    rc = MPI_Type_contiguous(2, MPI_BYTE, &filetype);
    finfo_check_rc(&res, rc, "MPI_Type_contiguous");
    if (MPI_SUCCESS == rc) {
        rc = MPI_Type_commit(&filetype);
        finfo_check_rc(&res, rc, "MPI_Type_commit");
    }
    if (MPI_SUCCESS != rc) {
        MPI_File_close(&fh);
        MPI_Finalize();
        return finfo_finish(name, &res);
    }

    /* Set a view carrying view-related hints plus an unknown one. */
    rc = MPI_Info_create(&info);
    finfo_check_rc(&res, rc, "MPI_Info_create");
    if (MPI_SUCCESS == rc) {
        MPI_Info_set(info, "cb_nodes", "1");
        MPI_Info_set(info, "collective_buffering", "false");
        MPI_Info_set(info, "unknown_ompio_hint", "set-view-ignore-me");
        rc = MPI_File_set_view(fh, 0, MPI_BYTE, filetype, "native", info);
        finfo_check_rc(&res, rc, "MPI_File_set_view (with hints)");
        MPI_Info_free(&info);
    }

    rc = MPI_File_get_info(fh, &info_used);
    finfo_check_rc(&res, rc, "MPI_File_get_info (after set_view)");
    if (MPI_SUCCESS == rc) {
        finfo_expect_value(&res, info_used, "cb_nodes", "1");
        finfo_expect_value(&res, info_used, "collective_buffering", "false");
        finfo_expect_no_key(&res, info_used, "unknown_ompio_hint");
        /* Default hints from open survive a view change. */
        finfo_expect_key(&res, info_used, "cb_buffer_size");
        MPI_Info_free(&info_used);
    }

    /* A second view with only an unknown key preserves the prior hints. */
    rc = MPI_Info_create(&info);
    finfo_check_rc(&res, rc, "MPI_Info_create");
    if (MPI_SUCCESS == rc) {
        MPI_Info_set(info, "unknown_ompio_hint", "unknown-only-set-view");
        rc = MPI_File_set_view(fh, 0, MPI_BYTE, filetype, "native", info);
        finfo_check_rc(&res, rc, "MPI_File_set_view (unknown only)");
        MPI_Info_free(&info);
    }

    rc = MPI_File_get_info(fh, &info_used);
    finfo_check_rc(&res, rc, "MPI_File_get_info (after unknown-only set_view)");
    if (MPI_SUCCESS == rc) {
        finfo_expect_value(&res, info_used, "cb_nodes", "1");
        finfo_expect_value(&res, info_used, "collective_buffering", "false");
        finfo_expect_no_key(&res, info_used, "unknown_ompio_hint");
        MPI_Info_free(&info_used);
    }

    MPI_Type_free(&filetype);

    rc = MPI_File_close(&fh);
    finfo_check_rc(&res, rc, "MPI_File_close");

    MPI_Finalize();
    return finfo_finish(name, &res);
}
