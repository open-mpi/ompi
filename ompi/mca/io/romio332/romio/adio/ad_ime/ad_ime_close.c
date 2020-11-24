/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *
 *   Copyright (C) 1997 University of Chicago.
 *   Copyright (C) 2017 DataDirect Networks.
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_ime.h"
#include "ad_ime_common.h"
#include <assert.h>

void ADIOI_IME_Close(ADIO_File fd, int *error_code)
{
    static char myname[] = "ADIOI_IME_CLOSE";
    int ret;
    struct ADIOI_IME_fs_s *ime_fs;
    int tmp_error_code;

    ret = ime_native_close(fd->fd_sys);
    if (ret != 0) {
        tmp_error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                              MPIR_ERR_RECOVERABLE,
                                              myname, __LINE__,
                                              MPI_ERR_UNKNOWN, "Error in ime_native_close", 0);
    } else {
        tmp_error_code = MPI_SUCCESS;
    }

    if (error_code) {
        *error_code = tmp_error_code;
    }

    ime_fs = (ADIOI_IME_fs *) fd->fs_ptr;
    assert(ime_fs);
    ADIOI_Free(ime_fs->ime_filename);
    ime_fs->ime_filename = NULL;
    ADIOI_Free(ime_fs);

    /* reset fds */
    fd->fd_direct = -1;
    fd->fd_sys = -1;
}
