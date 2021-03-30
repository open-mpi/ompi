/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "ad_ime.h"
#include "ad_ime_common.h"

#include <assert.h>

void ADIOI_IME_Flush(ADIO_File fd, int *error_code)
{
    int ret;
    static char myname[] = "ADIOI_IME_FLUSH";

    if (!error_code) {
        return;
    }

    if (!fd) {
        *error_code = MPI_ERR_FILE;
        return;
    }

    ret = ime_native_fsync(fd->fd_sys);
    if (ret != 0) {
        *error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                           MPIR_ERR_RECOVERABLE,
                                           myname, __LINE__,
                                           MPI_ERR_FILE, "Error in ime_native_fsync", 0);
        return;
    }

    *error_code = MPI_SUCCESS;

    return;
}
