/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */


#include "adio.h"

#include "ad_quobytefs.h"

void ADIOI_QUOBYTEFS_Close(ADIO_File fd, int *error_code)
{
    static char myname[] = "ADIOI_QUOBYTEFS_CLOSE";
    struct quobyte_fh *file_handle = fd->file_handle;
    if (file_handle != NULL) {
        if (quobyte_close(file_handle)) {
            *error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
                                               myname, __LINE__, MPI_ERR_IO, "**io",
                                               "Quobyte failed to close the file: %s",
                                               strerror(errno));
            return;
        }
        *error_code = MPI_SUCCESS;
    } else {
        *error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, myname,
                                           __LINE__, MPI_ERR_IO, "Quobyte file header is null", 0);
        return;
    }
}
