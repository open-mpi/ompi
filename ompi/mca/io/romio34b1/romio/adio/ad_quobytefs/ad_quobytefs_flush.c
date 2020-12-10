/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */


#include "adio.h"

#include "ad_quobytefs.h"

void ADIOI_QUOBYTEFS_Flush(ADIO_File fd, int *error_code)
{
    int err;
    static char myname[] = "ADIOI_QUOBYTEFS_FLUSH";

    /* the deferred-open optimization may mean that a file has not been opened
     * on this processor */
    if (fd->is_open > 0) {
        /* alexey: the original uses fsync although it is named flush */
        err = quobyte_fsync(fd->file_handle);
        if (err == -1) {
            *error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
                                               myname, __LINE__, MPI_ERR_IO,
                                               "**io", "**io %s", strerror(errno));
            return;
        }
    }
    *error_code = MPI_SUCCESS;
}
