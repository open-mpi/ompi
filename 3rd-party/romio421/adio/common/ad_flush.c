/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "adio.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

void ADIOI_GEN_Flush(ADIO_File fd, int *error_code)
{
    int err;
    static char myname[] = "ADIOI_GEN_FLUSH";

    *error_code = MPI_SUCCESS;

    /* the deferred-open optimization may mean that a file has not been opened
     * on this processor */
    /* additionally, if this process did no writes, there is no work to be done */
    if (fd->is_open > 0 && fd->dirty_write) {
        err = fsync(fd->fd_sys);
        /* --BEGIN ERROR HANDLING-- */
        if (err == -1) {
            *error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
                                               myname, __LINE__, MPI_ERR_IO,
                                               "**io", "**io %s", strerror(errno));
        } else {
            fd->dirty_write = 0;
        }
        /* --END ERROR HANDLING-- */
    }

    /* If MPI_File_sync is a temporally synchronizing sync, the caller can
     * avoid the 'sync/barrier/sync' process to ensure visibility and just call
     * 'sync' */
    if (fd->hints->synchronizing_flush > 0)
        MPI_Barrier(fd->comm);
}
