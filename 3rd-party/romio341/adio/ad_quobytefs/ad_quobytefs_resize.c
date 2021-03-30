/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */


#include "adio.h"

#include "ad_quobytefs.h"

void ADIOI_QUOBYTEFS_Resize(ADIO_File fd, ADIO_Offset size, int *error_code)
{
    static char myname[] = "ADIOI_QUOBYTEFS_RESIZE";
    struct quobyte_fh *file_handle = fd->file_handle;
    int err, rank;

    if (file_handle != NULL) {

        MPI_Comm_rank(fd->comm, &rank);
        /* first aggregator performs ftruncate() */
        if (rank == fd->hints->ranklist[0]) {
            ADIOI_Assert(size == (off_t) size);
            err = quobyte_ftruncate(fd->file_handle, (off_t) size);
        }

        /* bcast return value */
        MPI_Bcast(&err, 1, MPI_INT, fd->hints->ranklist[0], fd->comm);
        if (err == -1) {
            *error_code = ADIOI_Err_create_code(myname, fd->filename, errno);
            return;
        }
        *error_code = MPI_SUCCESS;
    } else {
        *error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, myname,
                                           __LINE__, MPI_ERR_IO, "Quobyte file header is null", 0);
        return;
    }
}
