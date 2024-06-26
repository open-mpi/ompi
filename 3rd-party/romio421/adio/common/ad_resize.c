/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "adio.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

void ADIOI_GEN_Resize(ADIO_File fd, ADIO_Offset size, int *error_code)
{
    int err, rank;
    static char myname[] = "ADIOI_GEN_RESIZE";

    MPI_Comm_rank(fd->comm, &rank);

    /* first aggregator performs ftruncate() */
    if (rank == fd->hints->ranklist[0]) {
        ADIOI_Assert(size == (off_t) size);
        err = ftruncate(fd->fd_sys, (off_t) size);
        if (err == -1) {
            /* detected an error, capture value of errno */
            err = errno;
        }
    }

    /* bcast success/errno value */
    MPI_Bcast(&err, 1, MPI_INT, fd->hints->ranklist[0], fd->comm);

    /* --BEGIN ERROR HANDLING-- */
    if (err != 0) {
        /* when err is not 0, it contains the errno value from ftruncate */
        *error_code = ADIOI_Err_create_code(myname, fd->filename, err);
        return;
    }
    /* --END ERROR HANDLING-- */

    *error_code = MPI_SUCCESS;
}
