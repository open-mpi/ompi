/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 2004 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
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
	err = ftruncate(fd->fd_sys, (off_t)size);
    }

    /* bcast return value */
    MPI_Bcast(&err, 1, MPI_INT, fd->hints->ranklist[0], fd->comm);

    /* --BEGIN ERROR HANDLING-- */
    if (err == -1) {
	*error_code = ADIOI_Err_create_code(myname, fd->filename, errno);
	return;
    }
    /* --END ERROR HANDLING-- */

    *error_code = MPI_SUCCESS;
}
