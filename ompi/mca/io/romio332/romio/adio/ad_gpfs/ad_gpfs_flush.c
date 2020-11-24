/* ---------------------------------------------------------------- */
/* (C)Copyright IBM Corp.  2007, 2008                               */
/* ---------------------------------------------------------------- */
/**
 * \file ad_gpfs_flush.c
 * \brief Scalable flush for GPFS
 */

/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_gpfs.h"

void ADIOI_GPFS_Flush(ADIO_File fd, int *error_code)
{
    int err=0;
    static char myname[] = "ADIOI_GPFS_FLUSH";

    int rank;

    MPI_Comm_rank(fd->comm, &rank);

    /* the old logic about who is an fsync aggregator and who is not fell down
     * when deferred open was enabled.  Instead, make this look more like
     * ad_pvfs2_flush.  If one day the I/O aggregators have something they need
     * to flush, we can consult the 'fd->hints->ranklist[]' array.  For now, a
     * flush from one process should suffice */

    /* ensure all other proceses are done writing. On many platforms MPI_Reduce
     * is fastest because it has the lightest constraints. On Blue Gene, BARRIER
     * is optimized  */
    MPI_Barrier(fd->comm);

    if (rank == fd->hints->ranklist[0]) {
	err = fsync(fd->fd_sys);
	DBG_FPRINTF(stderr,"aggregation:fsync %s, err=%#X, errno=%#X\n",fd->filename, err, errno);
	/* We want errno, not the return code if it failed */
	if (err == -1) err = errno;
	else err = 0;
    }
    MPI_Bcast(&err, 1, MPI_UNSIGNED, fd->hints->ranklist[0], fd->comm);
    DBGV_FPRINTF(stderr,"aggregation result:fsync %s, errno %#X,\n",fd->filename, err);

    if (err) /* if it's non-zero, it must be an errno */
    {
	errno = err;
	err = -1;
    }

    /* --BEGIN ERROR HANDLING-- */
    if (err == -1)
    {
	*error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
		myname, __LINE__, MPI_ERR_IO,
		"**io",
		"**io %s", strerror(errno));
	DBGT_FPRINTF(stderr,"fsync %s, err=%#X, errno=%#X\n",fd->filename, err, errno);
	return;
    }
    /* --END ERROR HANDLING-- */

    *error_code = MPI_SUCCESS;
}

