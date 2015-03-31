/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_nfs.h"

/* set the shared file pointer to "offset" etypes relative to the current 
   view */

/*
This looks very similar to ADIOI_GEN_Set_shared_fp, except this 
function avoids locking the file twice.  The generic version does

Write lock
ADIO_WriteContig
Unlock

For NFS, ADIOI_NFS_WriteContig does a lock before writing to disable
caching. To avoid the lock being called twice, this version for NFS does

Write lock
Lseek
Write
Unlock 

*/

void ADIOI_NFS_Set_shared_fp(ADIO_File fd, ADIO_Offset offset, int *error_code)
{
    ssize_t err;
    MPI_Comm dupcommself;
    static char myname[] = "ADIOI_NFS_SET_SHARED_FP";

    if (fd->shared_fp_fd == ADIO_FILE_NULL) {
	MPI_Comm_dup(MPI_COMM_SELF, &dupcommself);
	fd->shared_fp_fd = ADIO_Open(MPI_COMM_SELF, dupcommself,
				     fd->shared_fp_fname, 
				     fd->file_system, fd->fns,
				     ADIO_CREATE | ADIO_RDWR | ADIO_DELETE_ON_CLOSE, 
				     0, MPI_BYTE, MPI_BYTE, MPI_INFO_NULL, 
				     ADIO_PERM_NULL, error_code);
    }

    if (*error_code != MPI_SUCCESS) return;

    ADIOI_WRITE_LOCK(fd->shared_fp_fd, 0, SEEK_SET, sizeof(ADIO_Offset));
#ifdef ADIOI_MPE_LOGGING
    MPE_Log_event( ADIOI_MPE_lseek_a, 0, NULL );
#endif
    lseek(fd->shared_fp_fd->fd_sys, 0, SEEK_SET);
#ifdef ADIOI_MPE_LOGGING
    MPE_Log_event( ADIOI_MPE_lseek_b, 0, NULL );
#endif
#ifdef ADIOI_MPE_LOGGING
    MPE_Log_event( ADIOI_MPE_write_a, 0, NULL );
#endif
    err = write(fd->shared_fp_fd->fd_sys, &offset, sizeof(ADIO_Offset));
#ifdef ADIOI_MPE_LOGGING
    MPE_Log_event( ADIOI_MPE_write_b, 0, NULL );
#endif
    ADIOI_UNLOCK(fd->shared_fp_fd, 0, SEEK_SET, sizeof(ADIO_Offset));

    if (err == -1) {
	*error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					   myname, __LINE__, MPI_ERR_IO,
					   "**io",
					   "**io %s", strerror(errno));
    }
    else *error_code = MPI_SUCCESS;
}

