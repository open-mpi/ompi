/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"

/* returns the current location of the shared_fp in terms of the
   no. of etypes relative to the current view, and also increments the
   shared_fp by the number of etypes to be accessed (incr) in the read
   or write following this function. */

void ADIOI_NFS_Get_shared_fp(ADIO_File fd, ADIO_Offset incr, ADIO_Offset *shared_fp,
                             int *error_code);

void ADIO_Get_shared_fp(ADIO_File fd, ADIO_Offset incr, ADIO_Offset *shared_fp,
			 int *error_code)
{
    ADIO_Status status;
    ADIO_Offset new_fp;
    MPI_Comm dupcommself;

    /* Set the shared_fp in case this comes from an uninitialized stack variable
       The read routines will not read into the address of this variable if the file
       size of a shared pointer is 0, and if incr is always zero, this value will remain
       uninitialized.  Initialize it here to prevent incorrect values
    */
    *shared_fp = 0;

#ifdef ROMIO_NFS
    if (fd->file_system == ADIO_NFS) {
	ADIOI_NFS_Get_shared_fp(fd, incr, shared_fp, error_code);
	return;
    }
#endif

    if (fd->shared_fp_fd == ADIO_FILE_NULL) {
	MPI_Comm_dup(MPI_COMM_SELF, &dupcommself);
	fd->shared_fp_fd = ADIO_Open(MPI_COMM_SELF, dupcommself, 
				     fd->shared_fp_fname, 
				     fd->file_system,
				     fd->fns,
				     ADIO_CREATE | ADIO_RDWR | ADIO_DELETE_ON_CLOSE, 
				     0, MPI_BYTE, MPI_BYTE,
				     MPI_INFO_NULL, 
				     ADIO_PERM_NULL, error_code);
	if (*error_code != MPI_SUCCESS) return;
	ADIOI_WRITE_LOCK(fd->shared_fp_fd, 0, SEEK_SET, sizeof(ADIO_Offset));
	ADIO_ReadContig(fd->shared_fp_fd, shared_fp, sizeof(ADIO_Offset), 
		       MPI_BYTE, ADIO_EXPLICIT_OFFSET, 0, &status, error_code);
        /* if the file is empty, the above function may return error
           (reading beyond end of file). In that case, shared_fp = 0, 
           set above, is the correct value. */
    }
    else {
	ADIOI_WRITE_LOCK(fd->shared_fp_fd, 0, SEEK_SET, sizeof(ADIO_Offset));
	ADIO_ReadContig(fd->shared_fp_fd, shared_fp, sizeof(ADIO_Offset), 
		       MPI_BYTE, ADIO_EXPLICIT_OFFSET, 0, &status, error_code);
	if (*error_code != MPI_SUCCESS) {
	    ADIOI_UNLOCK(fd->shared_fp_fd, 0, SEEK_SET, sizeof(ADIO_Offset));
	    return;
	}
    }

    if (incr == 0) {goto done;}

    new_fp = *shared_fp + incr;

    ADIO_WriteContig(fd->shared_fp_fd, &new_fp, sizeof(ADIO_Offset), 
		    MPI_BYTE, ADIO_EXPLICIT_OFFSET, 0, &status, error_code);
done:
    ADIOI_UNLOCK(fd->shared_fp_fd, 0, SEEK_SET, sizeof(ADIO_Offset));
}
