/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_set_sh_fp.c,v 1.5 2002/10/24 17:01:13 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"

/* set the shared file pointer to "offset" etypes relative to the current 
   view */

void ADIO_Set_shared_fp(ADIO_File fd, ADIO_Offset offset, int *error_code)
{
    ADIO_Status status;
    MPI_Comm dupcommself;

#ifdef NFS
    if (fd->file_system == ADIO_NFS) {
	ADIOI_NFS_Set_shared_fp(fd, offset, error_code);
	return;
    }
#endif

    if (fd->shared_fp_fd == ADIO_FILE_NULL) {
	MPI_Comm_dup(MPI_COMM_SELF, &dupcommself);
	fd->shared_fp_fd = ADIO_Open(MPI_COMM_SELF, dupcommself, 
				     fd->shared_fp_fname, 
				     fd->file_system,
				     ADIO_CREATE | ADIO_RDWR | ADIO_DELETE_ON_CLOSE, 
				     0, MPI_BYTE, MPI_BYTE, M_ASYNC, 
				     MPI_INFO_NULL, 
				     ADIO_PERM_NULL, error_code);
    }

    if (*error_code != MPI_SUCCESS) return;

    ADIOI_WRITE_LOCK(fd->shared_fp_fd, 0, SEEK_SET, sizeof(ADIO_Offset));
    ADIO_WriteContig(fd->shared_fp_fd, &offset, sizeof(ADIO_Offset), 
		     MPI_BYTE, ADIO_EXPLICIT_OFFSET, 0, &status, error_code);
    ADIOI_UNLOCK(fd->shared_fp_fd, 0, SEEK_SET, sizeof(ADIO_Offset));
}

