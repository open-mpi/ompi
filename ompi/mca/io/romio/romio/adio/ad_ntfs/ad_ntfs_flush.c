/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_ntfs_flush.c,v 1.3 2002/10/24 17:00:49 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_ntfs.h"

void ADIOI_NTFS_Flush(ADIO_File fd, int *error_code)
{
    int err;
#ifndef PRINT_ERR_MSG
    static char myname[] = "ADIOI_GEN_FLUSH";
#endif

    err = (fd->access_mode & ADIO_RDONLY) ? TRUE : FlushFileBuffers(fd->fd_sys);

#ifdef PRINT_ERR_MSG
    *error_code = (err == TRUE) ? MPI_SUCCESS : MPI_ERR_UNKNOWN;
#else
    if (err == FALSE) {
	*error_code = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ADIO_ERROR,
			      myname, "I/O Error", "%s", strerror(errno));
	ADIOI_Error(MPI_FILE_NULL, *error_code, myname);	    
    }
    else *error_code = MPI_SUCCESS;
#endif
}
