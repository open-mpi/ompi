/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_ntfs_resize.c,v 1.2 2002/10/24 17:00:49 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_ntfs.h"

void ADIOI_NTFS_Resize(ADIO_File fd, ADIO_Offset size, int *error_code)
{
	DWORD dwTemp;
    int err;
#ifndef PRINT_ERR_MSG
    static char myname[] = "ADIOI_NTFS_RESIZE";
#endif

	dwTemp = DWORDHIGH(size);
	err = SetFilePointer(fd->fd_sys, DWORDLOW(size), &dwTemp, FILE_BEGIN);
    err = SetEndOfFile(fd->fd_sys);
#ifdef PRINT_ERR_MSG
    *error_code = (err == TRUE) ? MPI_SUCCESS : MPI_ERR_UNKNOWN;
#else
    if (err == FALSE) {
	*error_code = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ADIO_ERROR,
			      myname, "I/O Error", "%s", strerror(errno));
	ADIOI_Error(fd, *error_code, myname);	    
    }
    else *error_code = MPI_SUCCESS;
#endif
}
