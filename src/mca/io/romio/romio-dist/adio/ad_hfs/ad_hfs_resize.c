/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_hfs_resize.c,v 1.5 2002/10/24 17:00:44 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_hfs.h"

void ADIOI_HFS_Resize(ADIO_File fd, ADIO_Offset size, int *error_code)
{
    int err;
#ifndef PRINT_ERR_MSG
    static char myname[] = "ADIOI_HFS_RESIZE";
#endif
    
    err = ftruncate64(fd->fd_sys, size);
#ifdef PRINT_ERR_MSG
    *error_code = (err == 0) ? MPI_SUCCESS : MPI_ERR_UNKNOWN;
#else
    if (err == -1) {
	*error_code = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ADIO_ERROR,
			      myname, "I/O Error", "%s", strerror(errno));
	ADIOI_Error(fd, *error_code, myname);	    
    }
    else *error_code = MPI_SUCCESS;
#endif
}
