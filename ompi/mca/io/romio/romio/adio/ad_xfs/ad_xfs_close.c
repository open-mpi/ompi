/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_xfs_close.c,v 1.6 2002/10/24 17:01:09 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_xfs.h"

void ADIOI_XFS_Close(ADIO_File fd, int *error_code)
{
    int err, err1;
#ifndef PRINT_ERR_MSG
    static char myname[] = "ADIOI_XFS_CLOSE";
#endif

    err = close(fd->fd_sys);
    err1 = close(fd->fd_direct);

#ifdef PRINT_ERR_MSG
    *error_code = ((err == 0) && (err1 == 0)) ? MPI_SUCCESS : MPI_ERR_UNKNOWN;
#else
    if ((err == -1) || (err1 == -1)) {
	*error_code = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ADIO_ERROR,
			      myname, "I/O Error", "%s", strerror(errno));
	ADIOI_Error(fd, *error_code, myname);	    
    }
    else *error_code = MPI_SUCCESS;
#endif
}
