/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_xfs_open.c,v 1.6 2002/10/24 17:01:10 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_xfs.h"

void ADIOI_XFS_Open(ADIO_File fd, int *error_code)
{
    int perm, old_mask, amode, amode_direct;
    struct dioattr st;
#ifndef PRINT_ERR_MSG
    static char myname[] = "ADIOI_XFS_OPEN";
#endif

    if (fd->perm == ADIO_PERM_NULL) {
	old_mask = umask(022);
	umask(old_mask);
	perm = old_mask ^ 0666;
    }
    else perm = fd->perm;

    amode = 0;
    if (fd->access_mode & ADIO_CREATE)
	amode = amode | O_CREAT;
    if (fd->access_mode & ADIO_RDONLY)
	amode = amode | O_RDONLY;
    if (fd->access_mode & ADIO_WRONLY)
	amode = amode | O_WRONLY;
    if (fd->access_mode & ADIO_RDWR)
	amode = amode | O_RDWR;

    amode_direct = amode | O_DIRECT;

    if (fd->access_mode & ADIO_EXCL)
	amode = amode | O_EXCL;

    fd->fd_sys = open(fd->filename, amode, perm);

    fd->fd_direct = open(fd->filename, amode_direct, perm);
    if (fd->fd_direct != -1) {
	fcntl(fd->fd_direct, F_DIOINFO, &st);
	fd->d_mem = st.d_mem;
	fd->d_miniosz = st.d_miniosz;
	fd->d_maxiosz = st.d_maxiosz;
    }

    if ((fd->fd_sys != -1) && (fd->access_mode & ADIO_APPEND))
	fd->fp_ind = lseek64(fd->fd_sys, 0, SEEK_END);

    fd->fp_sys_posn = -1; /* set it to null because we use pread/pwrite */

#ifdef PRINT_ERR_MSG
    *error_code = ((fd->fd_sys == -1) || (fd->fd_direct == -1)) ? 
	             MPI_ERR_UNKNOWN : MPI_SUCCESS;
#else
    if ((fd->fd_sys == -1) || (fd->fd_direct == -1)) {
	*error_code = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ADIO_ERROR,
			      myname, "I/O Error", "%s", strerror(errno));
	ADIOI_Error(ADIO_FILE_NULL, *error_code, myname);	    
    }
    else *error_code = MPI_SUCCESS;
#endif
}
