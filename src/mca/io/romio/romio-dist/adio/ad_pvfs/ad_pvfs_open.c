/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_pvfs_open.c,v 1.11 2002/10/24 17:00:58 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_pvfs.h"
#include "pvfs_config.h"

void ADIOI_PVFS_Open(ADIO_File fd, int *error_code)
{
    int perm, amode, old_mask, flag;
    char *value;
    struct pvfs_filestat pstat = {-1,-1,-1,0,0};
#ifndef PRINT_ERR_MSG
    static char myname[] = "ADIOI_PVFS_OPEN";
#endif

    if (fd->perm == ADIO_PERM_NULL) {
	old_mask = umask(022);
	umask(old_mask);
	perm = old_mask ^ 0666;
    }
    else perm = fd->perm;

    amode = O_META;
    if (fd->access_mode & ADIO_CREATE)
	amode = amode | O_CREAT;
    if (fd->access_mode & ADIO_RDONLY)
	amode = amode | O_RDONLY;
    if (fd->access_mode & ADIO_WRONLY)
	amode = amode | O_WRONLY;
    if (fd->access_mode & ADIO_RDWR)
	amode = amode | O_RDWR;
    if (fd->access_mode & ADIO_EXCL)
	amode = amode | O_EXCL;

    value = (char *) ADIOI_Malloc((MPI_MAX_INFO_VAL+1)*sizeof(char));

    MPI_Info_get(fd->info, "striping_factor", MPI_MAX_INFO_VAL, 
                         value, &flag);
    if (flag && (atoi(value) > 0)) pstat.pcount = atoi(value);

    MPI_Info_get(fd->info, "striping_unit", MPI_MAX_INFO_VAL, 
                         value, &flag);
    if (flag && (atoi(value) > 0)) pstat.ssize = atoi(value);

    MPI_Info_get(fd->info, "start_iodevice", MPI_MAX_INFO_VAL, 
                         value, &flag);
    if (flag && (atoi(value) >= 0)) pstat.base = atoi(value);

    fd->fd_sys = pvfs_open64(fd->filename, amode, perm, &pstat, NULL);

    if ((fd->fd_sys != -1) && (fd->access_mode & ADIO_APPEND))
	fd->fp_ind = fd->fp_sys_posn = pvfs_lseek64(fd->fd_sys, 0, SEEK_END);

    if (fd->fd_sys != -1) {
	pvfs_ioctl(fd->fd_sys, GETMETA, &pstat);
	sprintf(value, "%d", pstat.pcount);
	MPI_Info_set(fd->info, "striping_factor", value);
	sprintf(value, "%d", pstat.ssize);
	MPI_Info_set(fd->info, "striping_unit", value);
	sprintf(value, "%d", pstat.base);
	MPI_Info_set(fd->info, "start_iodevice", value);
    }

    ADIOI_Free(value);

#ifdef PRINT_ERR_MSG
    *error_code = (fd->fd_sys == -1) ? MPI_ERR_UNKNOWN : MPI_SUCCESS;
#else
    if (fd->fd_sys == -1) {
	*error_code = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ADIO_ERROR,
			      myname, "I/O Error", "%s", strerror(errno));
	ADIOI_Error(ADIO_FILE_NULL, *error_code, myname);	    
    }
    else *error_code = MPI_SUCCESS;
#endif
}
