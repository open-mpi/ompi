/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_pfs_open.c,v 1.6 2002/10/24 17:00:52 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_pfs.h"
#ifdef PROFILE
#include "mpe.h"
#endif

void ADIOI_PFS_Open(ADIO_File fd, int *error_code)
{
    int perm, amode, old_mask, np_comm, np_total, err, flag;
    char *value;
    struct sattr attr;
#ifndef PRINT_ERR_MSG
    static char myname[] = "ADIOI_PFS_OPEN";
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
    if (fd->access_mode & ADIO_EXCL)
	amode = amode | O_EXCL;

    MPI_Comm_size(MPI_COMM_WORLD, &np_total);
    MPI_Comm_size(fd->comm, &np_comm);

#ifdef PROFILE
    MPE_Log_event(1, 0, "start open");
#endif
    if (np_total == np_comm) 
	fd->fd_sys = _gopen(fd->filename,amode,fd->iomode,perm);
    else fd->fd_sys = open(fd->filename, amode, perm);
#ifdef PROFILE
    MPE_Log_event(2, 0, "end open");
#endif

    if (fd->fd_sys != -1) {
	value = (char *) ADIOI_Malloc((MPI_MAX_INFO_VAL+1)*sizeof(char));

        /* if user has asked for pfs server buffering to be turned on,
           it will be set to true in fd->info in the earlier call
           to ADIOI_PFS_SetInfo. Turn it on now, since we now have a 
           valid file descriptor. */

	MPI_Info_get(fd->info, "pfs_svr_buf", MPI_MAX_INFO_VAL, 
		     value, &flag);
	if (flag && (!strcmp(value, "true"))) {
	    err = fcntl(fd->fd_sys, F_PFS_SVR_BUF, TRUE);
	    if (err) MPI_Info_set(fd->info, "pfs_svr_buf", "false");
	}

        /* get file striping information and set it in info */
	err = fcntl(fd->fd_sys, F_GETSATTR, &attr);

	if (!err) {
	    sprintf(value, "%d", attr.s_sunitsize);
	    MPI_Info_set(fd->info, "striping_unit", value);

	    sprintf(value, "%d", attr.s_sfactor);
	    MPI_Info_set(fd->info, "striping_factor", value);

	    sprintf(value, "%d", attr.s_start_sdir);
	    MPI_Info_set(fd->info, "start_iodevice", value);
	}
	ADIOI_Free(value);

	if (fd->access_mode & ADIO_APPEND) 
	    fd->fp_ind = fd->fp_sys_posn = lseek(fd->fd_sys, 0, SEEK_END);
    }

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
