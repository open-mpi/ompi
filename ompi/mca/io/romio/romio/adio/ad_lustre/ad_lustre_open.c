/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 *
 *   Copyright (C) 2007 Oak Ridge National Laboratory
 */

#include "ad_lustre.h"

void ADIOI_LUSTRE_Open(ADIO_File fd, int *error_code)
{
    int perm, old_mask, amode, amode_direct;
    struct lov_user_md lum = { 0 };
    char *value;

#if defined(MPICH2) || !defined(PRINT_ERR_MSG)
    static char myname[] = "ADIOI_LUSTRE_OPEN";
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

    amode_direct = amode | O_DIRECT;

    fd->fd_sys = open(fd->filename, amode|O_CREAT, perm);

    if (fd->fd_sys != -1) {
        int err;

        value = (char *) ADIOI_Malloc((MPI_MAX_INFO_VAL+1)*sizeof(char));

        /* get file striping information and set it in info */
        lum.lmm_magic = LOV_USER_MAGIC;
        err = ioctl(fd->fd_sys, LL_IOC_LOV_GETSTRIPE, (void *) &lum);

        if (!err) {
            sprintf(value, "%d", lum.lmm_stripe_size);
            MPI_Info_set(fd->info, "striping_unit", value);

            sprintf(value, "%d", lum.lmm_stripe_count);
            MPI_Info_set(fd->info, "striping_factor", value);

            sprintf(value, "%d", lum.lmm_stripe_offset);
            MPI_Info_set(fd->info, "start_iodevice", value);
        }
        ADIOI_Free(value);

        if (fd->access_mode & ADIO_APPEND)
            fd->fp_ind = fd->fp_sys_posn = lseek(fd->fd_sys, 0, SEEK_END);
    } 

    if ((fd->fd_sys != -1) && (fd->access_mode & ADIO_APPEND))
	fd->fp_ind = fd->fp_sys_posn = lseek(fd->fd_sys, 0, SEEK_END);

    fd->fd_direct = -1;
    if (fd->direct_write || fd->direct_read) {
	fd->fd_direct = open(fd->filename, amode_direct, perm);
	if (fd->fd_direct != -1) {
	    fd->d_mem = fd->d_miniosz = (1<<12);
	} else {
	    perror("cannot open file with O_Direct");
	    fd->direct_write = fd->direct_read = 0;
	}
    }

    /* --BEGIN ERROR HANDLING-- */
    if (fd->fd_sys == -1 || ((fd->fd_direct == -1) && 
		(fd->direct_write || fd->direct_read))) {
	if (errno == ENAMETOOLONG)
	    *error_code = MPIO_Err_create_code(MPI_SUCCESS,
					       MPIR_ERR_RECOVERABLE, myname,
					       __LINE__, MPI_ERR_BAD_FILE,
					       "**filenamelong",
					       "**filenamelong %s %d",
					       fd->filename,
					       strlen(fd->filename));
	else if (errno == ENOENT)
	    *error_code = MPIO_Err_create_code(MPI_SUCCESS,
					       MPIR_ERR_RECOVERABLE, myname,
					       __LINE__, MPI_ERR_NO_SUCH_FILE,
					       "**filenoexist",
					       "**filenoexist %s",
					       fd->filename);
	else if (errno == ENOTDIR || errno == ELOOP)
	    *error_code = MPIO_Err_create_code(MPI_SUCCESS,
					       MPIR_ERR_RECOVERABLE,
					       myname, __LINE__,
					       MPI_ERR_BAD_FILE,
					       "**filenamedir",
					       "**filenamedir %s",
					       fd->filename);
	else if (errno == EACCES) {
	    *error_code = MPIO_Err_create_code(MPI_SUCCESS,
					       MPIR_ERR_RECOVERABLE, myname,
					       __LINE__, MPI_ERR_ACCESS,
					       "**fileaccess",
					       "**fileaccess %s", 
					       fd->filename );
	}
	else if (errno == EROFS) {
	    /* Read only file or file system and write access requested */
	    *error_code = MPIO_Err_create_code(MPI_SUCCESS,
					       MPIR_ERR_RECOVERABLE, myname,
					       __LINE__, MPI_ERR_READ_ONLY,
					       "**ioneedrd", 0 );
	}
	else {
	    *error_code = MPIO_Err_create_code(MPI_SUCCESS,
					       MPIR_ERR_RECOVERABLE, myname,
					       __LINE__, MPI_ERR_IO, "**io",
					       "**io %s", strerror(errno));
	}
    }
    /* --END ERROR HANDLING-- */
    else *error_code = MPI_SUCCESS;

}
