/* ---------------------------------------------------------------- */
/* (C)Copyright IBM Corp.  2007, 2008                               */
/* ---------------------------------------------------------------- */
/**
 * \file ad_bgl_open.c
 * \brief ???
 */

/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_bgl.h"
#include "ad_bgl_aggrs.h"

void ADIOI_BGL_Open(ADIO_File fd, int *error_code)
{
    int perm, old_mask, amode;
    static char myname[] = "ADIOI_BGL_OPEN";

    /* set internal variables for tuning environment variables */
    ad_bgl_get_env_vars();		

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

    fd->fd_sys = open(fd->filename, amode, perm);
    fd->fd_direct = -1;

    if ((fd->fd_sys != -1) && (fd->access_mode & ADIO_APPEND))
	fd->fp_ind = fd->fp_sys_posn = lseek(fd->fd_sys, 0, SEEK_END);

    if(fd->fd_sys != -1)
    {
      struct stat64 bgl_stat;
      int rc = stat64(fd->filename,&bgl_stat);
      if (rc >= 0)
      {
        /* store the blksize in the file system specific storage */
        AD_BGL_assert(fd->fs_ptr == NULL);
        fd->fs_ptr = (ADIOI_BGL_fs*) ADIOI_Malloc(sizeof(ADIOI_BGL_fs));
        ((ADIOI_BGL_fs*)fd->fs_ptr)->blksize = bgl_stat.st_blksize;
/*        FPRINTF(stderr,"%s(%d):Successful stat '%s'.  Blocksize=%ld\n",myname,__LINE__,fd->filename,bgl_stat.st_blksize);*/
      }
/*      else
        FPRINTF(stderr,"%s(%d):Stat '%s' failed with rc=%d, errno=%d\n",myname,__LINE__,fd->filename,rc,errno);*/
    }

    if (fd->fd_sys == -1) {
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
    else *error_code = MPI_SUCCESS;
}
