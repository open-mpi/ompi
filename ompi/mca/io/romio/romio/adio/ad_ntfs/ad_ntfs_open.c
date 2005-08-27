/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

/* Set the style to c++ since this code will only be compiled with the
   Windows C/C++ compiler that accepts C++ style comments and other 
   constructions */
/* style:c++ header */

#include "ad_ntfs.h"

void ADIOI_NTFS_Open(ADIO_File fd, int *error_code)
{
    int cmode, amode, smode;
#ifndef PRINT_ERR_MSG
    static char myname[] = "ADIOI_NTFS_OPEN";
#endif

    amode = 0;
    cmode = OPEN_EXISTING;
    smode = 0;
    if (fd->access_mode & ADIO_CREATE)
	cmode = OPEN_ALWAYS; //CREATE_ALWAYS;
    if (fd->access_mode & ADIO_EXCL)
	cmode = CREATE_NEW;

    if (fd->access_mode & ADIO_RDONLY)
    {
	amode = amode | FILE_SHARE_READ;
	smode = smode | GENERIC_READ;
    }
    if (fd->access_mode & ADIO_WRONLY)
    {
	amode = amode | FILE_SHARE_WRITE;
	smode = smode | GENERIC_WRITE;
    }
    if (fd->access_mode & ADIO_RDWR)
    {
	amode = amode | FILE_SHARE_READ | FILE_SHARE_WRITE;
	smode = smode | GENERIC_READ | GENERIC_WRITE;
    }

	fd->fd_sys = CreateFile(
		fd->filename, 
		//smode,
		GENERIC_READ | GENERIC_WRITE,
		amode, 
		NULL, 
		cmode, 
		FILE_ATTRIBUTE_NORMAL, 
		NULL);

    if ((fd->fd_sys != INVALID_HANDLE_VALUE) && (fd->access_mode & ADIO_APPEND))
		fd->fp_ind = fd->fp_sys_posn = SetFilePointer(fd->fd_sys, 0, NULL, FILE_END);

#ifdef PRINT_ERR_MSG
    *error_code = (fd->fd_sys == INVALID_HANDLE_VALUE) ? MPI_ERR_UNKNOWN : MPI_SUCCESS;
    FPRINTF(stderr, "MPI_NTFS_File_open: Error %d opening file %s\n", GetLastError(), fd->filename);
#else
    if (fd->fd_sys == INVALID_HANDLE_VALUE) {
	*error_code = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ADIO_ERROR,
			      myname, "I/O Error", "%s", strerror(errno));
	ADIOI_Error(ADIO_FILE_NULL, *error_code, myname);	    
    }
    else *error_code = MPI_SUCCESS;
#endif
}

/*
void ADIOI_NTFS_Open(ADIO_File fd, int *error_code)
{
    int cmode, amode;
#ifndef PRINT_ERR_MSG
    static char myname[] = "ADIOI_NTFS_OPEN";
#endif

    amode = 0;
	cmode = 0;
    if (fd->access_mode & ADIO_CREATE)
	cmode = OPEN_ALWAYS; //CREATE_ALWAYS;
    if (fd->access_mode & ADIO_EXCL)
	cmode = CREATE_NEW;

    if (fd->access_mode & ADIO_RDONLY)
	amode = amode | FILE_SHARE_READ;
    if (fd->access_mode & ADIO_WRONLY)
	amode = amode | FILE_SHARE_WRITE;
    if (fd->access_mode & ADIO_RDWR)
	amode = amode | FILE_SHARE_READ | FILE_SHARE_WRITE;

	fd->fd_sys = CreateFile(
		fd->filename, 
		GENERIC_READ | GENERIC_WRITE,
		amode, 
		NULL, 
		cmode, 
		FILE_ATTRIBUTE_NORMAL, 
		NULL);

    if ((fd->fd_sys != INVALID_HANDLE_VALUE) && (fd->access_mode & ADIO_APPEND))
		fd->fp_ind = fd->fp_sys_posn = SetFilePointer(fd->fd_sys, 0, NULL, FILE_END);

#ifdef PRINT_ERR_MSG
    *error_code = (fd->fd_sys == INVALID_HANDLE_VALUE) ? MPI_ERR_UNKNOWN : MPI_SUCCESS;
#else
    if (fd->fd_sys == INVALID_HANDLE_VALUE) {
	*error_code = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ADIO_ERROR,
			      myname, "I/O Error", "%s", strerror(errno));
	ADIOI_Error(ADIO_FILE_NULL, *error_code, myname);	    
    }
    else *error_code = MPI_SUCCESS;
#endif
}
*/
