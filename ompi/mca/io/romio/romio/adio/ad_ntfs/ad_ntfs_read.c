/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_ntfs_read.c,v 1.4 2002/11/13 13:30:36 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

/* Set the style to c++ since this code will only be compiled with the
   Windows C/C++ compiler that accepts C++ style comments and other 
   constructions */
/* style:c++ header */

#include "ad_ntfs.h"

void ADIOI_NTFS_ReadContig(ADIO_File fd, void *buf, int count, 
                     MPI_Datatype datatype, int file_ptr_type,
		     ADIO_Offset offset, ADIO_Status *status, int *error_code)
{
	//int rank;
	DWORD dwTemp;
	DWORD dwNumRead = 0;
    int err=-1, datatype_size, len;
#ifndef PRINT_ERR_MSG
    static char myname[] = "ADIOI_NTFS_READCONTIG";
#endif

    MPI_Type_size(datatype, &datatype_size);
    len = datatype_size * count;

    if (file_ptr_type == ADIO_EXPLICIT_OFFSET) {
	if (fd->fp_sys_posn != offset)
	{
		dwTemp = DWORDHIGH(offset);
		SetFilePointer(fd->fd_sys, DWORDLOW(offset), &dwTemp, FILE_BEGIN);
	}
	err = ReadFile(fd->fd_sys, buf, len, &dwNumRead, NULL);
	//MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	//printf("[%d]R(%d,%d)\n", rank, DWORDLOW(offset), dwNumRead);
	fd->fp_sys_posn = offset + (ADIO_Offset)dwNumRead;
	/* individual file pointer not updated */        
    }
    else {  /* read from curr. location of ind. file pointer */
	if (fd->fp_sys_posn != fd->fp_ind)
	{
		dwTemp = DWORDHIGH(fd->fp_ind);
		SetFilePointer(fd->fd_sys, DWORDLOW(fd->fp_ind), &dwTemp, FILE_BEGIN);
	}
	err = ReadFile(fd->fd_sys, buf, len, &dwNumRead, NULL);
	//MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	//printf("[%d]r(%d,%d)\n", rank, DWORDLOW(offset), dwNumRead);
	fd->fp_ind = fd->fp_ind + (ADIO_Offset)dwNumRead; 
	fd->fp_sys_posn = fd->fp_ind;
    }         

#ifdef HAVE_STATUS_SET_BYTES
    if (err != FALSE) MPIR_Status_set_bytes(status, datatype, dwNumRead);
#endif

#ifdef PRINT_ERR_MSG
    *error_code = (err == FALSE) ? MPI_ERR_UNKNOWN : MPI_SUCCESS;
#else
    if (err == FALSE) {
	*error_code = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ADIO_ERROR,
			      myname, "I/O Error", "%s", strerror(errno));
	ADIOI_Error(fd, *error_code, myname);	    
    }
    else *error_code = MPI_SUCCESS;
#endif
}




void ADIOI_NTFS_ReadStrided(ADIO_File fd, void *buf, int count,
                       MPI_Datatype datatype, int file_ptr_type,
                       ADIO_Offset offset, ADIO_Status *status, int
                       *error_code)
{
    ADIOI_GEN_ReadStrided(fd, buf, count, datatype, file_ptr_type,
                        offset, status, error_code);
}
