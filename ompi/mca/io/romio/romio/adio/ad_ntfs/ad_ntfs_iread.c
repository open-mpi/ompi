/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_ntfs_iread.c,v 1.3 2002/11/13 13:30:35 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

/* Set the style to c++ since this code will only be compiled with the
   Windows C/C++ compiler that accepts C++ style comments and other 
   constructions */
/* style:c++ */

#include "ad_ntfs.h"

void ADIOI_NTFS_IreadContig(ADIO_File fd, void *buf, int count, 
                MPI_Datatype datatype, int file_ptr_type,
                ADIO_Offset offset, ADIO_Request *request, int *error_code)  
{
    int len, typesize;
    int err=FALSE;
#ifndef PRINT_ERR_MSG
    static char myname[] = "ADIOI_NTFS_IREADCONTIG";
#endif

    (*request) = ADIOI_Malloc_request();
    (*request)->optype = ADIOI_READ;
    (*request)->fd = fd;
    (*request)->datatype = datatype;

    MPI_Type_size(datatype, &typesize);
    len = count * typesize;

    if (file_ptr_type == ADIO_INDIVIDUAL) offset = fd->fp_ind;
    err = ADIOI_NTFS_aio(fd, buf, len, offset, 0, &((*request)->handle));
    if (file_ptr_type == ADIO_INDIVIDUAL) fd->fp_ind += len;

    (*request)->queued = 1;
    ADIOI_Add_req_to_list(request);

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

    fd->fp_sys_posn = -1;   /* set it to null. */
    fd->async_count++;
}



void ADIOI_NTFS_IreadStrided(ADIO_File fd, void *buf, int count, 
		       MPI_Datatype datatype, int file_ptr_type,
                       ADIO_Offset offset, ADIO_Request *request, int
                       *error_code)
{
    ADIO_Status status;
#ifdef HAVE_STATUS_SET_BYTES
    int typesize;
#endif

    *request = ADIOI_Malloc_request();
    (*request)->optype = ADIOI_READ;
    (*request)->fd = fd;
    (*request)->datatype = datatype;
    (*request)->queued = 0;
    (*request)->handle = 0;

/* call the blocking version. It is faster because it does data sieving. */
    ADIOI_NTFS_ReadStrided(fd, buf, count, datatype, file_ptr_type, 
                            offset, &status, error_code);  

    fd->async_count++;

#ifdef HAVE_STATUS_SET_BYTES
    if (*error_code == MPI_SUCCESS) {
	MPI_Type_size(datatype, &typesize);
	(*request)->nbytes = count * typesize;
    }
#endif
}
