/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_sfs_iread.c,v 1.4 2002/10/24 17:01:01 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_sfs.h"

void ADIOI_SFS_IreadContig(ADIO_File fd, void *buf, int count, 
                MPI_Datatype datatype, int file_ptr_type,
                ADIO_Offset offset, ADIO_Request *request, int *error_code)  
{
    ADIO_Status status;
    int len, typesize;

/* Use blocking I/O for now. */

    *request = ADIOI_Malloc_request();
    (*request)->optype = ADIOI_READ;
    (*request)->fd = fd;
    (*request)->queued = 0;
    (*request)->datatype = datatype;

    MPI_Type_size(datatype, &typesize);
    len = count * typesize;
    ADIOI_SFS_ReadContig(fd, buf, len, MPI_BYTE, file_ptr_type, offset, &status,
		    error_code);  

#ifdef HAVE_STATUS_SET_BYTES
    if (*error_code == MPI_SUCCESS) {
	MPI_Get_elements(&status, MPI_BYTE, &len);
	(*request)->nbytes = len;
    }
#endif
    fd->async_count++;
}



void ADIOI_SFS_IreadStrided(ADIO_File fd, void *buf, int count, 
		       MPI_Datatype datatype, int file_ptr_type,
                       ADIO_Offset offset, ADIO_Request *request, int
                       *error_code)
{
    ADIO_Status status;
#ifdef HAVE_STATUS_SET_BYTES
    int typesize;
#endif

/* Use blocking I/O for now. */

    *request = ADIOI_Malloc_request();
    (*request)->optype = ADIOI_READ;
    (*request)->fd = fd;
    (*request)->queued = 0;
    (*request)->datatype = datatype;

    ADIOI_SFS_ReadStrided(fd, buf, count, datatype, file_ptr_type, 
                            offset, &status, error_code);  

    fd->async_count++;
#ifdef HAVE_STATUS_SET_BYTES
    if (*error_code == MPI_SUCCESS) {
	MPI_Type_size(datatype, &typesize);
	(*request)->nbytes = count * typesize;
    }
#endif
}
