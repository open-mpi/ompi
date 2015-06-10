/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 2004 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"

#include "mpiu_greq.h"

/* Generic implementation of IwriteContig calls the blocking WriteContig
 * immediately.
 */
void ADIOI_FAKE_IwriteContig(ADIO_File fd, const void *buf, int count,
			    MPI_Datatype datatype, int file_ptr_type,
			    ADIO_Offset offset, ADIO_Request *request,
			    int *error_code)
{
    ADIO_Status status;
    MPI_Offset len;
    MPI_Count typesize;
    MPI_Offset nbytes=0;

    MPI_Type_size_x(datatype, &typesize);
    len = (MPI_Offset)count * (MPI_Offset)typesize;

    /* Call the blocking function.  It will create an error code
     * if necessary.
     */
    ADIOI_Assert(len == (int) len); /* the count is an int parm */
    ADIO_WriteContig(fd, buf, (int)len, MPI_BYTE, file_ptr_type, offset,
		     &status, error_code);  
    if (*error_code == MPI_SUCCESS) {
	MPI_Type_size_x(datatype, &typesize);
	nbytes = (MPI_Offset)count*(MPI_Offset)typesize;
    }
    MPIO_Completed_request_create(&fd, nbytes, error_code, request);

}


/* Generic implementation of IwriteStrided calls the blocking WriteStrided
 * immediately.
 */
void ADIOI_FAKE_IwriteStrided(ADIO_File fd, const void *buf, int count,
                             MPI_Datatype datatype, int file_ptr_type,
			     ADIO_Offset offset, ADIO_Request *request,
			     int *error_code)
{
    ADIO_Status status;
    MPI_Count typesize;
    MPI_Offset nbytes=0;

    /* Call the blocking function.  It will create an error code 
     * if necessary.
     */
    ADIO_WriteStrided(fd, buf, count, datatype, file_ptr_type, 
		      offset, &status, error_code);  
    if (*error_code == MPI_SUCCESS) {
	MPI_Type_size_x(datatype, &typesize);
	nbytes = (MPI_Offset)count * (MPI_Offset)typesize;
    }
    MPIO_Completed_request_create(&fd, nbytes, error_code, request);
}
