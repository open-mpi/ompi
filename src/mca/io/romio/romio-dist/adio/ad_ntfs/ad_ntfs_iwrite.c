/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_ntfs_iwrite.c,v 1.4 2002/11/15 16:26:22 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

/* Set the style to c++ since this code will only be compiled with the
   Windows C/C++ compiler that accepts C++ style comments and other 
   constructions */
/* style:c++ header */

#include "ad_ntfs.h"

void ADIOI_NTFS_IwriteContig(ADIO_File fd, void *buf, int count, 
                MPI_Datatype datatype, int file_ptr_type,
                ADIO_Offset offset, ADIO_Request *request, int *error_code)  
{
    int len, typesize;
    int err=FALSE;
#ifndef PRINT_ERR_MSG
    static char myname[] = "ADIOI_NTFS_IWRITECONTIG";
#endif

    *request = ADIOI_Malloc_request();
    (*request)->optype = ADIOI_WRITE;
    (*request)->fd = fd;
    (*request)->datatype = datatype;

    MPI_Type_size(datatype, &typesize);
    len = count * typesize;

    if (file_ptr_type == ADIO_INDIVIDUAL) offset = fd->fp_ind;
    err = ADIOI_NTFS_aio(fd, buf, len, offset, 1, &((*request)->handle));
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




void ADIOI_NTFS_IwriteStrided(ADIO_File fd, void *buf, int count, 
		       MPI_Datatype datatype, int file_ptr_type,
                       ADIO_Offset offset, ADIO_Request *request, int
                       *error_code)
{
    ADIO_Status status;
#ifdef HAVE_STATUS_SET_BYTES
    int typesize;
#endif

    *request = ADIOI_Malloc_request();
    (*request)->optype = ADIOI_WRITE;
    (*request)->fd = fd;
    (*request)->datatype = datatype;
    (*request)->queued = 0;
    (*request)->handle = 0;

/* call the blocking version. It is faster because it does data sieving. */
    ADIOI_NTFS_WriteStrided(fd, buf, count, datatype, file_ptr_type, 
                            offset, &status, error_code);  

    fd->async_count++;

#ifdef HAVE_STATUS_SET_BYTES
    if (*error_code == MPI_SUCCESS) {
	MPI_Type_size(datatype, &typesize);
	(*request)->nbytes = count * typesize;
    }
#endif
}


/* This function is for implementation convenience. It is not user-visible.
   It takes care of the differences in the interface for nonblocking I/O
   on various Unix machines! If wr==1 write, wr==0 read. */

int ADIOI_NTFS_aio(ADIO_File fd, void *buf, int len, ADIO_Offset offset,
		  int wr, void *handle)
{
	DWORD dwNumWritten=0, dwNumRead=0;
	BOOL ret_val = FALSE;
	FDTYPE fd_sys;

	OVERLAPPED *pOvl;

    fd_sys = fd->fd_sys;

    pOvl = (OVERLAPPED *) ADIOI_Calloc(sizeof(OVERLAPPED), 1);
	pOvl->hEvent = CreateEvent(NULL, TRUE, TRUE, NULL);
	pOvl->Offset = DWORDLOW(offset);
	pOvl->OffsetHigh = DWORDHIGH(offset);

	if (wr)
	{
		ret_val = WriteFile(fd_sys, buf, len, &dwNumWritten, pOvl);
		//ret_val = WriteFile(fd_sys, buf, len, &dwNumWritten, NULL);
		//if (ret_val && dwNumWritten) printf("written immediately: %d\n", dwNumWritten);
	}
	else
	{
		ret_val = ReadFile(fd_sys, buf, len, &dwNumRead, pOvl);
		//ret_val = ReadFile(fd_sys, buf, len, &dwNumRead, NULL);
	}

    if (ret_val == FALSE) 
	{
		errno = GetLastError();
		if (errno != ERROR_IO_PENDING)
		{
			if (wr)
				FPRINTF(stderr, "WriteFile error: len %d, dwNumWritten %d\n", len, dwNumWritten);
			else
				FPRINTF(stderr, "ReadFile error: len %d, dwNumRead %d\n", len, dwNumRead);
			FPRINTF(stderr, "Unknown errno %d in ADIOI_NTFS_aio\n", errno);
		    MPI_Abort(MPI_COMM_WORLD, 1);
		}
		ret_val = TRUE;
	}

    *((OVERLAPPED **) handle) = pOvl;

    return ret_val;
}
