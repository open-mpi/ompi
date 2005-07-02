/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_ntfs_done.c,v 1.4 2002/11/13 13:30:34 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

/* Set the style to c++ since this code will only be compiled with the
   Windows C/C++ compiler that accepts C++ style comments and other 
   constructions */
/* style:c++ header */

#include "ad_ntfs.h"

int ADIOI_NTFS_ReadDone(ADIO_Request *request, ADIO_Status *status, int *error_code)  
{
	DWORD ret_val;
    int done=0;
#ifndef PRINT_ERR_MSG
    static char myname[] = "ADIOI_NTFS_READDONE";
#endif
	
    if (*request == ADIO_REQUEST_NULL) {
		*error_code = MPI_SUCCESS;
		return 1;
    }
	
    if ((*request)->queued) 
	{
		(*request)->nbytes = 0;
		ret_val = GetOverlappedResult((*request)->fd, (*request)->handle, &(*request)->nbytes, FALSE);
		//ret_val = WaitForSingleObject((*request)->handle, INFINITE);
		//ret_val = (ret_val == WAIT_OBJECT_0) ? TRUE : FALSE;
		
		if (!ret_val)
		{
			ret_val = GetLastError();
			if (ret_val == ERROR_IO_INCOMPLETE)
			{
				done = 0;
				*error_code = MPI_SUCCESS;
			}
		}
		else 
		{
			done = 1;		
			*error_code = MPI_SUCCESS;
		}
    }
    else {
		done = 1;
		*error_code = MPI_SUCCESS;
    }
#ifdef HAVE_STATUS_SET_BYTES
    if (done && ((*request)->nbytes != -1))
		MPIR_Status_set_bytes(status, (*request)->datatype, (*request)->nbytes);
#endif
	
    if (done) 
	{
	/* if request is still queued in the system, it is also there
		on ADIOI_Async_list. Delete it from there. */
		if ((*request)->queued) ADIOI_Del_req_from_list(request);
		
		(*request)->fd->async_count--;
		if ((*request)->handle) 
		{
			CloseHandle(((OVERLAPPED*)((*request)->handle))->hEvent);
			ADIOI_Free((*request)->handle);
		}
		ADIOI_Free_request((ADIOI_Req_node *) (*request));
		*request = ADIO_REQUEST_NULL;
    }
    return done;
}


int ADIOI_NTFS_WriteDone(ADIO_Request *request, ADIO_Status *status, int *error_code)  
{
	return ADIOI_NTFS_ReadDone(request, status, error_code);
} 
