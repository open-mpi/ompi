/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_pfs_wait.c,v 1.5 2002/10/24 17:00:53 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_pfs.h"

void ADIOI_PFS_ReadComplete(ADIO_Request *request, ADIO_Status *status, int *error_code)  
{
    int err=0;
#ifndef PRINT_ERR_MSG
    static char myname[] = "ADIOI_PFS_READCOMPLETE";
#endif

    if (*request == ADIO_REQUEST_NULL) {
        *error_code = MPI_SUCCESS;
        return;
    }

    if ((*request)->queued) {
	err = _iowait(*((long *) (*request)->handle));
#ifdef PRINT_ERR_MSG
	*error_code = (err == -1) ? MPI_ERR_UNKNOWN : MPI_SUCCESS;
#else
	if (err == -1) {
	    *error_code = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ADIO_ERROR,
			  myname, "I/O Error", "%s", strerror(errno));
	    ADIOI_Error((*request)->fd, *error_code, myname);	    
	}
	else *error_code = MPI_SUCCESS;
#endif
    }
    else *error_code = MPI_SUCCESS;
#ifdef HAVE_STATUS_SET_BYTES
    if ((*request)->nbytes != -1)
	MPIR_Status_set_bytes(status, (*request)->datatype, (*request)->nbytes);
#endif

    if ((*request)->queued != -1) {

        /* queued = -1 is an internal hack used when the request must
           be completed, but the request object should not be
           freed. This is used in ADIOI_Complete_async, because the user
           will call MPI_Wait later, which would require status to
           be filled. Ugly but works. queued = -1 should be used only
           in ADIOI_Complete_async. 
           This should not affect the user in any way. */

        /* if request is still queued in the system, it is also there
           on ADIOI_Async_list. Delete it from there. */
        if ((*request)->queued) ADIOI_Del_req_from_list(request);

        (*request)->fd->async_count--;
        if ((*request)->handle) ADIOI_Free((*request)->handle);
        ADIOI_Free_request((ADIOI_Req_node *) (*request));
        *request = ADIO_REQUEST_NULL;
    }
}


void ADIOI_PFS_WriteComplete(ADIO_Request *request, ADIO_Status *status, int *error_code)  
{
    ADIOI_PFS_ReadComplete(request, status, error_code);
}
