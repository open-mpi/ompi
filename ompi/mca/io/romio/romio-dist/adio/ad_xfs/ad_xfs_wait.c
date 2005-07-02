/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_xfs_wait.c,v 1.5 2002/10/24 17:01:10 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_xfs.h"

void ADIOI_XFS_ReadComplete(ADIO_Request *request, ADIO_Status *status, int *error_code)  
{
    int err;
#ifndef PRINT_ERR_MSG
    static char myname[] = "ADIOI_XFS_READCOMPLETE";
#endif

    if (*request == ADIO_REQUEST_NULL) {
	*error_code = MPI_SUCCESS;
	return;
    }

    if ((*request)->queued) {
	do {
	    err = aio_suspend64((const aiocb64_t **) &((*request)->handle), 1, 0);
	} while ((err == -1) && (errno == EINTR));

	if (err != -1) {
	    err = aio_return64((aiocb64_t *) (*request)->handle); 
	    (*request)->nbytes = err;
	    errno = aio_error64((aiocb64_t *) (*request)->handle);
	}
	else (*request)->nbytes = -1;

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


void ADIOI_XFS_WriteComplete(ADIO_Request *request, ADIO_Status *status, int *error_code)  
{
    ADIOI_XFS_ReadComplete(request, status, error_code);
}
