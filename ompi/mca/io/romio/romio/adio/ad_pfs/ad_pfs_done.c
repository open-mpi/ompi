/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_pfs_done.c,v 1.6 2002/10/24 17:00:51 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_pfs.h"

int ADIOI_PFS_ReadDone(ADIO_Request *request, ADIO_Status *status, int *error_code)  
{
    int done=0;
#ifndef PRINT_ERR_MSG
    static char myname[] = "ADIOI_PFS_READDONE";
#endif

    if (*request == ADIO_REQUEST_NULL) {
        *error_code = MPI_SUCCESS;
        return 1;
    }

    if ((*request)->queued)
	done = _iodone(*((long *) (*request)->handle));
    else done = 1; /* ADIOI_Complete_Async completed this request, 
                      but request object was not freed. */

#ifdef HAVE_STATUS_SET_BYTES
    if ((done >= 0) && ((*request)->nbytes != -1))
	MPIR_Status_set_bytes(status, (*request)->datatype, (*request)->nbytes);
#endif

    if (done >= 0) {
        /* if request is still queued in the system, it is also there
           on ADIOI_Async_list. Delete it from there. */
        if ((*request)->queued) ADIOI_Del_req_from_list(request);

        (*request)->fd->async_count--;
        if ((*request)->handle) ADIOI_Free((*request)->handle);
        ADIOI_Free_request((ADIOI_Req_node *) (*request));
        *request = ADIO_REQUEST_NULL;
    }
    
#ifdef PRINT_ERR_MSG
    *error_code = (done == -1 && errno != 0) ? MPI_ERR_UNKNOWN : MPI_SUCCESS;
#else
    if (done == -1 && errno != 0) {
	*error_code = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ADIO_ERROR,
			      myname, "I/O Error", "%s", strerror(errno));
	ADIOI_Error((*request)->fd, *error_code, myname);	    
    }
    else *error_code = MPI_SUCCESS;
#endif
    return done;
}


int ADIOI_PFS_WriteDone(ADIO_Request *request, ADIO_Status *status, int *error_code)  
{
    return ADIOI_PFS_ReadDone(request, status, error_code);
} 
