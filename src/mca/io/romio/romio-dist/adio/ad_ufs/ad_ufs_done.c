/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_ufs_done.c,v 1.7 2002/10/24 17:01:06 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_ufs.h"

int ADIOI_UFS_ReadDone(ADIO_Request *request, ADIO_Status *status, int *error_code)  
{
#ifndef NO_AIO
    int done=0;
#ifndef PRINT_ERR_MSG
    static char myname[] = "ADIOI_UFS_READDONE";
#endif
#ifdef AIO_SUN 
    aio_result_t *result=0, *tmp;
#else
    int err;
#endif
#ifdef AIO_HANDLE_IN_AIOCB
    struct aiocb *tmp1;
#endif
#endif

    if (*request == ADIO_REQUEST_NULL) {
	*error_code = MPI_SUCCESS;
	return 1;
    }

#ifdef NO_AIO
/* HP, FreeBSD, Linux */
#ifdef HAVE_STATUS_SET_BYTES
    MPIR_Status_set_bytes(status, (*request)->datatype, (*request)->nbytes);
#endif
    (*request)->fd->async_count--;
    ADIOI_Free_request((ADIOI_Req_node *) (*request));
    *request = ADIO_REQUEST_NULL;
    *error_code = MPI_SUCCESS;
    return 1;
#endif    

#ifdef AIO_SUN
    if ((*request)->queued) {
	tmp = (aio_result_t *) (*request)->handle;
	if (tmp->aio_return == AIO_INPROGRESS) {
	    done = 0;
	    *error_code = MPI_SUCCESS;
	}
	else if (tmp->aio_return != -1) {
	    result = (aio_result_t *) aiowait(0); /* dequeue any one request */
	    done = 1;
	    (*request)->nbytes = tmp->aio_return;
	    *error_code = MPI_SUCCESS;
	}
	else {
#ifdef PRINT_ERR_MSG
	    *error_code = MPI_ERR_UNKNOWN;
#else
	    *error_code = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ADIO_ERROR,
		         myname, "I/O Error", "%s", strerror(tmp->aio_errno));
	    ADIOI_Error((*request)->fd, *error_code, myname);	    
#endif
	}
    }
    else {
	/* ADIOI_Complete_Async completed this request, but request object
           was not freed. */
	done = 1;
	*error_code = MPI_SUCCESS;
    }
#ifdef HAVE_STATUS_SET_BYTES
    if (done && ((*request)->nbytes != -1))
	MPIR_Status_set_bytes(status, (*request)->datatype, (*request)->nbytes);
#endif

#endif

#ifdef AIO_HANDLE_IN_AIOCB
/* IBM */
    if ((*request)->queued) {
	tmp1 = (struct aiocb *) (*request)->handle;
	errno = aio_error(tmp1->aio_handle);
	if (errno == EINPROG) {
	    done = 0;
	    *error_code = MPI_SUCCESS;
	}
	else {
	    err = aio_return(tmp1->aio_handle);
	    (*request)->nbytes = err;
	    errno = aio_error(tmp1->aio_handle);
	
	    done = 1;

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
    }
    else {
	done = 1;
	*error_code = MPI_SUCCESS;
    }
#ifdef HAVE_STATUS_SET_BYTES
    if (done && ((*request)->nbytes != -1))
	MPIR_Status_set_bytes(status, (*request)->datatype, (*request)->nbytes);
#endif

#elif (!defined(NO_AIO) && !defined(AIO_SUN))
/* DEC, SGI IRIX 5 and 6 */
    if ((*request)->queued) {
	errno = aio_error((const struct aiocb *) (*request)->handle);
	if (errno == EINPROGRESS) {
	    done = 0;
	    *error_code = MPI_SUCCESS;
	}
	else {
	    err = aio_return((struct aiocb *) (*request)->handle); 
	    (*request)->nbytes = err;
	    errno = aio_error((struct aiocb *) (*request)->handle);

	    done = 1;

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
    }
    else {
	done = 1;
	*error_code = MPI_SUCCESS;
    }
#ifdef HAVE_STATUS_SET_BYTES
    if (done && ((*request)->nbytes != -1))
	MPIR_Status_set_bytes(status, (*request)->datatype, (*request)->nbytes);
#endif

#endif

#ifndef NO_AIO
    if (done) {
	/* if request is still queued in the system, it is also there
           on ADIOI_Async_list. Delete it from there. */
	if ((*request)->queued) ADIOI_Del_req_from_list(request);

	(*request)->fd->async_count--;
	if ((*request)->handle) ADIOI_Free((*request)->handle);
	ADIOI_Free_request((ADIOI_Req_node *) (*request));
	*request = ADIO_REQUEST_NULL;
    }
    return done;
#endif

}


int ADIOI_UFS_WriteDone(ADIO_Request *request, ADIO_Status *status, int *error_code)  
{
    return ADIOI_UFS_ReadDone(request, status, error_code);
} 
