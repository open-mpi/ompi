/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_nfs_wait.c,v 1.8 2002/10/24 17:00:48 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_nfs.h"

void ADIOI_NFS_ReadComplete(ADIO_Request *request, ADIO_Status *status, int *error_code)  
{
#ifndef NO_AIO
#ifndef PRINT_ERR_MSG
    static char myname[] = "ADIOI_NFS_READCOMPLETE";
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
	return;
    }

#ifdef AIO_SUN
    if ((*request)->queued) {  /* dequeue it */
	tmp = (aio_result_t *) (*request)->handle;
	while (tmp->aio_return == AIO_INPROGRESS) usleep(1000); 
	/* sleep for 1 ms., until done. Is 1 ms. a good number? */
	/* when done, dequeue any one request */
	result = (aio_result_t *) aiowait(0);

        (*request)->nbytes = tmp->aio_return;

#ifdef PRINT_ERR_MSG
	*error_code = (tmp->aio_return == -1) ? MPI_ERR_UNKNOWN : MPI_SUCCESS;
#else
	if (tmp->aio_return == -1) {
	    *error_code = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ADIO_ERROR,
			  myname, "I/O Error", "%s", strerror(tmp->aio_errno));
	    ADIOI_Error((*request)->fd, *error_code, myname);	    
	}
	else *error_code = MPI_SUCCESS;
#endif

/* aiowait only dequeues a request. The completion of a request can be
   checked by just checking the aio_return flag in the handle passed
   to the original aioread()/aiowrite(). Therefore, I need to ensure
   that aiowait() is called exactly once for each previous
   aioread()/aiowrite(). This is also taken care of in ADIOI_xxxDone */
    }
    else *error_code = MPI_SUCCESS;

#ifdef HAVE_STATUS_SET_BYTES
    if ((*request)->nbytes != -1)
	MPIR_Status_set_bytes(status, (*request)->datatype, (*request)->nbytes);
#endif

#endif
    
#ifdef AIO_HANDLE_IN_AIOCB
/* IBM */
    if ((*request)->queued) {
	do {
#if !defined(_AIO_AIX_SOURCE) && !defined(_NO_PROTO)
	    err = aio_suspend((*request)->handle,1,NULL);
#else
	    err = aio_suspend(1, (struct aiocb **) &((*request)->handle));
#endif
	} while ((err == -1) && (errno == EINTR));

	tmp1 = (struct aiocb *) (*request)->handle;
	if (err != -1) {
	    err = aio_return(tmp1->aio_handle);
	    (*request)->nbytes = err;
	    errno = aio_error(tmp1->aio_handle);
	}
	else (*request)->nbytes = -1;

/* on DEC, it is required to call aio_return to dequeue the request.
   IBM man pages don't indicate what function to use for dequeue.
   I'm assuming it is aio_return! */

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

#elif (!defined(NO_AIO) && !defined(AIO_SUN))
/* DEC, SGI IRIX 5 and 6 */
    if ((*request)->queued) {
	do {
	    err = aio_suspend((const aiocb_t **) &((*request)->handle), 1, 0);
	} while ((err == -1) && (errno == EINTR));

	if (err != -1) {
	    err = aio_return((struct aiocb *) (*request)->handle); 
	    (*request)->nbytes = err;
	    errno = aio_error((struct aiocb *) (*request)->handle);
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
#endif

#ifndef NO_AIO
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

#else
/* HP, FreeBSD, Linux */

#ifdef HAVE_STATUS_SET_BYTES
    MPIR_Status_set_bytes(status, (*request)->datatype, (*request)->nbytes);
#endif
    (*request)->fd->async_count--;
    ADIOI_Free_request((ADIOI_Req_node *) (*request));
    *request = ADIO_REQUEST_NULL;
    *error_code = MPI_SUCCESS;
#endif    
}


void ADIOI_NFS_WriteComplete(ADIO_Request *request, ADIO_Status *status, int *error_code)  
{
    ADIOI_NFS_ReadComplete(request, status, error_code);
}
