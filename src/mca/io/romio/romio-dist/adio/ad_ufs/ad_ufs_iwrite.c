/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_ufs_iwrite.c,v 1.7 2002/10/24 17:01:07 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_ufs.h"

void ADIOI_UFS_IwriteContig(ADIO_File fd, void *buf, int count, 
                MPI_Datatype datatype, int file_ptr_type,
                ADIO_Offset offset, ADIO_Request *request, int *error_code)  
{
    int len, typesize;
#ifdef NO_AIO
    ADIO_Status status;
#else
    int err=-1;
#ifndef PRINT_ERR_MSG
    static char myname[] = "ADIOI_UFS_IWRITECONTIG";
#endif
#endif

    *request = ADIOI_Malloc_request();
    (*request)->optype = ADIOI_WRITE;
    (*request)->fd = fd;
    (*request)->datatype = datatype;

    MPI_Type_size(datatype, &typesize);
    len = count * typesize;

#ifdef NO_AIO
    /* HP, FreeBSD, Linux */
    /* no support for nonblocking I/O. Use blocking I/O. */

    ADIOI_UFS_WriteContig(fd, buf, len, MPI_BYTE, file_ptr_type, offset, 
			  &status, error_code);  
    (*request)->queued = 0;
#ifdef HAVE_STATUS_SET_BYTES
    if (*error_code == MPI_SUCCESS) {
	MPI_Get_elements(&status, MPI_BYTE, &len);
	(*request)->nbytes = len;
    }
#endif

#else
    if (file_ptr_type == ADIO_INDIVIDUAL) offset = fd->fp_ind;
    err = ADIOI_UFS_aio(fd, buf, len, offset, 1, &((*request)->handle));
    if (file_ptr_type == ADIO_INDIVIDUAL) fd->fp_ind += len;

    (*request)->queued = 1;
    ADIOI_Add_req_to_list(request);

#ifdef PRINT_ERR_MSG
    *error_code = (err == -1) ? MPI_ERR_UNKNOWN : MPI_SUCCESS;
#else
    if (err == -1) {
	*error_code = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ADIO_ERROR,
			      myname, "I/O Error", "%s", strerror(errno));
	ADIOI_Error(fd, *error_code, myname);	    
    }
    else *error_code = MPI_SUCCESS;
#endif
#endif

    fd->fp_sys_posn = -1;   /* set it to null. */
    fd->async_count++;
}




void ADIOI_UFS_IwriteStrided(ADIO_File fd, void *buf, int count, 
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
    ADIOI_UFS_WriteStrided(fd, buf, count, datatype, file_ptr_type, 
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

int ADIOI_UFS_aio(ADIO_File fd, void *buf, int len, ADIO_Offset offset,
		  int wr, void *handle)
{
    int err=-1, fd_sys;

#ifndef NO_AIO
    int error_code;
#ifdef AIO_SUN 
    aio_result_t *result;
#else
    struct aiocb *aiocbp;
#endif
#endif

    fd_sys = fd->fd_sys;

#ifdef AIO_SUN
    result = (aio_result_t *) ADIOI_Malloc(sizeof(aio_result_t));
    result->aio_return = AIO_INPROGRESS;
    if (wr) err = aiowrite(fd_sys, buf, len, offset, SEEK_SET, result); 
    else err = aioread(fd_sys, buf, len, offset, SEEK_SET, result);

    if (err == -1) {
	if (errno == EAGAIN) { 
       /* the man pages say EPROCLIM, but in reality errno is set to EAGAIN! */

        /* exceeded the max. no. of outstanding requests.
           complete all previous async. requests and try again.*/

	    ADIOI_Complete_async(&error_code);
	    if (wr) err = aiowrite(fd_sys, buf, len, offset, SEEK_SET, result); 
	    else err = aioread(fd_sys, buf, len, offset, SEEK_SET, result);

	    while (err == -1) {
		if (errno == EAGAIN) {
                    /* sleep and try again */
                    sleep(1);
		    if (wr) err = aiowrite(fd_sys, buf, len, offset, SEEK_SET, result); 
		    else err = aioread(fd_sys, buf, len, offset, SEEK_SET, result);
		}
                else {
                    FPRINTF(stderr, "Unknown errno %d in ADIOI_UFS_aio\n", errno);
                    MPI_Abort(MPI_COMM_WORLD, 1);
                }
	    }
	}
        else {
            FPRINTF(stderr, "Unknown errno %d in ADIOI_UFS_aio\n", errno);
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
    }

    *((aio_result_t **) handle) = result;
#endif

#ifdef NO_FD_IN_AIOCB
/* IBM */
    aiocbp = (struct aiocb *) ADIOI_Malloc(sizeof(struct aiocb));
    aiocbp->aio_whence = SEEK_SET;
    aiocbp->aio_offset = offset;
    aiocbp->aio_buf = buf;
    aiocbp->aio_nbytes = len;
    if (wr) err = aio_write(fd_sys, aiocbp);
    else err = aio_read(fd_sys, aiocbp);

    if (err == -1) {
	if (errno == EAGAIN) {
        /* exceeded the max. no. of outstanding requests.
          complete all previous async. requests and try again. */

	    ADIOI_Complete_async(&error_code);
	    if (wr) err = aio_write(fd_sys, aiocbp);
	    else err = aio_read(fd_sys, aiocbp);

            while (err == -1) {
                if (errno == EAGAIN) {
                    /* sleep and try again */
                    sleep(1);
		    if (wr) err = aio_write(fd_sys, aiocbp);
		    else err = aio_read(fd_sys, aiocbp);
		}
                else {
                    FPRINTF(stderr, "Unknown errno %d in ADIOI_UFS_aio\n", errno);
                    MPI_Abort(MPI_COMM_WORLD, 1);
                }
            }
	}
        else {
            FPRINTF(stderr, "Unknown errno %d in ADIOI_UFS_aio\n", errno);
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
    }

    *((struct aiocb **) handle) = aiocbp;

#elif (!defined(NO_AIO) && !defined(AIO_SUN))
/* DEC, SGI IRIX 5 and 6 */

    aiocbp = (struct aiocb *) ADIOI_Calloc(sizeof(struct aiocb), 1);
    aiocbp->aio_fildes = fd_sys;
    aiocbp->aio_offset = offset;
    aiocbp->aio_buf = buf;
    aiocbp->aio_nbytes = len;

#ifdef AIO_PRIORITY_DEFAULT
/* DEC */
    aiocbp->aio_reqprio = AIO_PRIO_DFL;   /* not needed in DEC Unix 4.0 */
    aiocbp->aio_sigevent.sigev_signo = 0;
#else
    aiocbp->aio_reqprio = 0;
#endif

#ifdef AIO_SIGNOTIFY_NONE
/* SGI IRIX 6 */
    aiocbp->aio_sigevent.sigev_notify = SIGEV_NONE;
#else
    aiocbp->aio_sigevent.sigev_signo = 0;
#endif

    if (wr) err = aio_write(aiocbp);
    else err = aio_read(aiocbp);

    if (err == -1) {
	if (errno == EAGAIN) {
        /* exceeded the max. no. of outstanding requests.
           complete all previous async. requests and try again. */

	    ADIOI_Complete_async(&error_code);
	    if (wr) err = aio_write(aiocbp);
	    else err = aio_read(aiocbp);

	    while (err == -1) {
		if (errno == EAGAIN) {
		    /* sleep and try again */
		    sleep(1);
		    if (wr) err = aio_write(aiocbp);
		    else err = aio_read(aiocbp);
		}
		else {
		    FPRINTF(stderr, "Unknown errno %d in ADIOI_UFS_aio\n", errno);
		    MPI_Abort(MPI_COMM_WORLD, 1);
		}
	    }
        }
	else {
	    FPRINTF(stderr, "Unknown errno %d in ADIOI_UFS_aio\n", errno);
	    MPI_Abort(MPI_COMM_WORLD, 1);
	}
    }

    *((struct aiocb **) handle) = aiocbp;
#endif

    return err;
}
