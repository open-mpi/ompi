/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_pfs_iread.c,v 1.5 2002/10/24 17:00:52 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_pfs.h"

void ADIOI_PFS_IreadContig(ADIO_File fd, void *buf, int count, 
                MPI_Datatype datatype, int file_ptr_type,
                ADIO_Offset offset, ADIO_Request *request, int *error_code)  
{
    long *id_sys;
    int len, typesize, err=-1;
    ADIO_Offset off;
#ifndef PRINT_ERR_MSG
    static char myname[] = "ADIOI_PFS_IREADCONTIG";
#endif

    *request = ADIOI_Malloc_request();
    (*request)->optype = ADIOI_READ;
    (*request)->fd = fd;
    (*request)->datatype = datatype;

    MPI_Type_size(datatype, &typesize);
    len = count * typesize;

    id_sys = (long *) ADIOI_Malloc(sizeof(long));
    (*request)->handle = (void *) id_sys;

    off = (file_ptr_type == ADIO_INDIVIDUAL) ? fd->fp_ind : offset;

    lseek(fd->fd_sys, off, SEEK_SET);
    *id_sys = _iread(fd->fd_sys, buf, len);

    if ((*id_sys == -1) && (errno == EQNOMID)) {
     /* the man pages say EMREQUEST, but in reality errno is set to EQNOMID! */

        /* exceeded the max. no. of outstanding requests. */

        /* complete all previous async. requests */
        ADIOI_Complete_async(&err);

        /* try again */
        *id_sys = _iread(fd->fd_sys, buf, len);

        if ((*id_sys == -1) && (errno == EQNOMID)) {
#ifdef PRINT_ERR_MSG
            FPRINTF(stderr, "Error in asynchronous I/O\n");
            MPI_Abort(MPI_COMM_WORLD, 1);
#else
	    *error_code = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ADIO_ERROR,
			      myname, "I/O Error", "%s", strerror(errno));
	    ADIOI_Error(fd, *error_code, myname);	    
	    return;
#endif
        }
    }
    else if (*id_sys == -1) {
#ifdef PRINT_ERR_MSG
        FPRINTF(stderr, "Unknown errno %d in ADIOI_PFS_IreadContig\n", errno);
        MPI_Abort(MPI_COMM_WORLD, 1);
#else
	*error_code = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ADIO_ERROR,
			      myname, "I/O Error", "%s", strerror(errno));
	ADIOI_Error(fd, *error_code, myname);	    
	return;
#endif
    }

    if (file_ptr_type == ADIO_INDIVIDUAL) fd->fp_ind += len; 

    (*request)->queued = 1;
    (*request)->nbytes = len;
    ADIOI_Add_req_to_list(request);
    fd->async_count++;

    fd->fp_sys_posn = -1;   /* set it to null. */

#ifdef PRINT_ERR_MSG
    *error_code = (*id_sys == -1) ? MPI_ERR_UNKNOWN : MPI_SUCCESS;
#else
    if (*id_sys == -1) {
	*error_code = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ADIO_ERROR,
			      myname, "I/O Error", "%s", strerror(errno));
	ADIOI_Error(fd, *error_code, myname);	    
    }
    else *error_code = MPI_SUCCESS;
#endif
}



void ADIOI_PFS_IreadStrided(ADIO_File fd, void *buf, int count, 
		       MPI_Datatype datatype, int file_ptr_type,
                       ADIO_Offset offset, ADIO_Request *request, int
                       *error_code)
{
    ADIO_Status status;
#ifdef HAVE_STATUS_SET_BYTES
    int typesize;
#endif

    *request = ADIOI_Malloc_request();
    (*request)->optype = ADIOI_READ;
    (*request)->fd = fd;
    (*request)->datatype = datatype;
    (*request)->queued = 0;
    (*request)->handle = 0;

/* call the blocking version. It is faster because it does data sieving. */
    ADIOI_PFS_ReadStrided(fd, buf, count, datatype, file_ptr_type, 
                            offset, &status, error_code);  

    fd->async_count++;
#ifdef HAVE_STATUS_SET_BYTES
    if (*error_code == MPI_SUCCESS) {
	MPI_Type_size(datatype, &typesize);
	(*request)->nbytes = count * typesize;
    }
#endif
}
