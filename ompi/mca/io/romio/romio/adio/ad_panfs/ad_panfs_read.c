/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_panfs.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

void ADIOI_PANFS_ReadContig(ADIO_File fd, void *buf, int count, 
			  MPI_Datatype datatype, int file_ptr_type,
			  ADIO_Offset offset, ADIO_Status *status,
			  int *error_code)
{
    MPI_Count err = -1, datatype_size, len;
    static char myname[] = "ADIOI_PANFS_READCONTIG";

    MPI_Type_size_x(datatype, &datatype_size);
    len = datatype_size * count;

    if (file_ptr_type == ADIO_INDIVIDUAL) {
	offset = fd->fp_ind;
    }

    if (fd->fp_sys_posn != offset) {
	err = lseek(fd->fd_sys, offset, SEEK_SET);
	/* --BEGIN ERROR HANDLING-- */
	if (err == -1) {
	    *error_code = MPIO_Err_create_code(MPI_SUCCESS,
					       MPIR_ERR_RECOVERABLE,
					       myname, __LINE__,
					       MPI_ERR_IO, "**io",
					       "**io %s", strerror(errno));
	    fd->fp_sys_posn = -1;
	    return;
	}
	/* --END ERROR HANDLING-- */
    }

    AD_PANFS_RETRY(read(fd->fd_sys, buf, len),err)
    /* --BEGIN ERROR HANDLING-- */
    if (err == -1) {
	*error_code = MPIO_Err_create_code(MPI_SUCCESS,
					   MPIR_ERR_RECOVERABLE,
					   myname, __LINE__,
					   MPI_ERR_IO, "**io",
					   "**io %s", strerror(errno));
	fd->fp_sys_posn = -1;
	return;
    }
    /* --END ERROR HANDLING-- */

    fd->fp_sys_posn = offset + err;

    if (file_ptr_type == ADIO_INDIVIDUAL) {
	fd->fp_ind += err; 
    }

#ifdef HAVE_STATUS_SET_BYTES
    if (err != -1) MPIR_Status_set_bytes(status, datatype, err);
#endif

    *error_code = MPI_SUCCESS;
}
