/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_pvfs.h"
#include "adio_extern.h"

void ADIOI_PVFS_Fcntl(ADIO_File fd, int flag, ADIO_Fcntl_t *fcntl_struct,
		      int *error_code)
{
    static char myname[] = "ADIOI_PVFS_FCNTL";

    switch(flag) {
    case ADIO_FCNTL_GET_FSIZE:
#ifdef ADIOI_MPE_LOGGING
        MPE_Log_event( ADIOI_MPE_lseek_a, 0, NULL );
#endif
	fcntl_struct->fsize = pvfs_lseek64(fd->fd_sys, 0, SEEK_END);
#ifdef ADIOI_MPE_LOGGING
        MPE_Log_event( ADIOI_MPE_lseek_b, 0, NULL );
#endif
	if (fd->fp_sys_posn != -1) {
#ifdef ADIOI_MPE_LOGGING
             MPE_Log_event( ADIOI_MPE_lseek_a, 0, NULL );
#endif
	     pvfs_lseek64(fd->fd_sys, fd->fp_sys_posn, SEEK_SET);
#ifdef ADIOI_MPE_LOGGING
             MPE_Log_event( ADIOI_MPE_lseek_b, 0, NULL );
#endif
        }
	if (fcntl_struct->fsize == -1) {
	    *error_code = MPIO_Err_create_code(MPI_SUCCESS,
					       MPIR_ERR_RECOVERABLE, myname,
					       __LINE__, MPI_ERR_IO, "**io",
					       "**io %s", strerror(errno));
	}
	else *error_code = MPI_SUCCESS;
	break;

    case ADIO_FCNTL_SET_DISKSPACE:
	ADIOI_GEN_Prealloc(fd, fcntl_struct->diskspace, error_code);
	break;

    case ADIO_FCNTL_SET_ATOMICITY:
	fd->atomicity = 0;
	/* --BEGIN ERROR HANDLING-- */
	if (fcntl_struct->atomicity != 0) {
	    *error_code = MPIO_Err_create_code(MPI_SUCCESS,
					       MPIR_ERR_RECOVERABLE,
					       myname, __LINE__,
					       MPI_ERR_UNSUPPORTED_OPERATION,
					       "PVFS does not support atomic mode",
					       0);
	    return;
	}
	/* --END ERROR HANDLING-- */
	break;

    default:
	/* --BEGIN ERROR HANDLING-- */
	*error_code = MPIO_Err_create_code(MPI_SUCCESS,
					   MPIR_ERR_RECOVERABLE,
					   myname, __LINE__,
					   MPI_ERR_ARG,
					   "**flag", "**flag %d", flag);
	return;  
	/* --END ERROR HANDLING-- */
    }
}
