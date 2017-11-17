/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */


#include <unistd.h>

#include "adio.h"
#ifdef AGGREGATION_PROFILE
#include "mpe.h"
#endif
#ifdef ROMIO_GPFS
# include "adio/ad_gpfs/ad_gpfs_tuning.h"
#endif

#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif

void ADIOI_GEN_ReadContig(ADIO_File fd, void *buf, int count, 
			  MPI_Datatype datatype, int file_ptr_type,
			  ADIO_Offset offset, ADIO_Status *status,
			  int *error_code)
{
    ssize_t err = -1;
    MPI_Count datatype_size;
    ADIO_Offset len, bytes_xfered=0;
    size_t rd_count;
    static char myname[] = "ADIOI_GEN_READCONTIG";
#ifdef ROMIO_GPFS
    double io_time=0;
#endif
    char *p;

#ifdef AGGREGATION_PROFILE
    MPE_Log_event (5034, 0, NULL);
#endif
    MPI_Type_size_x(datatype, &datatype_size);
    len = datatype_size * (ADIO_Offset)count;

#ifdef ROMIO_GPFS
    io_time = MPI_Wtime();
    if (gpfsmpio_timing) {
	gpfsmpio_prof_cr[ GPFSMPIO_CIO_DATA_SIZE ] += len;
    }
#endif

    if (file_ptr_type == ADIO_INDIVIDUAL) {
	offset = fd->fp_ind;
    }

    p=buf;
    while (bytes_xfered < len) {
#ifdef ADIOI_MPE_LOGGING
	MPE_Log_event( ADIOI_MPE_read_a, 0, NULL );
#endif
	rd_count = len - bytes_xfered;
	/* stupid FreeBSD and Darwin do not like a count larger than a signed
           int, even though size_t is eight bytes... */
        if (rd_count > INT_MAX)
            rd_count = INT_MAX;
#ifdef ROMIO_GPFS
	if (gpfsmpio_devnullio)
	    err = pread(fd->null_fd, p, rd_count, offset+bytes_xfered);
	else
#endif
	    err = pread(fd->fd_sys, p, rd_count, offset+bytes_xfered);
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
	if (err == 0) {
	    /* end of file */
	    break;
	}

#ifdef ADIOI_MPE_LOGGING
	MPE_Log_event( ADIOI_MPE_read_b, 0, NULL );
#endif
	bytes_xfered += err;
	p += err;
    }
#ifdef ROMIO_GPFS
    if (gpfsmpio_timing) gpfsmpio_prof_cr[ GPFSMPIO_CIO_T_POSI_RW ] += (MPI_Wtime() - io_time);
#endif
    fd->fp_sys_posn = offset + bytes_xfered;

    if (file_ptr_type == ADIO_INDIVIDUAL) {
	fd->fp_ind += bytes_xfered; 
    }

#ifdef HAVE_STATUS_SET_BYTES
    /* what if we only read half a datatype? */
    /* bytes_xfered could be larger than int */
    if (err != -1) MPIR_Status_set_bytes(status, datatype, bytes_xfered);
#endif

    *error_code = MPI_SUCCESS;
#ifdef AGGREGATION_PROFILE
    MPE_Log_event (5035, 0, NULL);
#endif
#ifdef ROMIO_GPFS
    if (gpfsmpio_timing) gpfsmpio_prof_cr[ GPFSMPIO_CIO_T_MPIO_RW ] += (MPI_Wtime() - io_time);
#endif
}
