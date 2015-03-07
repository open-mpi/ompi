/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 2004 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */


#include <unistd.h>

#include "adio.h"
#ifdef AGGREGATION_PROFILE
#include "mpe.h"
#endif

#ifdef ROMIO_GPFS
#include "adio/ad_gpfs/ad_gpfs_tuning.h"
#endif

#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif


void ADIOI_GEN_WriteContig(ADIO_File fd, const void *buf, int count,
			   MPI_Datatype datatype, int file_ptr_type,
			   ADIO_Offset offset, ADIO_Status *status,
			   int *error_code)
{
    ssize_t err = -1;
    MPI_Count datatype_size;
    ADIO_Offset len, bytes_xfered=0;
    size_t wr_count;
    static char myname[] = "ADIOI_GEN_WRITECONTIG";
#ifdef ROMIO_GPFS
    double io_time=0;
#endif
    char * p;

#ifdef AGGREGATION_PROFILE
    MPE_Log_event (5036, 0, NULL);
#endif

    MPI_Type_size_x(datatype, &datatype_size);
    len = (ADIO_Offset)datatype_size * (ADIO_Offset)count;

#ifdef ROMIO_GPFS
    io_time = MPI_Wtime();
    if (gpfsmpio_timing) {
	gpfsmpio_prof_cw[ GPFSMPIO_CIO_DATA_SIZE ] += len;
    }
#endif

    if (file_ptr_type == ADIO_INDIVIDUAL) {
	offset = fd->fp_ind;
    }

    p = (char *)buf;
    while (bytes_xfered < len) {
#ifdef ADIOI_MPE_LOGGING
	MPE_Log_event( ADIOI_MPE_write_a, 0, NULL );
#endif
	wr_count = len - bytes_xfered;
	/* Frustrating! FreeBSD and OS X do not like a count larger than 2^31 */
        if (wr_count > INT_MAX)
            wr_count = INT_MAX;

#ifdef ROMIO_GPFS
	if (gpfsmpio_devnullio)
	    err = pwrite(fd->null_fd, p, wr_count, offset+bytes_xfered);
	else
#endif
	    err = pwrite(fd->fd_sys, p, wr_count, offset+bytes_xfered);
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
#ifdef ADIOI_MPE_LOGGING
	MPE_Log_event( ADIOI_MPE_write_b, 0, NULL );
#endif
	bytes_xfered += err;
	p += err;
    }

#ifdef ROMIO_GPFS
    if (gpfsmpio_timing) gpfsmpio_prof_cw[ GPFSMPIO_CIO_T_POSI_RW ] += (MPI_Wtime() - io_time);
#endif
    fd->fp_sys_posn = offset + bytes_xfered;

    if (file_ptr_type == ADIO_INDIVIDUAL) {
	fd->fp_ind += bytes_xfered; 
    }

#ifdef ROMIO_GPFS
    if (gpfsmpio_timing) gpfsmpio_prof_cw[ GPFSMPIO_CIO_T_MPIO_RW ] += (MPI_Wtime() - io_time);
#endif

#ifdef HAVE_STATUS_SET_BYTES
    /* bytes_xfered could be larger than int */
    if (err != -1 && status) MPIR_Status_set_bytes(status, datatype, bytes_xfered);
#endif

    *error_code = MPI_SUCCESS;
#ifdef AGGREGATION_PROFILE
    MPE_Log_event (5037, 0, NULL);
#endif
}
