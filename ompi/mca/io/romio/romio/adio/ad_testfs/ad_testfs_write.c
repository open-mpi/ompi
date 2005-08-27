/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_testfs_write.c,v 1.5 2002/10/24 17:01:06 gropp Exp $    
 *
 *   Copyright (C) 2001 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_testfs.h"
#include "adioi.h"

void ADIOI_TESTFS_WriteContig(ADIO_File fd, void *buf, int count, 
			      MPI_Datatype datatype, int file_ptr_type,
			      ADIO_Offset offset, ADIO_Status *status, int
			      *error_code)
{
    int myrank, nprocs, datatype_size;

    *error_code = MPI_SUCCESS;

    MPI_Comm_size(fd->comm, &nprocs);
    MPI_Comm_rank(fd->comm, &myrank);
    MPI_Type_size(datatype, &datatype_size);
    FPRINTF(stdout, "[%d/%d] ADIOI_TESTFS_WriteContig called on %s\n", myrank, 
	    nprocs, fd->filename);
    FPRINTF(stdout, "[%d/%d]    writing (buf = 0x%x, loc = %Ld, sz = %Ld)\n",
	    myrank, nprocs, (int) buf, (long long) offset, 
	    (long long) datatype_size * count);

    if (file_ptr_type != ADIO_EXPLICIT_OFFSET)
    {
	fd->fp_ind += datatype_size * count;
	fd->fp_sys_posn = fd->fp_ind;
	FPRINTF(stdout, "[%d/%d]    new file position is %Ld\n", myrank, 
		nprocs, (long long) fd->fp_ind);
    }
    else {
	fd->fp_sys_posn = offset + datatype_size * count;
    }

#ifdef HAVE_STATUS_SET_BYTES
    MPIR_Status_set_bytes(status, datatype, datatype_size * count);
#endif
}

void ADIOI_TESTFS_WriteStrided(ADIO_File fd, void *buf, int count,
			       MPI_Datatype datatype, int file_ptr_type,
			       ADIO_Offset offset, ADIO_Status *status,
			       int *error_code)
{
    int myrank, nprocs;

    *error_code = MPI_SUCCESS;

    MPI_Comm_size(fd->comm, &nprocs);
    MPI_Comm_rank(fd->comm, &myrank);
    FPRINTF(stdout, "[%d/%d] ADIOI_TESTFS_WriteStrided called on %s\n", 
	    myrank, nprocs, fd->filename);
    FPRINTF(stdout, "[%d/%d]    calling ADIOI_GEN_WriteStrided\n", 
	    myrank, nprocs);

    ADIOI_GEN_WriteStrided(fd, buf, count, datatype, file_ptr_type, offset, 
			   status, error_code);
}
