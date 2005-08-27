/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: read_sh.c,v 1.7 2002/10/24 15:54:43 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_read_shared = PMPI_File_read_shared
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_read_shared MPI_File_read_shared
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_read_shared as PMPI_File_read_shared
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/* status object not filled currently */

/*@
    MPI_File_read_shared - Read using shared file pointer

Input Parameters:
. fh - file handle (handle)
. count - number of elements in buffer (nonnegative integer)
. datatype - datatype of each buffer element (handle)

Output Parameters:
. buf - initial address of buffer (choice)
. status - status object (Status)

.N fortran
@*/
int MPI_File_read_shared(MPI_File fh, void *buf, int count, 
                  MPI_Datatype datatype, MPI_Status *status)
{
    int error_code, bufsize, buftype_is_contig, filetype_is_contig;
#ifndef PRINT_ERR_MSG
    static char myname[] = "MPI_FILE_READ_SHARED";
#endif
    int datatype_size, incr;
    ADIO_Offset off, shared_fp;

#ifdef PRINT_ERR_MSG
    if ((fh <= (MPI_File) 0) || (fh->cookie != ADIOI_FILE_COOKIE)) {
	FPRINTF(stderr, "MPI_File_read_shared: Invalid file handle\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
#else
    ADIOI_TEST_FILE_HANDLE(fh, myname);
#endif

    if (count < 0) {
#ifdef PRINT_ERR_MSG
	FPRINTF(stderr, "MPI_File_read_shared: Invalid count argument\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_ARG, MPIR_ERR_COUNT_ARG,
				     myname, (char *) 0, (char *) 0);
	return ADIOI_Error(fh, error_code, myname);
#endif
    }

    if (datatype == MPI_DATATYPE_NULL) {
#ifdef PRINT_ERR_MSG
        FPRINTF(stderr, "MPI_File_read_shared: Invalid datatype\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_TYPE, MPIR_ERR_TYPE_NULL,
				     myname, (char *) 0, (char *) 0);
	return ADIOI_Error(fh, error_code, myname);	    
#endif
    }

    MPI_Type_size(datatype, &datatype_size);
    if (count*datatype_size == 0) return MPI_SUCCESS;

    if ((count*datatype_size) % fh->etype_size != 0) {
#ifdef PRINT_ERR_MSG
	FPRINTF(stderr, "MPI_File_read_shared: Only an integral number of etypes can be accessed\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ERR_ETYPE_FRACTIONAL,
				     myname, (char *) 0, (char *) 0);
	return ADIOI_Error(fh, error_code, myname);	    
#endif
    }

    if ((fh->file_system == ADIO_PIOFS) || (fh->file_system == ADIO_PVFS)) {
#ifdef PRINT_ERR_MSG
	FPRINTF(stderr, "MPI_File_read_shared: Shared file pointer not supported on PIOFS and PVFS\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_UNSUPPORTED_OPERATION, 
                    MPIR_ERR_NO_SHARED_FP, myname, (char *) 0, (char *) 0);
	return ADIOI_Error(fh, error_code, myname);
#endif
    }

    ADIOI_Datatype_iscontig(datatype, &buftype_is_contig);
    ADIOI_Datatype_iscontig(fh->filetype, &filetype_is_contig);

    incr = (count*datatype_size)/fh->etype_size;
    ADIO_Get_shared_fp(fh, incr, &shared_fp, &error_code);
    if (error_code != MPI_SUCCESS) {
	FPRINTF(stderr, "MPI_File_read_shared: Error! Could not access shared file pointer.\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    /* contiguous or strided? */
    if (buftype_is_contig && filetype_is_contig) {
	/* convert count and shared_fp to bytes */
        bufsize = datatype_size * count;
        off = fh->disp + fh->etype_size * shared_fp;

        /* if atomic mode requested, lock (exclusive) the region, because there
           could be a concurrent noncontiguous request. On NFS, locking 
           is done in the ADIO_ReadContig.*/

        if ((fh->atomicity) && (fh->file_system != ADIO_NFS))
            ADIOI_WRITE_LOCK(fh, off, SEEK_SET, bufsize);

        ADIO_ReadContig(fh, buf, count, datatype, ADIO_EXPLICIT_OFFSET,
                        off, status, &error_code); 

        if ((fh->atomicity) && (fh->file_system != ADIO_NFS))
            ADIOI_UNLOCK(fh, off, SEEK_SET, bufsize);
    }
    else ADIO_ReadStrided(fh, buf, count, datatype, ADIO_EXPLICIT_OFFSET,
                          shared_fp, status, &error_code); 
    /* For strided and atomic mode, locking is done in ADIO_ReadStrided */

    return error_code;
}
