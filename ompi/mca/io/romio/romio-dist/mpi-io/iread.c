/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: iread.c,v 1.8 2002/10/24 15:54:41 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_iread = PMPI_File_iread
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_iread MPI_File_iread
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_iread as PMPI_File_iread
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_iread - Nonblocking read using individual file pointer

Input Parameters:
. fh - file handle (handle)
. count - number of elements in buffer (nonnegative integer)
. datatype - datatype of each buffer element (handle)

Output Parameters:
. buf - initial address of buffer (choice)
. request - request object (handle)

.N fortran
@*/
int MPI_File_iread(MPI_File fh, void *buf, int count, 
                   MPI_Datatype datatype, MPIO_Request *request)
{
    int error_code, bufsize, buftype_is_contig, filetype_is_contig;
#ifndef PRINT_ERR_MSG
    static char myname[] = "MPI_FILE_IREAD";
#endif
    int datatype_size;
    ADIO_Offset off;
    ADIO_Status status;
#ifdef MPI_hpux
    int fl_xmpi;

    HPMP_IO_START(fl_xmpi, BLKMPIFILEIREAD, TRDTSYSTEM, fh, datatype, count);
#endif /* MPI_hpux */

#ifdef PRINT_ERR_MSG
    if ((fh <= (MPI_File) 0) || (fh->cookie != ADIOI_FILE_COOKIE)) {
	FPRINTF(stderr, "MPI_File_iread: Invalid file handle\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
#else
    ADIOI_TEST_FILE_HANDLE(fh, myname);
#endif

    if (count < 0) {
#ifdef PRINT_ERR_MSG
	FPRINTF(stderr, "MPI_File_iread: Invalid count argument\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_ARG, MPIR_ERR_COUNT_ARG,
				     myname, (char *) 0, (char *) 0);
	return ADIOI_Error(fh, error_code, myname);
#endif
    }

    if (datatype == MPI_DATATYPE_NULL) {
#ifdef PRINT_ERR_MSG
        FPRINTF(stderr, "MPI_File_iread: Invalid datatype\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_TYPE, MPIR_ERR_TYPE_NULL,
				     myname, (char *) 0, (char *) 0);
	return ADIOI_Error(fh, error_code, myname);	    
#endif
    }

    MPI_Type_size(datatype, &datatype_size);

    if ((count*datatype_size) % fh->etype_size != 0) {
#ifdef PRINT_ERR_MSG
        FPRINTF(stderr, "MPI_File_iread: Only an integral number of etypes can be accessed\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ERR_ETYPE_FRACTIONAL,
				     myname, (char *) 0, (char *) 0);
	return ADIOI_Error(fh, error_code, myname);	    
#endif
    }

    if (fh->access_mode & MPI_MODE_WRONLY) {
#ifdef PRINT_ERR_MSG
	FPRINTF(stderr, "MPI_File_iread: Can't read from a file opened with MPI_MODE_WRONLY\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_UNSUPPORTED_OPERATION, 
 		MPIR_ERR_MODE_WRONLY, myname, (char *) 0, (char *) 0);
	return ADIOI_Error(fh, error_code, myname);	    
#endif
    }

    if (fh->access_mode & MPI_MODE_SEQUENTIAL) {
#ifdef PRINT_ERR_MSG
	FPRINTF(stderr, "MPI_File_iread: Can't use this function because file was opened with MPI_MODE_SEQUENTIAL\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_UNSUPPORTED_OPERATION, 
                        MPIR_ERR_AMODE_SEQ, myname, (char *) 0, (char *) 0);
	return ADIOI_Error(fh, error_code, myname);
#endif
    }

    ADIOI_Datatype_iscontig(datatype, &buftype_is_contig);
    ADIOI_Datatype_iscontig(fh->filetype, &filetype_is_contig);

    /* contiguous or strided? */

    if (buftype_is_contig && filetype_is_contig) {
    /* convert count and offset to bytes */
	bufsize = datatype_size * count;
	if (!(fh->atomicity))
	    ADIO_IreadContig(fh, buf, count, datatype, ADIO_INDIVIDUAL, 0,
			request, &error_code);
	else {
	    /* to maintain strict atomicity semantics with other concurrent
              operations, lock (exclusive) and call blocking routine */

	    *request = ADIOI_Malloc_request();
	    (*request)->optype = ADIOI_READ;
	    (*request)->fd = fh;
            (*request)->datatype = datatype;
	    (*request)->queued = 0;
	    (*request)->handle = 0;
	    
	    off = fh->fp_ind;
	    if ((fh->file_system != ADIO_PIOFS) && 
	       (fh->file_system != ADIO_NFS) && (fh->file_system != ADIO_PVFS))
		ADIOI_WRITE_LOCK(fh, off, SEEK_SET, bufsize);
		
	    ADIO_ReadContig(fh, buf, count, datatype, ADIO_INDIVIDUAL, 0, 
                    &status, &error_code);  

	    if ((fh->file_system != ADIO_PIOFS) && 
	       (fh->file_system != ADIO_NFS) && (fh->file_system != ADIO_PVFS))
		ADIOI_UNLOCK(fh, off, SEEK_SET, bufsize);

	    fh->async_count++;
	    /* status info. must be linked to the request structure, so that it
	       can be accessed later from a wait */
	}
    }
    else ADIO_IreadStrided(fh, buf, count, datatype, ADIO_INDIVIDUAL,
			  0, request, &error_code); 
    
#ifdef MPI_hpux
    HPMP_IO_END(fl_xmpi, fh, datatype, count);
#endif /* MPI_hpux */
    return error_code;
}
