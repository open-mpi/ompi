/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: iread_sh.c,v 1.7 2002/10/24 15:54:41 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_iread_shared = PMPI_File_iread_shared
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_iread_shared MPI_File_iread_shared
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_iread_shared as PMPI_File_iread_shared
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_iread_shared - Nonblocking read using shared file pointer

Input Parameters:
. fh - file handle (handle)
. count - number of elements in buffer (nonnegative integer)
. datatype - datatype of each buffer element (handle)

Output Parameters:
. buf - initial address of buffer (choice)
. request - request object (handle)

.N fortran
@*/
int MPI_File_iread_shared(MPI_File fh, void *buf, int count, 
                          MPI_Datatype datatype, MPIO_Request *request)
{
    int error_code, bufsize, buftype_is_contig, filetype_is_contig;
#ifndef PRINT_ERR_MSG
    static char myname[] = "MPI_FILE_IREAD_SHARED";
#endif
    int datatype_size, incr;
    ADIO_Status status;
    ADIO_Offset off, shared_fp;

#ifdef PRINT_ERR_MSG
    if ((fh <= (MPI_File) 0) || (fh->cookie != ADIOI_FILE_COOKIE)) {
	FPRINTF(stderr, "MPI_File_iread_shared: Invalid file handle\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
#else
    ADIOI_TEST_FILE_HANDLE(fh, myname);
#endif

    if (count < 0) {
#ifdef PRINT_ERR_MSG
	FPRINTF(stderr, "MPI_File_iread_shared: Invalid count argument\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_ARG, MPIR_ERR_COUNT_ARG,
				     myname, (char *) 0, (char *) 0);
	return ADIOI_Error(fh, error_code, myname);
#endif
    }

    if (datatype == MPI_DATATYPE_NULL) {
#ifdef PRINT_ERR_MSG
        FPRINTF(stderr, "MPI_File_iread_shared: Invalid datatype\n");
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
        FPRINTF(stderr, "MPI_File_iread_shared: Only an integral number of etypes can be accessed\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ERR_ETYPE_FRACTIONAL,
				     myname, (char *) 0, (char *) 0);
	return ADIOI_Error(fh, error_code, myname);	    
#endif
    }

    if ((fh->file_system == ADIO_PIOFS) || (fh->file_system == ADIO_PVFS)) {
#ifdef PRINT_ERR_MSG
	FPRINTF(stderr, "MPI_File_iread_shared: Shared file pointer not supported on PIOFS and PVFS\n");
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
	FPRINTF(stderr, "MPI_File_iread_shared: Error! Could not access shared file pointer.\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    /* contiguous or strided? */
    if (buftype_is_contig && filetype_is_contig) {
    /* convert count and shared_fp to bytes */
	bufsize = datatype_size * count;
	off = fh->disp + fh->etype_size * shared_fp;
        if (!(fh->atomicity))
	    ADIO_IreadContig(fh, buf, count, datatype, ADIO_EXPLICIT_OFFSET,
			off, request, &error_code); 
        else {
            /* to maintain strict atomicity semantics with other concurrent
              operations, lock (exclusive) and call blocking routine */

            *request = ADIOI_Malloc_request();
            (*request)->optype = ADIOI_READ;
            (*request)->fd = fh;
            (*request)->datatype = datatype;
            (*request)->queued = 0;
	    (*request)->handle = 0;

            if (fh->file_system != ADIO_NFS)
                ADIOI_WRITE_LOCK(fh, off, SEEK_SET, bufsize);

            ADIO_ReadContig(fh, buf, count, datatype, ADIO_EXPLICIT_OFFSET, off, 
                    &status, &error_code);  

            if (fh->file_system != ADIO_NFS)
                ADIOI_UNLOCK(fh, off, SEEK_SET, bufsize);

            fh->async_count++;
            /* status info. must be linked to the request structure, so that it
               can be accessed later from a wait */
        }
    }
    else ADIO_IreadStrided(fh, buf, count, datatype, ADIO_EXPLICIT_OFFSET,
			   shared_fp, request, &error_code); 

    return error_code;
}
