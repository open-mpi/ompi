/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_iwrite_shared = PMPI_File_iwrite_shared
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_iwrite_shared MPI_File_iwrite_shared
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_iwrite_shared as PMPI_File_iwrite_shared
/* end of weak pragmas */
#elif defined(HAVE_WEAK_ATTRIBUTE)
int MPI_File_iwrite_shared(MPI_File fh, const void *buf, int count, MPI_Datatype datatype,
                           MPIO_Request *request) __attribute__((weak,alias("PMPI_File_iwrite_shared")));
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_iwrite_shared - Nonblocking write using shared file pointer

Input Parameters:
. fh - file handle (handle)
. buf - initial address of buffer (choice)
. count - number of elements in buffer (nonnegative integer)
. datatype - datatype of each buffer element (handle)

Output Parameters:
. request - request object (handle)

.N fortran
@*/
#ifdef HAVE_MPI_GREQUEST
#include "mpiu_greq.h"
#endif

int MPI_File_iwrite_shared(MPI_File fh, ROMIO_CONST void *buf, int count,
			   MPI_Datatype datatype, MPIO_Request *request)
{
    int error_code, buftype_is_contig, filetype_is_contig;
    ADIO_File adio_fh;
    ADIO_Offset incr, bufsize;
    MPI_Count datatype_size;
    ADIO_Status status;
    ADIO_Offset off, shared_fp;
    static char myname[] = "MPI_FILE_IWRITE_SHARED";

    MPIU_THREAD_CS_ENTER(ALLFUNC,);

    adio_fh = MPIO_File_resolve(fh);

    /* --BEGIN ERROR HANDLING-- */
    MPIO_CHECK_FILE_HANDLE(adio_fh, myname, error_code);
    MPIO_CHECK_COUNT(adio_fh, count, myname, error_code);
    MPIO_CHECK_DATATYPE(adio_fh, datatype, myname, error_code);
    /* --END ERROR HANDLING-- */

    MPI_Type_size_x(datatype, &datatype_size);

    /* --BEGIN ERROR HANDLING-- */
    MPIO_CHECK_INTEGRAL_ETYPE(adio_fh, count, datatype_size, myname, error_code);
    MPIO_CHECK_FS_SUPPORTS_SHARED(adio_fh, myname, error_code);
    MPIO_CHECK_COUNT_SIZE(adio_fh, count, datatype_size, myname, error_code);
    /* --END ERROR HANDLING-- */

    ADIOI_Datatype_iscontig(datatype, &buftype_is_contig);
    ADIOI_Datatype_iscontig(adio_fh->filetype, &filetype_is_contig);

    ADIOI_TEST_DEFERRED(adio_fh, myname, &error_code);

    incr = (count*datatype_size)/adio_fh->etype_size;
    ADIO_Get_shared_fp(adio_fh, incr, &shared_fp, &error_code);
    if (error_code != MPI_SUCCESS) {
	/* note: ADIO_Get_shared_fp should have set up error code already? */
	MPIO_Err_return_file(adio_fh, error_code);
    }

    /* contiguous or strided? */
    if (buftype_is_contig && filetype_is_contig) {
    /* convert sizes to bytes */
	bufsize = datatype_size * count;
	off = adio_fh->disp + adio_fh->etype_size * shared_fp;
        if (!(adio_fh->atomicity))
	    ADIO_IwriteContig(adio_fh, buf, count, datatype, ADIO_EXPLICIT_OFFSET,
		            off, request, &error_code); 
	else {
            /* to maintain strict atomicity semantics with other concurrent
              operations, lock (exclusive) and call blocking routine */

            if (adio_fh->file_system != ADIO_NFS)
                ADIOI_WRITE_LOCK(adio_fh, off, SEEK_SET, bufsize);

            ADIO_WriteContig(adio_fh, buf, count, datatype, ADIO_EXPLICIT_OFFSET,
			     off, &status, &error_code);  

            if (adio_fh->file_system != ADIO_NFS)
                ADIOI_UNLOCK(adio_fh, off, SEEK_SET, bufsize);

	    MPIO_Completed_request_create(&adio_fh, bufsize, &error_code, request);
	}
    }
    else
	ADIO_IwriteStrided(adio_fh, buf, count, datatype, ADIO_EXPLICIT_OFFSET,
			   shared_fp, request, &error_code); 

fn_exit:
    MPIU_THREAD_CS_EXIT(ALLFUNC,);

    return error_code;
}
