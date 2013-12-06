/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
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

#ifdef HAVE_MPI_GREQUEST
#include "mpiu_greq.h"

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
                          MPI_Datatype datatype, MPI_Request *request)
{
    int error_code, bufsize, buftype_is_contig, filetype_is_contig;
    ADIO_File adio_fh;
    static char myname[] = "MPI_FILE_IREAD_SHARED";
    int datatype_size, incr;
    MPI_Status status;
    ADIO_Offset off, shared_fp;
    MPI_Offset nbytes=0;

    MPIU_THREAD_CS_ENTER(ALLFUNC,);

    adio_fh = MPIO_File_resolve(fh);

    /* --BEGIN ERROR HANDLING-- */
    MPIO_CHECK_FILE_HANDLE(adio_fh, myname, error_code);
    MPIO_CHECK_COUNT(adio_fh, count, myname, error_code);
    MPIO_CHECK_DATATYPE(adio_fh, datatype, myname, error_code);
    /* --END ERROR HANDLING-- */

    MPI_Type_size(datatype, &datatype_size);

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

    /* --BEGIN ERROR HANDLING-- */
    if (error_code != MPI_SUCCESS)
    {
	/* note: ADIO_Get_shared_fp should have set up error code already? */
	MPIO_Err_return_file(adio_fh, error_code);
    }
    /* --END ERROR HANDLING-- */

    if (buftype_is_contig && filetype_is_contig)
    {
    /* convert count and shared_fp to bytes */
	bufsize = datatype_size * count;
	off = adio_fh->disp + adio_fh->etype_size * shared_fp;
        if (!(adio_fh->atomicity))
	{
	    ADIO_IreadContig(adio_fh, buf, count, datatype, ADIO_EXPLICIT_OFFSET,
			off, request, &error_code);
	}
        else
	{
            /* to maintain strict atomicity semantics with other concurrent
              operations, lock (exclusive) and call blocking routine */

            if (adio_fh->file_system != ADIO_NFS)
	    {
                ADIOI_WRITE_LOCK(adio_fh, off, SEEK_SET, bufsize);
	    }

            ADIO_ReadContig(adio_fh, buf, count, datatype, ADIO_EXPLICIT_OFFSET,
			    off, &status, &error_code);  

            if (adio_fh->file_system != ADIO_NFS)
	    {
                ADIOI_UNLOCK(adio_fh, off, SEEK_SET, bufsize);
	    }
	    if (error_code == MPI_SUCCESS){
		    nbytes = count * datatype_size;
	    }
	    MPIO_Completed_request_create(&adio_fh, nbytes, &error_code, request);
        }
    }
    else
    {
	ADIO_IreadStrided(adio_fh, buf, count, datatype, ADIO_EXPLICIT_OFFSET,
			   shared_fp, request, &error_code);
    }

    /* --BEGIN ERROR HANDLING-- */
    if (error_code != MPI_SUCCESS)
	error_code = MPIO_Err_return_file(adio_fh, error_code);
    /* --END ERROR HANDLING-- */

fn_exit:
    MPIU_THREAD_CS_EXIT(ALLFUNC,);
    return error_code;
}
#endif
