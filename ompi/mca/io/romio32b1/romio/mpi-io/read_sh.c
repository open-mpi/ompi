/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
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
#elif defined(HAVE_WEAK_ATTRIBUTE)
int MPI_File_read_shared(MPI_File fh, void *buf, int count, MPI_Datatype datatype,
                         MPI_Status *status) __attribute__((weak,alias("PMPI_File_read_shared")));
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
    int error_code, buftype_is_contig, filetype_is_contig;
    static char myname[] = "MPI_FILE_READ_SHARED";
    MPI_Count datatype_size;
    ADIO_Offset off, shared_fp, incr, bufsize;
    ADIO_File adio_fh;
    void *xbuf=NULL, *e32_buf=NULL;

    MPIU_THREAD_CS_ENTER(ALLFUNC,);

    adio_fh = MPIO_File_resolve(fh);

    /* --BEGIN ERROR HANDLING-- */
    MPIO_CHECK_FILE_HANDLE(adio_fh, myname, error_code);
    MPIO_CHECK_COUNT(adio_fh, count, myname, error_code);
    MPIO_CHECK_DATATYPE(adio_fh, datatype, myname, error_code);
    /* --END ERROR HANDLING-- */

    MPI_Type_size_x(datatype, &datatype_size);

    /* --BEGIN ERROR HANDLING-- */
    MPIO_CHECK_COUNT_SIZE(adio_fh, count, datatype_size, myname, error_code);
    /* --END ERROR HANDLING-- */

    if (count*datatype_size == 0)
    {
#ifdef HAVE_STATUS_SET_BYTES
	MPIR_Status_set_bytes(status, datatype, 0);
#endif
	error_code = MPI_SUCCESS;
	goto fn_exit;
    }

    /* --BEGIN ERROR HANDLING-- */
    MPIO_CHECK_INTEGRAL_ETYPE(adio_fh, count, datatype_size, myname, error_code);
    MPIO_CHECK_READABLE(adio_fh, myname, error_code);
    MPIO_CHECK_FS_SUPPORTS_SHARED(adio_fh, myname, error_code);
    /* --END ERROR HANDLING-- */

    ADIOI_Datatype_iscontig(datatype, &buftype_is_contig);
    ADIOI_Datatype_iscontig(adio_fh->filetype, &filetype_is_contig);

    ADIOI_TEST_DEFERRED(adio_fh, myname, &error_code);

    incr = (count*datatype_size)/adio_fh->etype_size;

    ADIO_Get_shared_fp(adio_fh, incr, &shared_fp, &error_code);
    /* --BEGIN ERROR HANDLING-- */
    if (error_code != MPI_SUCCESS)
    {
        error_code = MPIO_Err_return_file(adio_fh, error_code);
	goto fn_exit;
    }
    /* --END ERROR HANDLING-- */

    xbuf = buf;
    if (adio_fh->is_external32)
    {
        MPI_Aint e32_size = 0;
        error_code = MPIU_datatype_full_size(datatype, &e32_size);
        if (error_code != MPI_SUCCESS)
            goto fn_exit;

        e32_buf = ADIOI_Malloc(e32_size*count);
	xbuf = e32_buf;
    }

    /* contiguous or strided? */
    if (buftype_is_contig && filetype_is_contig)
    {
	/* convert count and shared_fp to bytes */
        bufsize = datatype_size * count;
        off = adio_fh->disp + adio_fh->etype_size * shared_fp;

        /* if atomic mode requested, lock (exclusive) the region, because there
           could be a concurrent noncontiguous request. On NFS, locking 
           is done in the ADIO_ReadContig.*/

        if ((adio_fh->atomicity) && (adio_fh->file_system != ADIO_NFS))
            ADIOI_WRITE_LOCK(adio_fh, off, SEEK_SET, bufsize);

        ADIO_ReadContig(adio_fh, xbuf, count, datatype, ADIO_EXPLICIT_OFFSET,
                        off, status, &error_code); 

        if ((adio_fh->atomicity) && (adio_fh->file_system != ADIO_NFS))
            ADIOI_UNLOCK(adio_fh, off, SEEK_SET, bufsize);
    }
    else
    {
	ADIO_ReadStrided(adio_fh, xbuf, count, datatype, ADIO_EXPLICIT_OFFSET,
                          shared_fp, status, &error_code);
	/* For strided and atomic mode, locking is done in ADIO_ReadStrided */
    }

    /* --BEGIN ERROR HANDLING-- */
    if (error_code != MPI_SUCCESS)
	error_code = MPIO_Err_return_file(adio_fh, error_code);
    /* --END ERROR HANDLING-- */

    if (e32_buf != NULL) {
        error_code = MPIU_read_external32_conversion_fn(xbuf, datatype,
                count, e32_buf);
	ADIOI_Free(e32_buf);
    }
fn_exit:
    MPIU_THREAD_CS_EXIT(ALLFUNC,);

    return error_code;
}
