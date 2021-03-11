/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_write_ordered_begin = PMPI_File_write_ordered_begin
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_write_ordered_begin MPI_File_write_ordered_begin
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_write_ordered_begin as PMPI_File_write_ordered_begin
/* end of weak pragmas */
#elif defined(HAVE_WEAK_ATTRIBUTE)
int MPI_File_write_ordered_begin(MPI_File fh, const void *buf, int count, MPI_Datatype datatype)
    __attribute__((weak,alias("PMPI_File_write_ordered_begin")));
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_write_ordered_begin - Begin a split collective write using shared file pointer

Input Parameters:
. fh - file handle (handle)
. count - number of elements in buffer (nonnegative integer)
. datatype - datatype of each buffer element (handle)

Output Parameters:
. buf - initial address of buffer (choice)

.N fortran
@*/
int MPI_File_write_ordered_begin(MPI_File fh, ROMIO_CONST void *buf, int count,
				 MPI_Datatype datatype)
{
    int error_code, nprocs, myrank;
    ADIO_Offset incr;
    MPI_Count datatype_size;
    int source, dest;
    static char myname[] = "MPI_FILE_WRITE_ORDERED_BEGIN";
    ADIO_Offset shared_fp;
    ADIO_File adio_fh;
    void *e32buf = NULL;
    const void *xbuf=NULL;

    ROMIO_THREAD_CS_ENTER();

    adio_fh = MPIO_File_resolve(fh);

    /* --BEGIN ERROR HANDLING-- */
    MPIO_CHECK_FILE_HANDLE(adio_fh, myname, error_code);
    MPIO_CHECK_COUNT(adio_fh, count, myname, error_code);
    MPIO_CHECK_DATATYPE(adio_fh, datatype, myname, error_code);

    if (adio_fh->split_coll_count)
    {
	error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					  myname, __LINE__, MPI_ERR_IO, 
					  "**iosplitcoll", 0);
	error_code = MPIO_Err_return_file(adio_fh, error_code);
	goto fn_exit;
    }
    /* --END ERROR HANDLING-- */

    adio_fh->split_coll_count = 1;

    MPI_Type_size_x(datatype, &datatype_size);
    /* --BEGIN ERROR HANDLING-- */
    MPIO_CHECK_INTEGRAL_ETYPE(adio_fh, count, datatype_size, myname, error_code);
    MPIO_CHECK_FS_SUPPORTS_SHARED(adio_fh, myname, error_code);
    MPIO_CHECK_COUNT_SIZE(adio_fh, count, datatype_size, myname, error_code);
    /* --END ERROR HANDLING-- */

    ADIOI_TEST_DEFERRED(adio_fh, myname, &error_code);

    MPI_Comm_size(adio_fh->comm, &nprocs);
    MPI_Comm_rank(adio_fh->comm, &myrank);

    incr = (count*datatype_size)/adio_fh->etype_size;
    /* Use a message as a 'token' to order the operations */
    source = myrank - 1;
    dest   = myrank + 1;
    if (source < 0) source = MPI_PROC_NULL;
    if (dest >= nprocs) dest = MPI_PROC_NULL;
    MPI_Recv(NULL, 0, MPI_BYTE, source, 0, adio_fh->comm, MPI_STATUS_IGNORE);

    ADIO_Get_shared_fp(adio_fh, incr, &shared_fp, &error_code);
    /* --BEGIN ERROR HANDLING-- */
    if (error_code != MPI_SUCCESS)
    {
	error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL,
					  myname, __LINE__, MPI_ERR_INTERN, 
					  "**iosharedfailed", 0);
	error_code = MPIO_Err_return_file(adio_fh, error_code);
	goto fn_exit;
    }
    /* --END ERROR HANDLING-- */

    MPI_Send(NULL, 0, MPI_BYTE, dest, 0, adio_fh->comm);

    xbuf = buf;
    if (adio_fh->is_external32) {
	error_code = MPIU_external32_buffer_setup(buf, count, datatype, &e32buf);
	if (error_code != MPI_SUCCESS) 
	    goto fn_exit;

	xbuf = e32buf;
    }

    ADIO_WriteStridedColl(adio_fh, xbuf, count, datatype, ADIO_EXPLICIT_OFFSET,
			  shared_fp, &adio_fh->split_status, &error_code);

    /* --BEGIN ERROR HANDLING-- */
    if (error_code != MPI_SUCCESS)
	error_code = MPIO_Err_return_file(adio_fh, error_code);
    /* --END ERROR HANDLING-- */

fn_exit:
    ROMIO_THREAD_CS_EXIT();

    /* FIXME: Check for error code from WriteStridedColl? */
    return error_code;
}
