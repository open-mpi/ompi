/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_seek_shared = PMPI_File_seek_shared
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_seek_shared MPI_File_seek_shared
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_seek_shared as PMPI_File_seek_shared
/* end of weak pragmas */
#elif defined(HAVE_WEAK_ATTRIBUTE)
int MPI_File_seek_shared(MPI_File fh, MPI_Offset offset, int whence) __attribute__((weak,alias("PMPI_File_seek_shared")));
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_seek_shared - Updates the shared file pointer

Input Parameters:
. fh - file handle (handle)
. offset - file offset (integer)
. whence - update mode (state)

.N fortran
@*/
int MPI_File_seek_shared(MPI_File fh, MPI_Offset offset, int whence)
{
    int error_code=MPI_SUCCESS, tmp_whence, myrank;
    static char myname[] = "MPI_FILE_SEEK_SHARED";
    MPI_Offset curr_offset, eof_offset, tmp_offset;
    ADIO_File adio_fh;

    MPIU_THREAD_CS_ENTER(ALLFUNC,);

    adio_fh = MPIO_File_resolve(fh);

    /* --BEGIN ERROR HANDLING-- */
    MPIO_CHECK_FILE_HANDLE(adio_fh, myname, error_code);
    MPIO_CHECK_NOT_SEQUENTIAL_MODE(adio_fh, myname, error_code);
    MPIO_CHECK_FS_SUPPORTS_SHARED(adio_fh, myname, error_code);
    /* --END ERROR HANDLING-- */

    tmp_offset = offset;
    MPI_Bcast(&tmp_offset, 1, ADIO_OFFSET, 0, adio_fh->comm);
    /* --BEGIN ERROR HANDLING-- */
    if (tmp_offset != offset)
    {
	error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					  myname, __LINE__, MPI_ERR_ARG,
					  "**notsame", 0);
	error_code = MPIO_Err_return_file(adio_fh, error_code);
	goto fn_exit;
    }
    /* --END ERROR HANDLING-- */

    tmp_whence = whence;
    MPI_Bcast(&tmp_whence, 1, MPI_INT, 0, adio_fh->comm);
    /* --BEGIN ERROR HANDLING-- */
    if (tmp_whence != whence)
    {
	error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					  myname, __LINE__, MPI_ERR_ARG,
					  "**iobadwhence", 0);
	error_code = MPIO_Err_return_file(adio_fh, error_code);
	goto fn_exit;
    }
    /* --END ERROR HANDLING-- */

    ADIOI_TEST_DEFERRED(adio_fh, "MPI_File_seek_shared", &error_code);

    MPI_Comm_rank(adio_fh->comm, &myrank);

    if (!myrank)
    {
	switch(whence)
	{
	case MPI_SEEK_SET:
	    /* --BEGIN ERROR HANDLING-- */
	    if (offset < 0)
	    {
		error_code = MPIO_Err_create_code(MPI_SUCCESS,
						  MPIR_ERR_RECOVERABLE,
						  myname, __LINE__,
						  MPI_ERR_ARG,
						  "**iobadoffset", 0);
		error_code = MPIO_Err_return_file(adio_fh, error_code);
		goto fn_exit;
	    }
	    /* --END ERROR HANDLING-- */
	    break;
	case MPI_SEEK_CUR:
	    /* get current location of shared file pointer */
	    ADIO_Get_shared_fp(adio_fh, 0, &curr_offset, &error_code);
	    /* --BEGIN ERROR HANDLING-- */
	    if (error_code != MPI_SUCCESS)
	    {
		error_code = MPIO_Err_create_code(MPI_SUCCESS,
						  MPIR_ERR_FATAL,
						  myname, __LINE__,
						  MPI_ERR_INTERN, 
						  "**iosharedfailed", 0);
		error_code = MPIO_Err_return_file(adio_fh, error_code);
		goto fn_exit;
	    }
	    /* --END ERROR HANDLING-- */
	    offset += curr_offset;
	    /* --BEGIN ERROR HANDLING-- */
	    if (offset < 0)
	    {
		error_code = MPIO_Err_create_code(MPI_SUCCESS,
						  MPIR_ERR_RECOVERABLE,
						  myname, __LINE__,
						  MPI_ERR_ARG,
						  "**ionegoffset", 0);
		error_code = MPIO_Err_return_file(adio_fh, error_code);
		goto fn_exit;
	    }
	    /* --END ERROR HANDLING-- */
	    break;
	case MPI_SEEK_END:
	    /* find offset corr. to end of file */
	    ADIOI_Get_eof_offset(adio_fh, &eof_offset);
	    offset += eof_offset;
	    /* --BEGIN ERROR HANDLING-- */
	    if (offset < 0)
	    {
		error_code = MPIO_Err_create_code(MPI_SUCCESS,
						  MPIR_ERR_RECOVERABLE,
						  myname, __LINE__,
						  MPI_ERR_ARG,
						  "**ionegoffset", 0);
		error_code = MPIO_Err_return_file(adio_fh, error_code);
		goto fn_exit;
	    }
	    /* --END ERROR HANDLING-- */
	    break;
	default:
	    /* --BEGIN ERROR HANDLING-- */
	    error_code = MPIO_Err_create_code(MPI_SUCCESS,
					      MPIR_ERR_RECOVERABLE,
					      myname, __LINE__, MPI_ERR_ARG,
					      "**iobadwhence", 0);
	    error_code = MPIO_Err_return_file(adio_fh, error_code);
	    goto fn_exit;
	    /* --END ERROR HANDLING-- */
	}

	ADIO_Set_shared_fp(adio_fh, offset, &error_code);
	/* --BEGIN ERROR HANDLING-- */
	if (error_code != MPI_SUCCESS)
	{
	    error_code = MPIO_Err_create_code(MPI_SUCCESS,
					      MPIR_ERR_FATAL,
					      myname, __LINE__,
					      MPI_ERR_INTERN, 
					      "**iosharedfailed", 0);
	    error_code = MPIO_Err_return_file(adio_fh, error_code);
	    goto fn_exit;
	}
	/* --END ERROR HANDLING-- */

    }

    /* FIXME: explain why the barrier is necessary */
    MPI_Barrier(adio_fh->comm);

    error_code = MPI_SUCCESS;

fn_exit:
    MPIU_THREAD_CS_EXIT(ALLFUNC,);

    return error_code;
}
