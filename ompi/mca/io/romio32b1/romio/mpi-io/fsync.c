/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_sync = PMPI_File_sync
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_sync MPI_File_sync
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_sync as PMPI_File_sync
/* end of weak pragmas */
#elif defined(HAVE_WEAK_ATTRIBUTE)
int MPI_File_sync(MPI_File fh) __attribute__((weak,alias("PMPI_File_sync")));
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_sync - Causes all previous writes to be transferred
                    to the storage device

Input Parameters:
. fh - file handle (handle)

.N fortran
@*/
int MPI_File_sync(MPI_File fh)
{
    int error_code;
    ADIO_File adio_fh;
    static char myname[] = "MPI_FILE_SYNC";
#ifdef MPI_hpux
    int fl_xmpi;

    HPMP_IO_START(fl_xmpi, BLKMPIFILESYNC, TRDTBLOCK, adio_fh,
		  MPI_DATATYPE_NULL, -1);
#endif /* MPI_hpux */
    MPIU_THREAD_CS_ENTER(ALLFUNC,);

    adio_fh = MPIO_File_resolve(fh);
    /* --BEGIN ERROR HANDLING-- */
    if ((adio_fh <= (MPI_File) 0) || ((adio_fh)->cookie != ADIOI_FILE_COOKIE))
    {
	error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					  myname, __LINE__, MPI_ERR_ARG,
					  "**iobadfh", 0);
	error_code = MPIO_Err_return_file(MPI_FILE_NULL, error_code);
	goto fn_exit;
    }
    MPIO_CHECK_WRITABLE(fh, myname, error_code);
    /* --END ERROR HANDLING-- */

    ADIO_Flush(adio_fh, &error_code);
    /* --BEGIN ERROR HANDLING-- */
    if (error_code != MPI_SUCCESS)
	error_code = MPIO_Err_return_file(adio_fh, error_code);
    /* --END ERROR HANDLING-- */

#ifdef MPI_hpux
    HPMP_IO_END(fl_xmpi, adio_fh, MPI_DATATYPE_NULL, -1);
#endif /* MPI_hpux */
 
fn_exit:
    MPIU_THREAD_CS_EXIT(ALLFUNC,);
    return error_code;
}
