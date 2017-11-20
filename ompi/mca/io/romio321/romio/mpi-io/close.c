/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_close = PMPI_File_close
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_close MPI_File_close
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_close as PMPI_File_close
/* end of weak pragmas */
#elif defined(HAVE_WEAK_ATTRIBUTE)
int MPI_File_close(MPI_File *fh) __attribute__((weak,alias("PMPI_File_close")));
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_close - Closes a file

Input Parameters:
. fh - file handle (handle)

.N fortran
@*/
int MPI_File_close(MPI_File *fh)
{
    int error_code;
    ADIO_File adio_fh;
    static char myname[] = "MPI_FILE_CLOSE";
#ifdef MPI_hpux
    int fl_xmpi;

    HPMP_IO_WSTART(fl_xmpi, BLKMPIFILECLOSE, TRDTBLOCK, *adio_fh);
#endif /* MPI_hpux */

    ROMIO_THREAD_CS_ENTER();

    adio_fh = MPIO_File_resolve(*fh);

    /* --BEGIN ERROR HANDLING-- */
    MPIO_CHECK_FILE_HANDLE(adio_fh, myname, error_code);
    /* --END ERROR HANDLING-- */

    if (ADIO_Feature(adio_fh, ADIO_SHARED_FP))
    {
	ADIOI_Free((adio_fh)->shared_fp_fname);
	/* POSIX semantics say a deleted file remains available until all
	 * processes close the file.  But since when was NFS posix-compliant?
	 */
	/* this used to be gated by the lack of the UNLINK_AFTER_CLOSE feature,
	 * but a race condition in GPFS necessated this.  See ticket #2214 */
	MPI_Barrier((adio_fh)->comm);
	if ((adio_fh)->shared_fp_fd != ADIO_FILE_NULL) {
	    MPI_File *fh_shared = &(adio_fh->shared_fp_fd);
	    ADIO_Close((adio_fh)->shared_fp_fd, &error_code);
            MPIO_File_free(fh_shared);
	    /* --BEGIN ERROR HANDLING-- */
	    if (error_code != MPI_SUCCESS) goto fn_fail;
	    /* --END ERROR HANDLING-- */
	}
    }

    /* Because ROMIO expects the MPI library to provide error handler management
     * routines but it doesn't ever participate in MPI_File_close, we have to
     * somehow inform the MPI library that we no longer hold a reference to any
     * user defined error handler.  We do this by setting the errhandler at this
     * point to MPI_ERRORS_RETURN. */
    error_code = PMPI_File_set_errhandler(*fh, MPI_ERRORS_RETURN);
    if (error_code != MPI_SUCCESS) goto fn_fail;

    ADIO_Close(adio_fh, &error_code);
    MPIO_File_free(fh);
    /* --BEGIN ERROR HANDLING-- */
    if (error_code != MPI_SUCCESS) goto fn_fail;
    /* --END ERROR HANDLING-- */

#ifdef MPI_hpux
    HPMP_IO_WEND(fl_xmpi);
#endif /* MPI_hpux */

fn_exit:
    ROMIO_THREAD_CS_EXIT();
    return error_code;
fn_fail:
    /* --BEGIN ERROR HANDLING-- */
    error_code = MPIO_Err_return_file(adio_fh, error_code);
    goto fn_exit;
    /* --END ERROR HANDLING-- */
}
