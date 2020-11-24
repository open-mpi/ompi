/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_set_size = PMPI_File_set_size
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_set_size MPI_File_set_size
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_set_size as PMPI_File_set_size
/* end of weak pragmas */
#elif defined(HAVE_WEAK_ATTRIBUTE)
int MPI_File_set_size(MPI_File fh, MPI_Offset size) __attribute__((weak,alias("PMPI_File_set_size")));
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_set_size - Sets the file size

Input Parameters:
. fh - file handle (handle)
. size - size to truncate or expand file (nonnegative integer)

.N fortran
@*/
int MPI_File_set_size(MPI_File fh, MPI_Offset size)
{
    int error_code;
    ADIO_File adio_fh;
    static char myname[] = "MPI_FILE_SET_SIZE";
    MPI_Offset tmp_sz, max_sz, min_sz;

#ifdef MPI_hpux
    int fl_xmpi;

    HPMP_IO_START(fl_xmpi, BLKMPIFILESETSIZE, TRDTBLOCK, adio_fh,
		  MPI_DATATYPE_NULL, -1);
#endif /* MPI_hpux */

    ROMIO_THREAD_CS_ENTER();

    adio_fh = MPIO_File_resolve(fh);

    /* --BEGIN ERROR HANDLING-- */
    MPIO_CHECK_FILE_HANDLE(adio_fh, myname, error_code);
    MPIO_CHECK_NOT_SEQUENTIAL_MODE(adio_fh, myname, error_code);

    if (size < 0) {
	error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					  myname, __LINE__, MPI_ERR_ARG,
					  "**iobadsize", 0);
	error_code = MPIO_Err_return_file(adio_fh, error_code);
	goto fn_exit;
    }
    MPIO_CHECK_WRITABLE(fh, myname, error_code);
    /* --END ERROR HANDLING-- */

    tmp_sz = size;
    MPI_Allreduce(&tmp_sz, &max_sz, 1, ADIO_OFFSET, MPI_MAX, adio_fh->comm);
    MPI_Allreduce(&tmp_sz, &min_sz, 1, ADIO_OFFSET, MPI_MIN, adio_fh->comm);

    /* --BEGIN ERROR HANDLING-- */
    if (max_sz != min_sz) {
	error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					  myname, __LINE__, MPI_ERR_ARG,
					  "**notsame", 0);
	error_code = MPIO_Err_return_file(adio_fh, error_code);
	goto fn_exit;
    }
    /* --END ERROR HANDLING-- */

    if (!ADIO_Feature(adio_fh, ADIO_SCALABLE_RESIZE)) {
	/* rare stupid file systems (like NFS) need to carry out resize on all
	 * processes */
	ADIOI_TEST_DEFERRED(adio_fh, "MPI_File_set_size", &error_code);
    }

    ADIO_Resize(adio_fh, size, &error_code);
    /* TODO: what to do with error code? */
    
    /* --BEGIN ERROR HANDLING-- */
    if (error_code != MPI_SUCCESS)
	error_code = MPIO_Err_return_file(adio_fh, error_code);
    /* --END ERROR HANDLING-- */

#ifdef MPI_hpux
    HPMP_IO_END(fl_xmpi, adio_fh, MPI_DATATYPE_NULL, -1);
#endif /* MPI_hpux */

fn_exit:
    ROMIO_THREAD_CS_EXIT();

    return error_code;
}
