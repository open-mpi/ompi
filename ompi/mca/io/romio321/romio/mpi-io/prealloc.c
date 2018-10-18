/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_preallocate = PMPI_File_preallocate
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_preallocate MPI_File_preallocate
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_preallocate as PMPI_File_preallocate
/* end of weak pragmas */
#elif defined(HAVE_WEAK_ATTRIBUTE)
int MPI_File_preallocate(MPI_File fh, MPI_Offset size) __attribute__((weak,alias("PMPI_File_preallocate")));
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_preallocate - Preallocates storage space for a file

Input Parameters:
. fh - file handle (handle)
. size - size to preallocate (nonnegative integer)

.N fortran
@*/
int MPI_File_preallocate(MPI_File fh, MPI_Offset size)
{
    ADIO_Fcntl_t *fcntl_struct;
    int error_code=0, mynod=0;
    ADIO_File adio_fh;
    static char myname[] = "MPI_FILE_PREALLOCATE";
    MPI_Offset tmp_sz, max_sz, min_sz;
#ifdef MPI_hpux
    int fl_xmpi;

    HPMP_IO_START(fl_xmpi, BLKMPIFILEPREALLOCATE, TRDTBLOCK,
		  adio_fh, MPI_DATATYPE_NULL, -1);
#endif /* MPI_hpux */

    ROMIO_THREAD_CS_ENTER();

    adio_fh = MPIO_File_resolve(fh);

    /* --BEGIN ERROR HANDLING-- */
    MPIO_CHECK_FILE_HANDLE(adio_fh, myname, error_code);

    if (size < 0) {
	error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					  myname, __LINE__, MPI_ERR_ARG,
					  "**iobadsize", 0);
	error_code = MPIO_Err_return_file(adio_fh, error_code);
	goto fn_exit;
    }

    tmp_sz = size;
    MPI_Allreduce(&tmp_sz, &max_sz, 1, ADIO_OFFSET, MPI_MAX, adio_fh->comm);
    MPI_Allreduce(&tmp_sz, &min_sz, 1, ADIO_OFFSET, MPI_MIN, adio_fh->comm);

    if (max_sz != min_sz) {
	error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					  myname, __LINE__, MPI_ERR_ARG,
					  "**notsame", 0);
	error_code = MPIO_Err_return_file(adio_fh, error_code);
	goto fn_exit;
    }
    /* --END ERROR HANDLING-- */

    if (size == 0) goto fn_exit;

    ADIOI_TEST_DEFERRED(adio_fh, myname, &error_code);

    MPI_Comm_rank(adio_fh->comm, &mynod);
    if (!mynod) {
	fcntl_struct = (ADIO_Fcntl_t *) ADIOI_Malloc(sizeof(ADIO_Fcntl_t));
	fcntl_struct->diskspace = size;
	ADIO_Fcntl(adio_fh, ADIO_FCNTL_SET_DISKSPACE, fcntl_struct, &error_code);
	ADIOI_Free(fcntl_struct);
	/* --BEGIN ERROR HANDLING-- */
	if (error_code != MPI_SUCCESS)
	    error_code = MPIO_Err_return_file(adio_fh, error_code);
	/* --END ERROR HANDLING-- */
    }
    MPI_Barrier(adio_fh->comm);
    
#ifdef MPI_hpux
    HPMP_IO_END(fl_xmpi, adio_fh, MPI_DATATYPE_NULL, -1);
#endif /* MPI_hpux */


fn_exit:
    ROMIO_THREAD_CS_EXIT();

    /* TODO: bcast result? */
    if (!mynod) return error_code;
    else return MPI_SUCCESS;
}
