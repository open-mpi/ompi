/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: prealloc.c,v 1.8 2002/10/24 15:54:42 gropp Exp $    
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
    int error_code, mynod;
#ifndef PRINT_ERR_MSG
    static char myname[] = "MPI_FILE_PREALLOCATE";
#endif
    MPI_Offset tmp_sz;
#ifdef MPI_hpux
    int fl_xmpi;

    HPMP_IO_START(fl_xmpi, BLKMPIFILEPREALLOCATE, TRDTBLOCK,
		  fh, MPI_DATATYPE_NULL, -1);
#endif /* MPI_hpux */

#ifdef PRINT_ERR_MSG
    if ((fh <= (MPI_File) 0) || (fh->cookie != ADIOI_FILE_COOKIE)) {
	FPRINTF(stderr, "MPI_File_preallocate: Invalid file handle\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
#else
    ADIOI_TEST_FILE_HANDLE(fh, myname);
#endif

    if (size < 0) {
#ifdef PRINT_ERR_MSG
        FPRINTF(stderr, "MPI_File_preallocate: Invalid size argument\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_ARG, MPIR_ERR_SIZE_ARG,
				     myname, (char *) 0, (char *) 0);
	return ADIOI_Error(fh, error_code, myname);
#endif
    }

    tmp_sz = size;
    MPI_Bcast(&tmp_sz, 1, ADIO_OFFSET, 0, fh->comm);

    if (tmp_sz != size) {
#ifdef PRINT_ERR_MSG
        FPRINTF(stderr, "MPI_File_preallocate: size argument must be the same on all processes\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_ARG, MPIR_ERR_SIZE_ARG_NOT_SAME,
				     myname, (char *) 0, (char *) 0);
	return ADIOI_Error(fh, error_code, myname);
#endif
    }

    if (size == 0) return MPI_SUCCESS;

    MPI_Comm_rank(fh->comm, &mynod);
    if (!mynod) {
	fcntl_struct = (ADIO_Fcntl_t *) ADIOI_Malloc(sizeof(ADIO_Fcntl_t));
	fcntl_struct->diskspace = size;
	ADIO_Fcntl(fh, ADIO_FCNTL_SET_DISKSPACE, fcntl_struct, &error_code);
	ADIOI_Free(fcntl_struct);
    }
    MPI_Barrier(fh->comm);
    
#ifdef MPI_hpux
    HPMP_IO_END(fl_xmpi, fh, MPI_DATATYPE_NULL, -1);
#endif /* MPI_hpux */
    if (!mynod) return error_code;
    else return MPI_SUCCESS;
}
