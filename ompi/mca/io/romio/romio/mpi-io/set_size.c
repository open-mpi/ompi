/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: set_size.c,v 1.8 2002/10/24 15:54:44 gropp Exp $    
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
#ifndef PRINT_ERR_MSG
    static char myname[] = "MPI_FILE_SET_SIZE";
#endif
    MPI_Offset tmp_sz;
#ifdef MPI_hpux
    int fl_xmpi;

    HPMP_IO_START(fl_xmpi, BLKMPIFILESETSIZE, TRDTBLOCK, fh,
		  MPI_DATATYPE_NULL, -1);
#endif /* MPI_hpux */

#ifdef PRINT_ERR_MSG
    if ((fh <= (MPI_File) 0) || (fh->cookie != ADIOI_FILE_COOKIE)) {
	FPRINTF(stderr, "MPI_File_set_size: Invalid file handle\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
#else
    ADIOI_TEST_FILE_HANDLE(fh, myname);
#endif

    if (size < 0) {
#ifdef PRINT_ERR_MSG
        FPRINTF(stderr, "MPI_File_set_size: Invalid size argument\n");
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
	FPRINTF(stderr, "MPI_File_set_size: size argument must be the same on all processes\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_ARG, MPIR_ERR_SIZE_ARG_NOT_SAME,
				     myname, (char *) 0, (char *) 0);
	return ADIOI_Error(fh, error_code, myname);
#endif
    }

    ADIO_Resize(fh, size, &error_code);
    
#ifdef MPI_hpux
    HPMP_IO_END(fl_xmpi, fh, MPI_DATATYPE_NULL, -1);
#endif /* MPI_hpux */

    return error_code;
}
