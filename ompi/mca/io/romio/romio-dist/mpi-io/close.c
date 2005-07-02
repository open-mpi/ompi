/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: close.c,v 1.8 2002/10/24 17:01:16 gropp Exp $    
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
#ifndef PRINT_ERR_MSG
    static char myname[] = "MPI_FILE_CLOSE";
#endif
#ifdef MPI_hpux
    int fl_xmpi;

    HPMP_IO_WSTART(fl_xmpi, BLKMPIFILECLOSE, TRDTBLOCK, *fh);
#endif /* MPI_hpux */

#ifdef PRINT_ERR_MSG
    if ((*fh <= (MPI_File) 0) || ((*fh)->cookie != ADIOI_FILE_COOKIE)) {
	FPRINTF(stderr, "MPI_File_close: Invalid file handle\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
#else
    ADIOI_TEST_FILE_HANDLE(*fh, myname);
#endif

    if (((*fh)->file_system != ADIO_PIOFS) && ((*fh)->file_system != ADIO_PVFS)) {
	ADIOI_Free((*fh)->shared_fp_fname);
	if ((*fh)->shared_fp_fd != ADIO_FILE_NULL)
	    ADIO_Close((*fh)->shared_fp_fd, &error_code);
    }

    ADIO_Close(*fh, &error_code);

    *fh = MPI_FILE_NULL;
#ifdef MPI_hpux
    HPMP_IO_WEND(fl_xmpi);
#endif /* MPI_hpux */
    return error_code;
}
