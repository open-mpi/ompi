/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: get_errh.c,v 1.3 2002/10/24 15:54:40 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"
#include "adio_extern.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_get_errhandler = PMPI_File_get_errhandler
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_get_errhandler MPI_File_get_errhandler
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_get_errhandler as PMPI_File_get_errhandler
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_get_errhandler - Returns the error handler for a file

Input Parameters:
. fh - file handle (handle)

Output Parameters:
. errhandler - error handler (handle)

.N fortran
@*/
int MPI_File_get_errhandler(MPI_File fh, MPI_Errhandler *errhandler)
{
    int error_code = MPI_SUCCESS;
#ifndef PRINT_ERR_MSG
    static char myname[] = "MPI_FILE_GET_ERRHANDLER";
#endif

    if (fh == MPI_FILE_NULL) *errhandler = ADIOI_DFLT_ERR_HANDLER;
    else if (fh->cookie != ADIOI_FILE_COOKIE) {
#ifdef PRINT_ERR_MSG
	FPRINTF(stderr, "MPI_File_close: Invalid file handle\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_FILE, MPIR_ERR_FILE_CORRUPT, 
              myname, (char *) 0, (char *) 0);
	return ADIOI_Error(MPI_FILE_NULL, error_code, myname);
#endif
    }
    else *errhandler = fh->err_handler;

    return error_code;
}
