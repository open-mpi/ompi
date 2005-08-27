/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: set_errh.c,v 1.3 2002/10/24 15:54:43 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"
#include "adio_extern.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_set_errhandler = PMPI_File_set_errhandler
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_set_errhandler MPI_File_set_errhandler
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_set_errhandler as PMPI_File_set_errhandler
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_set_errhandler - Sets the error handler for a file

Input Parameters:
. fh - file handle (handle)
. errhandler - error handler (handle)

.N fortran
@*/
int MPI_File_set_errhandler(MPI_File fh, MPI_Errhandler errhandler)
{
    int error_code = MPI_SUCCESS;
#ifndef PRINT_ERR_MSG
    static char myname[] = "MPI_FILE_SET_ERRHANDLER";
#endif

    if ((errhandler != MPI_ERRORS_RETURN) || (errhandler != MPI_ERRORS_ARE_FATAL)) {
	FPRINTF(stderr, "Only MPI_ERRORS_RETURN and MPI_ERRORS_ARE_FATAL are currently supported for MPI_File_set_errhandler\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    if (fh == MPI_FILE_NULL) ADIOI_DFLT_ERR_HANDLER = errhandler;
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
    else fh->err_handler = errhandler;

    return error_code;
}
