/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: set_info.c,v 1.8 2002/10/24 15:54:43 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_set_info = PMPI_File_set_info
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_set_info MPI_File_set_info
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_set_info as PMPI_File_set_info
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_set_info - Sets new values for the hints associated with a file

Input Parameters:
. fh - file handle (handle)
. info - info object (handle)

.N fortran
@*/
int MPI_File_set_info(MPI_File fh, MPI_Info info)
{
    int error_code;
#ifndef PRINT_ERR_MSG
    static char myname[] = "MPI_FILE_SET_INFO";
#endif

#ifdef PRINT_ERR_MSG
    if ((fh <= (MPI_File) 0) || (fh->cookie != ADIOI_FILE_COOKIE)) {
	FPRINTF(stderr, "MPI_File_set_info: Invalid file handle\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
#else
    ADIOI_TEST_FILE_HANDLE(fh, myname);
#endif

    /* set new info */
    ADIO_SetInfo(fh, info, &error_code);

    return error_code;
}
