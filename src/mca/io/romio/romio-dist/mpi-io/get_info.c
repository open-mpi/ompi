/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: get_info.c,v 1.8 2002/10/24 17:01:17 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_get_info = PMPI_File_get_info
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_get_info MPI_File_get_info
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_get_info as PMPI_File_get_info
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_get_info - Returns the hints for a file that are actually being used by MPI

Input Parameters:
. fh - file handle (handle)

Output Parameters:
. info_used - info object (handle)

.N fortran
@*/
int MPI_File_get_info(MPI_File fh, MPI_Info *info_used)
{
#ifndef PRINT_ERR_MSG
    int error_code;
    static char myname[] = "MPI_FILE_GET_INFO";
#endif

#ifdef PRINT_ERR_MSG
    if ((fh <= (MPI_File) 0) || (fh->cookie != ADIOI_FILE_COOKIE)) {
	FPRINTF(stderr, "MPI_File_get_info: Invalid file handle\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
#else
    ADIOI_TEST_FILE_HANDLE(fh, myname);
#endif

    return MPI_Info_dup(fh->info, info_used);
}
