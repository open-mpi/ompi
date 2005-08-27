/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: get_atom.c,v 1.8 2002/10/24 15:54:39 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_get_atomicity = PMPI_File_get_atomicity
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_get_atomicity MPI_File_get_atomicity
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_get_atomicity as PMPI_File_get_atomicity
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_get_atomicity - Returns the atomicity mode

Input Parameters:
. fh - file handle (handle)

Output Parameters:
. flag - true if atomic mode, false if nonatomic mode (logical)

.N fortran
@*/
int MPI_File_get_atomicity(MPI_File fh, int *flag)
{
#ifndef PRINT_ERR_MSG
    int error_code;
    static char myname[] = "MPI_FILE_GET_ATOMICITY";
#endif

#ifdef PRINT_ERR_MSG
    if ((fh <= (MPI_File) 0) || (fh->cookie != ADIOI_FILE_COOKIE)) {
	FPRINTF(stderr, "MPI_File_get_atomicity: Invalid file handle\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
#else
    ADIOI_TEST_FILE_HANDLE(fh, myname);
#endif

    *flag = fh->atomicity;
    return MPI_SUCCESS;
}
