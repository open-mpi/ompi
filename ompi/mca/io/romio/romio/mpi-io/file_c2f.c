/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: file_c2f.c,v 1.7 2002/10/24 15:54:39 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_c2f = PMPI_File_c2f
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_c2f MPI_File_c2f
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_c2f as PMPI_File_c2f
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif
#include "adio_extern.h"

/*@
    MPI_File_c2f - Translates a C file handle to a Fortran file handle

Input Parameters:
. fh - C file handle (handle)

Return Value:
  Fortran file handle (integer)
@*/
MPI_Fint MPI_File_c2f(MPI_File fh)
{
#ifndef INT_LT_POINTER
    return (MPI_Fint) fh;
#else
    int i;

    if ((fh <= (MPI_File) 0) || (fh->cookie != ADIOI_FILE_COOKIE))
	return (MPI_Fint) 0;
    if (!ADIOI_Ftable) {
	ADIOI_Ftable_max = 1024;
	ADIOI_Ftable = (MPI_File *)
	    ADIOI_Malloc(ADIOI_Ftable_max*sizeof(MPI_File)); 
        ADIOI_Ftable_ptr = 0;  /* 0 can't be used though, because 
                                  MPI_FILE_NULL=0 */
	for (i=0; i<ADIOI_Ftable_max; i++) ADIOI_Ftable[i] = MPI_FILE_NULL;
    }
    if (ADIOI_Ftable_ptr == ADIOI_Ftable_max-1) {
	ADIOI_Ftable = (MPI_File *) ADIOI_Realloc(ADIOI_Ftable, 
                           (ADIOI_Ftable_max+1024)*sizeof(MPI_File));
	for (i=ADIOI_Ftable_max; i<ADIOI_Ftable_max+1024; i++) 
	    ADIOI_Ftable[i] = MPI_FILE_NULL;
	ADIOI_Ftable_max += 1024;
    }
    ADIOI_Ftable_ptr++;
    ADIOI_Ftable[ADIOI_Ftable_ptr] = fh;
    return (MPI_Fint) ADIOI_Ftable_ptr;
#endif
}
