/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: file_f2c.c,v 1.9 2002/10/24 15:54:39 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_f2c = PMPI_File_f2c
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_f2c MPI_File_f2c
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_f2c as PMPI_File_f2c
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif
#include "adio_extern.h"

/*@
    MPI_File_f2c - Translates a Fortran file handle to a C file handle

Input Parameters:
. fh - Fortran file handle (integer)

Return Value:
  C file handle (handle)
@*/
MPI_File MPI_File_f2c(MPI_Fint fh)
{
#ifndef INT_LT_POINTER
    return (MPI_File) ((void *) fh);  
    /* the extra cast is to get rid of a compiler warning on Exemplar.
       The warning is because MPI_File points to a structure containing
       longlongs, which may be 8-byte aligned. But MPI_Fint itself
       may not be 8-byte aligned.*/
#else
    if (!fh) return MPI_FILE_NULL;
    if ((fh < 0) || (fh > ADIOI_Ftable_ptr)) {
	FPRINTF(stderr, "MPI_File_f2c: Invalid file handle\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
    return ADIOI_Ftable[fh];
#endif
}
