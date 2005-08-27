/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: get_bytoff.c,v 1.8 2002/10/24 15:54:40 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_get_byte_offset = PMPI_File_get_byte_offset
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_get_byte_offset MPI_File_get_byte_offset
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_get_byte_offset as PMPI_File_get_byte_offset
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_get_byte_offset - Returns the absolute byte position in 
                the file corresponding to "offset" etypes relative to
                the current view

Input Parameters:
. fh - file handle (handle)
. offset - offset (nonnegative integer)

Output Parameters:
. disp - absolute byte position of offset (nonnegative integer)

.N fortran
@*/
int MPI_File_get_byte_offset(MPI_File fh, MPI_Offset offset, MPI_Offset *disp)
{
#ifndef PRINT_ERR_MSG
    int error_code;
    static char myname[] = "MPI_FILE_GET_BYTE_OFFSET";
#endif

#ifdef PRINT_ERR_MSG
    if ((fh <= (MPI_File) 0) || (fh->cookie != ADIOI_FILE_COOKIE)) {
	FPRINTF(stderr, "MPI_File_get_byte_offset: Invalid file handle\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
#else
    ADIOI_TEST_FILE_HANDLE(fh, myname);
#endif

    if (offset < 0) {
#ifdef PRINT_ERR_MSG
        FPRINTF(stderr, "MPI_File_get_byte_offset: Invalid offset argument\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_ARG, MPIR_ERR_OFFSET_ARG,
				     myname, (char *) 0, (char *) 0);
	return ADIOI_Error(fh, error_code, myname);	    
#endif
    }

    if (fh->access_mode & MPI_MODE_SEQUENTIAL) {
#ifdef PRINT_ERR_MSG
	FPRINTF(stderr, "MPI_File_get_byte_offset: Can't use this function because file was opened with MPI_MODE_SEQUENTIAL\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_UNSUPPORTED_OPERATION, MPIR_ERR_AMODE_SEQ,
				     myname, (char *) 0, (char *) 0);
	return ADIOI_Error(fh, error_code, myname);
#endif
    }

    ADIOI_Get_byte_offset(fh, offset, disp);
    return MPI_SUCCESS;
}
