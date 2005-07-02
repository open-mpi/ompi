/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: get_posn_sh.c,v 1.7 2002/10/24 17:01:17 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_get_position_shared = PMPI_File_get_position_shared
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_get_position_shared MPI_File_get_position_shared
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_get_position_shared as PMPI_File_get_position_shared
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_get_position_shared - Returns the current position of the 
               shared file pointer in etype units relative to the current view

Input Parameters:
. fh - file handle (handle)

Output Parameters:
. offset - offset of shared file pointer (nonnegative integer)

.N fortran
@*/
int MPI_File_get_position_shared(MPI_File fh, MPI_Offset *offset)
{
    int error_code;
#ifndef PRINT_ERR_MSG
    static char myname[] = "MPI_FILE_GET_POSITION_SHARED";
#endif

#ifdef PRINT_ERR_MSG
    if ((fh <= (MPI_File) 0) || (fh->cookie != ADIOI_FILE_COOKIE)) {
	FPRINTF(stderr, "MPI_File_get_position_shared: Invalid file handle\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
#else
    ADIOI_TEST_FILE_HANDLE(fh, myname);
#endif

    if (fh->access_mode & MPI_MODE_SEQUENTIAL) {
#ifdef PRINT_ERR_MSG
        FPRINTF(stderr, "MPI_File_get_position_shared: Can't use this function because file was opened with MPI_MODE_SEQUENTIAL\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_UNSUPPORTED_OPERATION, MPIR_ERR_AMODE_SEQ,
				     myname, (char *) 0, (char *) 0);
	return ADIOI_Error(fh, error_code, myname);
#endif
    }

    if ((fh->file_system == ADIO_PIOFS) || (fh->file_system == ADIO_PVFS)) {
#ifdef PRINT_ERR_MSG
	FPRINTF(stderr, "MPI_File_get_position_shared: Shared file pointer not supported on PIOFS and PVFS\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_UNSUPPORTED_OPERATION, 
                    MPIR_ERR_NO_SHARED_FP, myname, (char *) 0, (char *) 0);
	return ADIOI_Error(fh, error_code, myname);
#endif
    }

    ADIO_Get_shared_fp(fh, 0, offset, &error_code);
    return error_code;
}
