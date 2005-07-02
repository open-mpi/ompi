/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: get_view.c,v 1.8 2002/10/24 17:01:17 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_get_view = PMPI_File_get_view
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_get_view MPI_File_get_view
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_get_view as PMPI_File_get_view
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif
#ifdef MPISGI
#include "mpisgi2.h"
#endif

/*@
    MPI_File_get_view - Returns the file view

Input Parameters:
. fh - file handle (handle)

Output Parameters:
. disp - displacement (nonnegative integer)
. etype - elementary datatype (handle)
. filetype - filetype (handle)
. datarep - data representation (string)

.N fortran
@*/
int MPI_File_get_view(MPI_File fh, MPI_Offset *disp, MPI_Datatype *etype,
		 MPI_Datatype *filetype, char *datarep)
{
#ifndef PRINT_ERR_MSG
    int error_code;
    static char myname[] = "MPI_FILE_GET_VIEW";
#endif
    int i, j, k, combiner;
    MPI_Datatype copy_etype, copy_filetype;

#ifdef PRINT_ERR_MSG
    if ((fh <= (MPI_File) 0) || (fh->cookie != ADIOI_FILE_COOKIE)) {
	FPRINTF(stderr, "MPI_File_get_view: Invalid file handle\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
#else
    ADIOI_TEST_FILE_HANDLE(fh, myname);
#endif

    if (datarep <= (char *) 0) {
#ifdef PRINT_ERR_MSG
	FPRINTF(stderr, "MPI_File_get_view: The user must allocate memory for datarep\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_ARG, MPIR_ERR_DATAREP_ARG,
				     myname, (char *) 0, (char *) 0);
	return ADIOI_Error(fh, error_code, myname);
#endif
    }

    *disp = fh->disp;
    strcpy(datarep, "native");

    MPI_Type_get_envelope(fh->etype, &i, &j, &k, &combiner);
    if (combiner == MPI_COMBINER_NAMED) *etype = fh->etype;
    else {
        MPI_Type_contiguous(1, fh->etype, &copy_etype);
        MPI_Type_commit(&copy_etype);
        *etype = copy_etype;
    }
    MPI_Type_get_envelope(fh->filetype, &i, &j, &k, &combiner);
    if (combiner == MPI_COMBINER_NAMED) *filetype = fh->filetype;
    else {
        MPI_Type_contiguous(1, fh->filetype, &copy_filetype);
        MPI_Type_commit(&copy_filetype);
        *filetype = copy_filetype;
    }

    return MPI_SUCCESS;
}
