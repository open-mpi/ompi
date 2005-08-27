/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: get_extent.c,v 1.8 2002/10/24 15:54:40 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_get_type_extent = PMPI_File_get_type_extent
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_get_type_extent MPI_File_get_type_extent
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_get_type_extent as PMPI_File_get_type_extent
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_get_type_extent - Returns the extent of datatype in the file

Input Parameters:
. fh - file handle (handle)
. datatype - datatype (handle)

Output Parameters:
. extent - extent of the datatype (nonnegative integer)

.N fortran
@*/
int MPI_File_get_type_extent(MPI_File fh, MPI_Datatype datatype, 
                             MPI_Aint *extent)
{
#ifndef PRINT_ERR_MSG
    int error_code;
    static char myname[] = "MPI_FILE_GET_TYPE_EXTENT";
#endif

#ifdef PRINT_ERR_MSG
    if ((fh <= (MPI_File) 0) || (fh->cookie != ADIOI_FILE_COOKIE)) {
	FPRINTF(stderr, "MPI_File_get_type_extent: Invalid file handle\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
#else
    ADIOI_TEST_FILE_HANDLE(fh, myname);
#endif

    if (datatype == MPI_DATATYPE_NULL) {
#ifdef PRINT_ERR_MSG
        FPRINTF(stderr, "MPI_File_get_type_extent: Invalid datatype\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_TYPE, MPIR_ERR_TYPE_NULL,
				     myname, (char *) 0, (char *) 0);
	return ADIOI_Error(fh, error_code, myname);	    
#endif
    }

    return MPI_Type_extent(datatype, extent);
}
