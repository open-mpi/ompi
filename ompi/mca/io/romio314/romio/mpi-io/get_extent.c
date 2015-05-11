/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
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
#elif defined(HAVE_WEAK_ATTRIBUTE)
int MPI_File_get_type_extent(MPI_File fh, MPI_Datatype datatype, MPI_Aint *extent) __attribute__((weak,alias("PMPI_File_get_type_extent")));
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
int MPI_File_get_type_extent(MPI_File fh, MPI_Datatype datatype, MPI_Aint *extent)
{
    int error_code;
    ADIO_File adio_fh;
    static char myname[] = "MPI_FILE_GET_TYPE_EXTENT";

    adio_fh = MPIO_File_resolve(fh);

    /* --BEGIN ERROR HANDLING-- */
    MPIO_CHECK_FILE_HANDLE(adio_fh, myname, error_code);
    MPIO_CHECK_DATATYPE(adio_fh, datatype, myname, error_code);
    /* --END ERROR HANDLING-- */

    /* FIXME: handle other file data representations */

    error_code = MPI_Type_extent(datatype, extent);

fn_exit:
    return error_code;
}
