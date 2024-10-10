/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
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
int MPI_File_get_type_extent(MPI_File fh, MPI_Datatype datatype, MPI_Aint * extent)
    __attribute__ ((weak, alias("PMPI_File_get_type_extent")));
#endif

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_get_type_extent_c = PMPI_File_get_type_extent_c
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_get_type_extent_c MPI_File_get_type_extent_c
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_get_type_extent_c as PMPI_File_get_type_extent_c
/* end of weak pragmas */
#elif defined(HAVE_WEAK_ATTRIBUTE)
int MPI_File_get_type_extent_c(MPI_File fh, MPI_Datatype datatype, MPI_Count * extent)
    __attribute__ ((weak, alias("PMPI_File_get_type_extent_c")));
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
int MPI_File_get_type_extent(MPI_File fh, MPI_Datatype datatype, MPI_Aint * extent)
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

    MPI_Aint lb;
    error_code = PMPI_Type_get_extent(datatype, &lb, extent);

  fn_exit:
    return error_code;
}

/* large count function */


/*@
    MPI_File_get_type_extent_c - Returns the extent of datatype in the file

Input Parameters:
. fh - file handle (handle)
. datatype - datatype (handle)

Output Parameters:
. extent - extent of the datatype (nonnegative integer)

.N fortran
@*/
int MPI_File_get_type_extent_c(MPI_File fh, MPI_Datatype datatype, MPI_Count * extent)
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

    MPI_Aint lb_i, extent_i;
    error_code = PMPI_Type_get_extent(datatype, &lb_i, &extent_i);
    *extent = extent_i;

  fn_exit:
    return error_code;
}
