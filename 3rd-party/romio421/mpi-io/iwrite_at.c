/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "mpioimpl.h"
#include <limits.h>
#include <assert.h>

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_iwrite_at = PMPI_File_iwrite_at
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_iwrite_at MPI_File_iwrite_at
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_iwrite_at as PMPI_File_iwrite_at
/* end of weak pragmas */
#elif defined(HAVE_WEAK_ATTRIBUTE)
int MPI_File_iwrite_at(MPI_File fh, MPI_Offset offset, const void *buf, int count,
                       MPI_Datatype datatype, MPIO_Request * request)
    __attribute__ ((weak, alias("PMPI_File_iwrite_at")));
#endif

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_iwrite_at_c = PMPI_File_iwrite_at_c
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_iwrite_at_c MPI_File_iwrite_at_c
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_iwrite_at_c as PMPI_File_iwrite_at_c
/* end of weak pragmas */
#elif defined(HAVE_WEAK_ATTRIBUTE)
int MPI_File_iwrite_at_c(MPI_File fh, MPI_Offset offset, const void *buf, MPI_Count count,
                         MPI_Datatype datatype, MPIO_Request * request)
    __attribute__ ((weak, alias("PMPI_File_iwrite_at_c")));
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_iwrite_at - Nonblocking write using explicit offset

Input Parameters:
. fh - file handle (handle)
. offset - file offset (nonnegative integer)
. buf - initial address of buffer (choice)
. count - number of elements in buffer (nonnegative integer)
. datatype - datatype of each buffer element (handle)

Output Parameters:
. request - request object (handle)

.N fortran
@*/
#ifdef HAVE_MPI_GREQUEST
#include "mpiu_greq.h"
#endif

int MPI_File_iwrite_at(MPI_File fh, MPI_Offset offset, ROMIO_CONST void *buf,
                       int count, MPI_Datatype datatype, MPIO_Request * request)
{
    int error_code;
    ADIO_File adio_fh;
    static char myname[] = "MPI_FILE_IWRITE_AT";

#ifdef MPI_hpux
    int fl_xmpi;

    HPMP_IO_START(fl_xmpi, BLKMPIFILEIWRITEAT, TRDTSYSTEM, fh, datatype, count);
#endif /* MPI_hpux */


    adio_fh = MPIO_File_resolve(fh);

    error_code = MPIOI_File_iwrite(adio_fh, offset, ADIO_EXPLICIT_OFFSET, buf,
                                   count, datatype, myname, request);

    /* --BEGIN ERROR HANDLING-- */
    if (error_code != MPI_SUCCESS)
        error_code = MPIO_Err_return_file(adio_fh, error_code);
    /* --END ERROR HANDLING-- */

#ifdef MPI_hpux
    HPMP_IO_END(fl_xmpi, fh, datatype, count)
#endif /* MPI_hpux */
        return error_code;
}

/* large count function */


/*@
    MPI_File_iwrite_at_c - Nonblocking write using explicit offset

Input Parameters:
. fh - file handle (handle)
. offset - file offset (nonnegative integer)
. buf - initial address of buffer (choice)
. count - number of elements in buffer (nonnegative integer)
. datatype - datatype of each buffer element (handle)

Output Parameters:
. request - request object (handle)

.N fortran
@*/
#ifdef HAVE_MPI_GREQUEST
#include "mpiu_greq.h"
#endif

int MPI_File_iwrite_at_c(MPI_File fh, MPI_Offset offset, ROMIO_CONST void *buf,
                         MPI_Count count, MPI_Datatype datatype, MPIO_Request * request)
{
    int error_code;
    ADIO_File adio_fh;
    static char myname[] = "MPI_FILE_IWRITE_AT";

#ifdef MPI_hpux
    int fl_xmpi;

    HPMP_IO_START(fl_xmpi, BLKMPIFILEIWRITEAT, TRDTSYSTEM, fh, datatype, count);
#endif /* MPI_hpux */


    adio_fh = MPIO_File_resolve(fh);

    error_code = MPIOI_File_iwrite(adio_fh, offset, ADIO_EXPLICIT_OFFSET, buf,
                                   count, datatype, myname, request);

    /* --BEGIN ERROR HANDLING-- */
    if (error_code != MPI_SUCCESS)
        error_code = MPIO_Err_return_file(adio_fh, error_code);
    /* --END ERROR HANDLING-- */

#ifdef MPI_hpux
    HPMP_IO_END(fl_xmpi, fh, datatype, count)
#endif /* MPI_hpux */
        return error_code;
}
