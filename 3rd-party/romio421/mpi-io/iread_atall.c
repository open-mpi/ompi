/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "mpioimpl.h"
#include <limits.h>
#include <assert.h>

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_iread_at_all = PMPI_File_iread_at_all
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_iread_at_all MPI_File_iread_at_all
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_iread_at_all as PMPI_File_iread_at_all
/* end of weak pragmas */
#elif defined(HAVE_WEAK_ATTRIBUTE)
int MPI_File_iread_at_all(MPI_File fh, MPI_Offset offset, void *buf, int count,
                          MPI_Datatype datatype, MPI_Request * request)
    __attribute__ ((weak, alias("PMPI_File_iread_at_all")));
#endif

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_iread_at_all_c = PMPI_File_iread_at_all_c
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_iread_at_all_c MPI_File_iread_at_all_c
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_iread_at_all_c as PMPI_File_iread_at_all_c
/* end of weak pragmas */
#elif defined(HAVE_WEAK_ATTRIBUTE)
int MPI_File_iread_at_all_c(MPI_File fh, MPI_Offset offset, void *buf, MPI_Count count,
                            MPI_Datatype datatype, MPI_Request * request)
    __attribute__ ((weak, alias("PMPI_File_iread_at_all_c")));
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

#ifdef HAVE_MPI_GREQUEST
#include "mpiu_greq.h"
#endif

/*@
    MPI_File_iread_at_all - Nonblocking collective read using explicit offset

Input Parameters:
. fh - file handle (handle)
. offset - file offset (nonnegative integer)
. count - number of elements in buffer (nonnegative integer)
. datatype - datatype of each buffer element (handle)

Output Parameters:
. buf - initial address of buffer (choice)
. request - request object (handle)

.N fortran
@*/
int MPI_File_iread_at_all(MPI_File fh, MPI_Offset offset, void *buf,
                          int count, MPI_Datatype datatype, MPI_Request * request)
{
    int error_code;
    static char myname[] = "MPI_FILE_IREAD_AT_ALL";
#ifdef MPI_hpux
    int fl_xmpi;

    HPMP_IO_START(fl_xmpi, BLKMPIFILEIREADATALL, TRDTBLOCK, fh, datatype, count);
#endif /* MPI_hpux */

    error_code = MPIOI_File_iread_all(fh, offset, ADIO_EXPLICIT_OFFSET, buf,
                                      count, datatype, myname, request);

    /* --BEGIN ERROR HANDLING-- */
    if (error_code != MPI_SUCCESS)
        error_code = MPIO_Err_return_file(fh, error_code);
    /* --END ERROR HANDLING-- */

#ifdef MPI_hpux
    HPMP_IO_END(fl_xmpi, fh, datatype, count);
#endif /* MPI_hpux */

    return error_code;
}

/* large count function */


#ifdef HAVE_MPI_GREQUEST
#include "mpiu_greq.h"
#endif

/*@
    MPI_File_iread_at_all_c - Nonblocking collective read using explicit offset

Input Parameters:
. fh - file handle (handle)
. offset - file offset (nonnegative integer)
. count - number of elements in buffer (nonnegative integer)
. datatype - datatype of each buffer element (handle)

Output Parameters:
. buf - initial address of buffer (choice)
. request - request object (handle)

.N fortran
@*/
int MPI_File_iread_at_all_c(MPI_File fh, MPI_Offset offset, void *buf,
                            MPI_Count count, MPI_Datatype datatype, MPI_Request * request)
{
    int error_code;
    static char myname[] = "MPI_FILE_IREAD_AT_ALL";
#ifdef MPI_hpux
    int fl_xmpi;

    HPMP_IO_START(fl_xmpi, BLKMPIFILEIREADATALL, TRDTBLOCK, fh, datatype, count);
#endif /* MPI_hpux */

    error_code = MPIOI_File_iread_all(fh, offset, ADIO_EXPLICIT_OFFSET, buf,
                                      count, datatype, myname, request);

    /* --BEGIN ERROR HANDLING-- */
    if (error_code != MPI_SUCCESS)
        error_code = MPIO_Err_return_file(fh, error_code);
    /* --END ERROR HANDLING-- */

#ifdef MPI_hpux
    HPMP_IO_END(fl_xmpi, fh, datatype, count);
#endif /* MPI_hpux */

    return error_code;
}
