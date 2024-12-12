/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "mpioimpl.h"
#include <limits.h>
#include <assert.h>

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_write_at_all_begin = PMPI_File_write_at_all_begin
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_write_at_all_begin MPI_File_write_at_all_begin
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_write_at_all_begin as PMPI_File_write_at_all_begin
/* end of weak pragmas */
#elif defined(HAVE_WEAK_ATTRIBUTE)
int MPI_File_write_at_all_begin(MPI_File fh, MPI_Offset offset, const void *buf, int count,
                                MPI_Datatype datatype)
    __attribute__ ((weak, alias("PMPI_File_write_at_all_begin")));
#endif

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_write_at_all_begin_c = PMPI_File_write_at_all_begin_c
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_write_at_all_begin_c MPI_File_write_at_all_begin_c
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_write_at_all_begin_c as PMPI_File_write_at_all_begin_c
/* end of weak pragmas */
#elif defined(HAVE_WEAK_ATTRIBUTE)
int MPI_File_write_at_all_begin_c(MPI_File fh, MPI_Offset offset, const void *buf, MPI_Count count,
                                  MPI_Datatype datatype)
    __attribute__ ((weak, alias("PMPI_File_write_at_all_begin_c")));
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_write_at_all_begin - Begin a split collective write using
    explicit offset

Input Parameters:
. fh - file handle (handle)
. offset - file offset (nonnegative integer)
. buf - initial address of buffer (choice)
. count - number of elements in buffer (nonnegative integer)
. datatype - datatype of each buffer element (handle)

.N fortran
@*/
int MPI_File_write_at_all_begin(MPI_File fh, MPI_Offset offset, ROMIO_CONST void *buf,
                                int count, MPI_Datatype datatype)
{
    int error_code;
    static char myname[] = "MPI_FILE_WRITE_AT_ALL_BEGIN";

    error_code = MPIOI_File_write_all_begin(fh, offset,
                                            ADIO_EXPLICIT_OFFSET, buf, count, datatype, myname);

    return error_code;
}

/* large count function */


/*@
    MPI_File_write_at_all_begin_c - Begin a split collective write using
    explicit offset

Input Parameters:
. fh - file handle (handle)
. offset - file offset (nonnegative integer)
. buf - initial address of buffer (choice)
. count - number of elements in buffer (nonnegative integer)
. datatype - datatype of each buffer element (handle)

.N fortran
@*/
int MPI_File_write_at_all_begin_c(MPI_File fh, MPI_Offset offset, ROMIO_CONST void *buf,
                                  MPI_Count count, MPI_Datatype datatype)
{
    int error_code;
    static char myname[] = "MPI_FILE_WRITE_AT_ALL_BEGIN";

    error_code = MPIOI_File_write_all_begin(fh, offset,
                                            ADIO_EXPLICIT_OFFSET, buf, count, datatype, myname);

    return error_code;
}
