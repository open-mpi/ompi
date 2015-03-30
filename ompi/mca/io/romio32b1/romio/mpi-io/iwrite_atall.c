/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2014 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPIX_File_iwrite_at_all = PMPIX_File_iwrite_at_all
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPIX_File_iwrite_at_all MPIX_File_iwrite_at_all
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPIX_File_iwrite_at_all as PMPIX_File_iwrite_at_all
/* end of weak pragmas */
#elif defined(HAVE_WEAK_ATTRIBUTE)
int MPIX_File_iwrite_at_all(MPI_File fh, MPI_Offset offset, const void *buf, int count,
                           MPI_Datatype datatype, MPI_Request *request)
    __attribute__((weak,alias("PMPIX_File_iwrite_at_all")));
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

#ifdef HAVE_MPI_GREQUEST
#include "mpiu_greq.h"
#endif

/*@
    MPIX_File_iwrite_at_all - Nonblocking collective write using explicit offset

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
int MPIX_File_iwrite_at_all(MPI_File fh, MPI_Offset offset, ROMIO_CONST void *buf,
                           int count, MPI_Datatype datatype,
                           MPI_Request *request)
{
    int error_code;
    static char myname[] = "MPIX_FILE_IWRITE_AT_ALL";
#ifdef MPI_hpux
    int fl_xmpi;

    HPMP_IO_START(fl_xmpi, BLKMPIFILEIWRITEATALL, TRDTBLOCK, fh, datatype, count);
#endif /* MPI_hpux */

    error_code = MPIOI_File_iwrite_all(fh, offset, ADIO_EXPLICIT_OFFSET,
				      buf, count, datatype, myname, request);

#ifdef MPI_hpux
    HPMP_IO_END(fl_xmpi, fh, datatype, count);
#endif /* MPI_hpux */
    return error_code;
}

