/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_iread_at = PMPI_File_iread_at
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_iread_at MPI_File_iread_at
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_iread_at as PMPI_File_iread_at
/* end of weak pragmas */
#elif defined(HAVE_WEAK_ATTRIBUTE)
int MPI_File_iread_at(MPI_File fh, MPI_Offset offset, void *buf, int count, MPI_Datatype datatype,
                      MPIO_Request *request) __attribute__((weak,alias("PMPI_File_iread_at")));
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

#ifdef HAVE_MPI_GREQUEST
#include "mpiu_greq.h"
#endif

/*@
    MPI_File_iread_at - Nonblocking read using explicit offset

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
int MPI_File_iread_at(MPI_File fh, MPI_Offset offset, void *buf, int count, MPI_Datatype datatype, 
                      MPIO_Request *request)
{
    int error_code;
    static char myname[] = "MPI_FILE_IREAD_AT";

#ifdef MPI_hpux
    int fl_xmpi;

    HPMP_IO_START(fl_xmpi, BLKMPIFILEIREADAT, TRDTSYSTEM, fh, datatype,
		  count);
#endif /* MPI_hpux */


    error_code = MPIOI_File_iread(fh, offset, ADIO_EXPLICIT_OFFSET, buf,
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
