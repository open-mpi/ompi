/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: fsync.c,v 1.8 2002/10/24 15:54:39 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_sync = PMPI_File_sync
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_sync MPI_File_sync
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_sync as PMPI_File_sync
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_sync - Causes all previous writes to be transferred
                    to the storage device

Input Parameters:
. fh - file handle (handle)

.N fortran
@*/
int MPI_File_sync(MPI_File fh)
{
    int error_code;
#ifndef PRINT_ERR_MSG
    static char myname[] = "MPI_FILE_SYNC";
#endif
#ifdef MPI_hpux
    int fl_xmpi;

    HPMP_IO_START(fl_xmpi, BLKMPIFILESYNC, TRDTBLOCK, fh, MPI_DATATYPE_NULL, -1);
#endif /* MPI_hpux */

#ifdef PRINT_ERR_MSG
    if ((fh <= (MPI_File) 0) || (fh->cookie != ADIOI_FILE_COOKIE)) {
	FPRINTF(stderr, "MPI_File_sync: Invalid file handle\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
#else
    ADIOI_TEST_FILE_HANDLE(fh, myname);
#endif

    ADIO_Flush(fh, &error_code);
#ifdef MPI_hpux
    HPMP_IO_END(fl_xmpi, fh, MPI_DATATYPE_NULL, -1);
#endif /* MPI_hpux */
    return error_code;
}
