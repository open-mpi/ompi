/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: get_errhf.c,v 1.8 2002/10/24 17:01:19 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"
#include "mpio.h"


#if defined(MPIO_BUILD_PROFILING) || defined(HAVE_WEAK_SYMBOLS)

#if defined(HAVE_WEAK_SYMBOLS)
#if defined(HAVE_PRAGMA_WEAK)
#if defined(FORTRANCAPS)
#pragma weak MPI_FILE_GET_ERRHANDLER = PMPI_FILE_GET_ERRHANDLER
#elif defined(FORTRANDOUBLEUNDERSCORE)
#pragma weak mpi_file_get_errhandler__ = pmpi_file_get_errhandler__
#elif !defined(FORTRANUNDERSCORE)
#pragma weak mpi_file_get_errhandler = pmpi_file_get_errhandler
#else
#pragma weak mpi_file_get_errhandler_ = pmpi_file_get_errhandler_
#endif

#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#if defined(FORTRANCAPS)
#pragma _HP_SECONDARY_DEF PMPI_FILE_GET_ERRHANDLER MPI_FILE_GET_ERRHANDLER
#elif defined(FORTRANDOUBLEUNDERSCORE)
#pragma _HP_SECONDARY_DEF pmpi_file_get_errhandler__ mpi_file_get_errhandler__
#elif !defined(FORTRANUNDERSCORE)
#pragma _HP_SECONDARY_DEF pmpi_file_get_errhandler mpi_file_get_errhandler
#else
#pragma _HP_SECONDARY_DEF pmpi_file_get_errhandler_ mpi_file_get_errhandler_
#endif

#elif defined(HAVE_PRAGMA_CRI_DUP)
#if defined(FORTRANCAPS)
#pragma _CRI duplicate MPI_FILE_GET_ERRHANDLER as PMPI_FILE_GET_ERRHANDLER
#elif defined(FORTRANDOUBLEUNDERSCORE)
#pragma _CRI duplicate mpi_file_get_errhandler__ as pmpi_file_get_errhandler__
#elif !defined(FORTRANUNDERSCORE)
#pragma _CRI duplicate mpi_file_get_errhandler as pmpi_file_get_errhandler
#else
#pragma _CRI duplicate mpi_file_get_errhandler_ as pmpi_file_get_errhandler_
#endif

/* end of weak pragmas */
#endif
/* Include mapping from MPI->PMPI */
#include "mpioprof.h"
#endif

#ifdef FORTRANCAPS
#define mpi_file_get_errhandler_ PMPI_FILE_GET_ERRHANDLER
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define mpi_file_get_errhandler_ pmpi_file_get_errhandler__
#elif !defined(FORTRANUNDERSCORE)
#if defined(HPUX) || defined(SPPUX)
#pragma _HP_SECONDARY_DEF pmpi_file_get_errhandler pmpi_file_get_errhandler_
#endif
#define mpi_file_get_errhandler_ pmpi_file_get_errhandler
#else
#if defined(HPUX) || defined(SPPUX)
#pragma _HP_SECONDARY_DEF pmpi_file_get_errhandler_ pmpi_file_get_errhandler
#endif
#define mpi_file_get_errhandler_ pmpi_file_get_errhandler_
#endif

#else

#ifdef FORTRANCAPS
#define mpi_file_get_errhandler_ MPI_FILE_GET_ERRHANDLER
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define mpi_file_get_errhandler_ mpi_file_get_errhandler__
#elif !defined(FORTRANUNDERSCORE)
#if defined(HPUX) || defined(SPPUX)
#pragma _HP_SECONDARY_DEF mpi_file_get_errhandler mpi_file_get_errhandler_
#endif
#define mpi_file_get_errhandler_ mpi_file_get_errhandler
#else
#if defined(HPUX) || defined(SPPUX)
#pragma _HP_SECONDARY_DEF mpi_file_get_errhandler_ mpi_file_get_errhandler
#endif
#endif
#endif

/* Prototype to keep compiler happy */
FORTRAN_API void FORT_CALL mpi_file_get_errhandler_(MPI_Fint *fh, MPI_Fint *err_handler, int *ierr);

FORTRAN_API void FORT_CALL mpi_file_get_errhandler_(MPI_Fint *fh, MPI_Fint *err_handler, int *ierr)
{
    MPI_File fh_c;
    MPI_Errhandler err_handler_c;
    
    fh_c = MPI_File_f2c(*fh);
    *ierr = MPI_File_get_errhandler(fh_c, &err_handler_c);
    *err_handler = MPI_Errhandler_c2f(err_handler_c);
}
