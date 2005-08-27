/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: openf.c,v 1.15 2002/10/24 17:01:22 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#ifdef _UNICOS
#include <fortran.h>
#endif
#include "adio.h"
#include "mpio.h"


#if defined(MPIO_BUILD_PROFILING) || defined(HAVE_WEAK_SYMBOLS)

#if defined(HAVE_WEAK_SYMBOLS)
#if defined(HAVE_PRAGMA_WEAK)
#if defined(FORTRANCAPS)
#pragma weak MPI_FILE_OPEN = PMPI_FILE_OPEN
#elif defined(FORTRANDOUBLEUNDERSCORE)
#pragma weak mpi_file_open__ = pmpi_file_open__
#elif !defined(FORTRANUNDERSCORE)
#pragma weak mpi_file_open = pmpi_file_open
#else
#pragma weak mpi_file_open_ = pmpi_file_open_
#endif

#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#if defined(FORTRANCAPS)
#pragma _HP_SECONDARY_DEF PMPI_FILE_OPEN MPI_FILE_OPEN
#elif defined(FORTRANDOUBLEUNDERSCORE)
#pragma _HP_SECONDARY_DEF pmpi_file_open__ mpi_file_open__
#elif !defined(FORTRANUNDERSCORE)
#pragma _HP_SECONDARY_DEF pmpi_file_open mpi_file_open
#else
#pragma _HP_SECONDARY_DEF pmpi_file_open_ mpi_file_open_
#endif

#elif defined(HAVE_PRAGMA_CRI_DUP)
#if defined(FORTRANCAPS)
#pragma _CRI duplicate MPI_FILE_OPEN as PMPI_FILE_OPEN
#elif defined(FORTRANDOUBLEUNDERSCORE)
#pragma _CRI duplicate mpi_file_open__ as pmpi_file_open__
#elif !defined(FORTRANUNDERSCORE)
#pragma _CRI duplicate mpi_file_open as pmpi_file_open
#else
#pragma _CRI duplicate mpi_file_open_ as pmpi_file_open_
#endif

/* end of weak pragmas */
#endif
/* Include mapping from MPI->PMPI */
#include "mpioprof.h"
#endif

#ifdef FORTRANCAPS
#define mpi_file_open_ PMPI_FILE_OPEN
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define mpi_file_open_ pmpi_file_open__
#elif !defined(FORTRANUNDERSCORE)
#if defined(HPUX) || defined(SPPUX)
#pragma _HP_SECONDARY_DEF pmpi_file_open pmpi_file_open_
#endif
#define mpi_file_open_ pmpi_file_open
#else
#if defined(HPUX) || defined(SPPUX)
#pragma _HP_SECONDARY_DEF pmpi_file_open_ pmpi_file_open
#endif
#define mpi_file_open_ pmpi_file_open_
#endif

#else

#ifdef FORTRANCAPS
#define mpi_file_open_ MPI_FILE_OPEN
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define mpi_file_open_ mpi_file_open__
#elif !defined(FORTRANUNDERSCORE)
#if defined(HPUX) || defined(SPPUX)
#pragma _HP_SECONDARY_DEF mpi_file_open mpi_file_open_
#endif
#define mpi_file_open_ mpi_file_open
#else
#if defined(HPUX) || defined(SPPUX)
#pragma _HP_SECONDARY_DEF mpi_file_open_ mpi_file_open
#endif
#endif
#endif

#if defined(MPIHP) || defined(MPILAM)
/* Prototype to keep compiler happy */
void mpi_file_open_(MPI_Fint *comm,char *filename,int *amode,
		    MPI_Fint *info, MPI_Fint *fh, int *ierr, int str_len );

void mpi_file_open_(MPI_Fint *comm,char *filename,int *amode,
                  MPI_Fint *info, MPI_Fint *fh, int *ierr, int str_len )
{
    char *newfname;
    MPI_File fh_c;
    int real_len, i;
    MPI_Comm comm_c;
    MPI_Info info_c;

    comm_c = MPI_Comm_f2c(*comm);
    info_c = MPI_Info_f2c(*info);

    /* strip trailing blanks */
    if (filename <= (char *) 0) {
        FPRINTF(stderr, "MPI_File_open: filename is an invalid address\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
    }
    for (i=str_len-1; i>=0; i--) if (filename[i] != ' ') break;
    if (i < 0) {
	FPRINTF(stderr, "MPI_File_open: filename is a blank string\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
    }
    real_len = i + 1;

    newfname = (char *) ADIOI_Malloc((real_len+1)*sizeof(char));
    strncpy(newfname, filename, real_len);
    newfname[real_len] = '\0';

    *ierr = MPI_File_open(comm_c, newfname, *amode, info_c, &fh_c);

    *fh = MPI_File_c2f(fh_c);
    ADIOI_Free(newfname);
}

#else

#ifdef _UNICOS
void mpi_file_open_(MPI_Comm *comm,_fcd filename_fcd,int *amode,
                  MPI_Fint *info, MPI_Fint *fh, int *ierr)
{
    char *filename = _fcdtocp(filename_fcd);
    int str_len = _fcdlen(filename_fcd);
#else
/* Prototype to keep compiler happy */
/*
FORTRAN_API void FORT_CALL mpi_file_open_(MPI_Comm *comm,char *filename,int *amode,
		    MPI_Fint *info, MPI_Fint *fh, int *ierr, int str_len );

FORTRAN_API void FORT_CALL mpi_file_open_(MPI_Comm *comm,char *filename,int *amode,
                  MPI_Fint *info, MPI_Fint *fh, int *ierr, int str_len )
*/
/* Prototype to keep compiler happy */
FORTRAN_API void FORT_CALL mpi_file_open_(MPI_Comm *comm,char *filename FORT_MIXED_LEN_DECL,int *amode,
		    MPI_Fint *info, MPI_Fint *fh, int *ierr FORT_END_LEN_DECL);

FORTRAN_API void FORT_CALL mpi_file_open_(MPI_Comm *comm,char *filename FORT_MIXED_LEN(str_len),int *amode,
                  MPI_Fint *info, MPI_Fint *fh, int *ierr FORT_END_LEN(str_len))
{
#endif
    char *newfname;
    MPI_File fh_c;
    int real_len, i;
    MPI_Info info_c;

    info_c = MPI_Info_f2c(*info);

    /* strip trailing blanks */
    if (filename <= (char *) 0) {
        FPRINTF(stderr, "MPI_File_open: filename is an invalid address\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
    }
    for (i=str_len-1; i>=0; i--) if (filename[i] != ' ') break;
    if (i < 0) {
	FPRINTF(stderr, "MPI_File_open: filename is a blank string\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
    }
    real_len = i + 1;

    newfname = (char *) ADIOI_Malloc((real_len+1)*sizeof(char));
    strncpy(newfname, filename, real_len);
    newfname[real_len] = '\0';

    *ierr = MPI_File_open(*comm, newfname, *amode, info_c, &fh_c);

    *fh = MPI_File_c2f(fh_c);
    ADIOI_Free(newfname);
}
#endif
