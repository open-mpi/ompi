/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: set_atom.c,v 1.8 2002/10/24 15:54:43 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_set_atomicity = PMPI_File_set_atomicity
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_set_atomicity MPI_File_set_atomicity
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_set_atomicity as PMPI_File_set_atomicity
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_set_atomicity - Sets the atomicity mode

Input Parameters:
. fh - file handle (handle)
. flag - true to set atomic mode, false to set nonatomic mode (logical)

.N fortran
@*/
int MPI_File_set_atomicity(MPI_File fh, int flag)
{
    int error_code, tmp_flag;
#ifndef PRINT_ERR_MSG
    static char myname[] = "MPI_FILE_SET_ATOMICITY";
#endif
    ADIO_Fcntl_t *fcntl_struct;

#ifdef PRINT_ERR_MSG
    if ((fh <= (MPI_File) 0) || (fh->cookie != ADIOI_FILE_COOKIE)) {
	FPRINTF(stderr, "MPI_File_set_atomicity: Invalid file handle\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
#else
    ADIOI_TEST_FILE_HANDLE(fh, myname);
#endif

    if (flag) flag = 1;  /* take care of non-one values! */

/* check if flag is the same on all processes */
    tmp_flag = flag;
    MPI_Bcast(&tmp_flag, 1, MPI_INT, 0, fh->comm);
    if (tmp_flag != flag) {
#ifdef PRINT_ERR_MSG
        FPRINTF(stderr, "MPI_File_set_atomicity: flag must be the same on all processes\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
#else
	error_code = MPIR_Err_setmsg(MPI_ERR_ARG, MPIR_ERR_FLAG_ARG,
				     myname, (char *) 0, (char *) 0);
	return ADIOI_Error(fh, error_code, myname);
#endif
    }

    if (fh->atomicity == flag) return MPI_SUCCESS;

    fcntl_struct = (ADIO_Fcntl_t *) ADIOI_Malloc(sizeof(ADIO_Fcntl_t));
    fcntl_struct->atomicity = flag;
    ADIO_Fcntl(fh, ADIO_FCNTL_SET_ATOMICITY, fcntl_struct, &error_code);
    ADIOI_Free(fcntl_struct);

    return error_code;
}
