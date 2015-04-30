/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
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
#elif defined(HAVE_WEAK_ATTRIBUTE)
int MPI_File_set_atomicity(MPI_File fh, int flag) __attribute__((weak,alias("PMPI_File_set_atomicity")));
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
    static char myname[] = "MPI_FILE_SET_ATOMICITY";
    ADIO_Fcntl_t *fcntl_struct;
    ADIO_File adio_fh;

    MPIU_THREAD_CS_ENTER(ALLFUNC,);

    adio_fh = MPIO_File_resolve(fh);

    /* --BEGIN ERROR HANDLING-- */
    MPIO_CHECK_FILE_HANDLE(adio_fh, myname, error_code);
    /* --END ERROR HANDLING-- */

    ADIOI_TEST_DEFERRED(adio_fh, myname, &error_code);

    if (flag) flag = 1;  /* take care of non-one values! */

/* check if flag is the same on all processes */
    tmp_flag = flag;
    MPI_Bcast(&tmp_flag, 1, MPI_INT, 0, adio_fh->comm);

    /* --BEGIN ERROR HANDLING-- */
    if (tmp_flag != flag) {
	error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					  myname, __LINE__, MPI_ERR_ARG, 
					  "**notsame", 0);
	error_code = MPIO_Err_return_file(adio_fh, error_code);
	goto fn_exit;
    }
    /* --END ERROR HANDLING-- */

    if (adio_fh->atomicity == flag){
	    error_code = MPI_SUCCESS;
	    goto fn_exit;
    }


    fcntl_struct = (ADIO_Fcntl_t *) ADIOI_Malloc(sizeof(ADIO_Fcntl_t));
    fcntl_struct->atomicity = flag;
    ADIO_Fcntl(adio_fh, ADIO_FCNTL_SET_ATOMICITY, fcntl_struct, &error_code);
    /* TODO: what do we do with this error code? */

    /* --BEGIN ERROR HANDLING-- */
    if (error_code != MPI_SUCCESS)
	error_code = MPIO_Err_return_file(adio_fh, error_code);
    /* --END ERROR HANDLING-- */

    ADIOI_Free(fcntl_struct);

fn_exit:
    MPIU_THREAD_CS_EXIT(ALLFUNC,);
    return error_code;
}
