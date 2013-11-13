/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_get_view = PMPI_File_get_view
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_get_view MPI_File_get_view
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_get_view as PMPI_File_get_view
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif
#ifdef MPISGI
#include "mpisgi2.h"
#endif

/*@
    MPI_File_get_view - Returns the file view

Input Parameters:
. fh - file handle (handle)

Output Parameters:
. disp - displacement (nonnegative integer)
. etype - elementary datatype (handle)
. filetype - filetype (handle)
. datarep - data representation (string)

.N fortran
@*/
int MPI_File_get_view(MPI_File fh, MPI_Offset *disp, MPI_Datatype *etype,
                      MPI_Datatype *filetype, char *datarep)
{
    int error_code;
    ADIO_File adio_fh;
    static char myname[] = "MPI_FILE_GET_VIEW";
    int i, j, k, combiner;
    MPI_Datatype copy_etype, copy_filetype;

    MPIU_THREAD_CS_ENTER(ALLFUNC,);

    adio_fh = MPIO_File_resolve(fh);

    /* --BEGIN ERROR HANDLING-- */
    MPIO_CHECK_FILE_HANDLE(adio_fh, myname, error_code);

    if (datarep <= (char *) 0)
    {
	error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					  myname, __LINE__, MPI_ERR_ARG, 
					  "**iodatarepnomem", 0);
	error_code = MPIO_Err_return_file(adio_fh, error_code);
	goto fn_exit;
    }
    /* --END ERROR HANDLING-- */

    *disp = adio_fh->disp;
    ADIOI_Strncpy(datarep,
	    (adio_fh->is_external32 ? "external32": "native"), MPI_MAX_DATAREP_STRING);

    MPI_Type_get_envelope(adio_fh->etype, &i, &j, &k, &combiner);
    if (combiner == MPI_COMBINER_NAMED) *etype = adio_fh->etype;
    else {
	/* FIXME: It is wrong to use MPI_Type_contiguous; the user could choose to
	   re-implement MPI_Type_contiguous in an unexpected way.  Either use 
	   MPIR_Barrier_impl as in MPICH or PMPI_Type_contiguous */
        MPI_Type_contiguous(1, adio_fh->etype, &copy_etype);

	/* FIXME: Ditto for MPI_Type_commit - use NMPI or PMPI */
        MPI_Type_commit(&copy_etype);
        *etype = copy_etype;
    }
    /* FIXME: Ditto for MPI_Type_xxx - use NMPI or PMPI */
    MPI_Type_get_envelope(adio_fh->filetype, &i, &j, &k, &combiner);
    if (combiner == MPI_COMBINER_NAMED) *filetype = adio_fh->filetype;
    else {
        MPI_Type_contiguous(1, adio_fh->filetype, &copy_filetype);

        MPI_Type_commit(&copy_filetype);
        *filetype = copy_filetype;
    }

fn_exit:
    MPIU_THREAD_CS_EXIT(ALLFUNC,);

    return MPI_SUCCESS;
}
