/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
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
#elif defined(HAVE_WEAK_ATTRIBUTE)
int MPI_File_get_view(MPI_File fh, MPI_Offset * disp, MPI_Datatype * etype, MPI_Datatype * filetype,
                      char *datarep) __attribute__ ((weak, alias("PMPI_File_get_view")));
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
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
int MPI_File_get_view(MPI_File fh, MPI_Offset * disp, MPI_Datatype * etype,
                      MPI_Datatype * filetype, char *datarep)
{
    int error_code;
    ADIO_File adio_fh;
    static char myname[] = "MPI_FILE_GET_VIEW";
    int is_predef;
    MPI_Datatype copy_etype, copy_filetype;

    ROMIO_THREAD_CS_ENTER();

    adio_fh = MPIO_File_resolve(fh);

    /* --BEGIN ERROR HANDLING-- */
    MPIO_CHECK_FILE_HANDLE(adio_fh, myname, error_code);

    if (datarep == NULL) {
        error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
                                          myname, __LINE__, MPI_ERR_ARG, "**iodatarepnomem", 0);
        error_code = MPIO_Err_return_file(adio_fh, error_code);
        goto fn_exit;
    }
    /* --END ERROR HANDLING-- */

    *disp = adio_fh->disp;
    ADIOI_Strncpy(datarep,
                  (adio_fh->is_external32 ? "external32" : "native"), MPI_MAX_DATAREP_STRING);

    ADIOI_Type_ispredef(adio_fh->etype, &is_predef);
    if (is_predef)
        *etype = adio_fh->etype;
    else {
#ifdef MPIIMPL_HAVE_MPI_COMBINER_DUP
        MPI_Type_dup(adio_fh->etype, &copy_etype);
#else
        MPI_Type_contiguous(1, adio_fh->etype, &copy_etype);
#endif

        MPI_Type_commit(&copy_etype);
        *etype = copy_etype;
    }
    ADIOI_Type_ispredef(adio_fh->filetype, &is_predef);
    if (is_predef)
        *filetype = adio_fh->filetype;
    else {
#ifdef MPIIMPL_HAVE_MPI_COMBINER_DUP
        MPI_Type_dup(adio_fh->filetype, &copy_filetype);
#else
        MPI_Type_contiguous(1, adio_fh->filetype, &copy_filetype);
#endif

        MPI_Type_commit(&copy_filetype);
        *filetype = copy_filetype;
    }

  fn_exit:
    ROMIO_THREAD_CS_EXIT();

    return MPI_SUCCESS;
}
