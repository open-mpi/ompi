/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_set_view = PMPI_File_set_view
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_set_view MPI_File_set_view
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_set_view as PMPI_File_set_view
/* end of weak pragmas */
#elif defined(HAVE_WEAK_ATTRIBUTE)
int MPI_File_set_view(MPI_File fh, MPI_Offset disp, MPI_Datatype etype, MPI_Datatype filetype,
                      const char *datarep, MPI_Info info) __attribute__((weak,alias("PMPI_File_set_view")));
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_set_view - Sets the file view

Input Parameters:
. fh - file handle (handle)
. disp - displacement (nonnegative integer)
. etype - elementary datatype (handle)
. filetype - filetype (handle)
. datarep - data representation (string)
. info - info object (handle)

.N fortran
@*/
int MPI_File_set_view(MPI_File fh, MPI_Offset disp, MPI_Datatype etype,
		      MPI_Datatype filetype, ROMIO_CONST char *datarep, MPI_Info info)
{
    int error_code;
    MPI_Count filetype_size, etype_size;
    static char myname[] = "MPI_FILE_SET_VIEW";
    ADIO_Offset shared_fp, byte_off;
    ADIO_File adio_fh;

    MPIU_THREAD_CS_ENTER(ALLFUNC,);

    adio_fh = MPIO_File_resolve(fh);

    /* --BEGIN ERROR HANDLING-- */
    MPIO_CHECK_FILE_HANDLE(adio_fh, myname, error_code);

    if ((disp < 0) && (disp != MPI_DISPLACEMENT_CURRENT))
    {
	error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					  myname, __LINE__, MPI_ERR_ARG, 
					  "**iobaddisp", 0);
	error_code = MPIO_Err_return_file(adio_fh, error_code);
	goto fn_exit;
    }

    /* rudimentary checks for incorrect etype/filetype.*/
    if (etype == MPI_DATATYPE_NULL) {
	error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					  myname, __LINE__, MPI_ERR_ARG,
					  "**ioetype", 0);
	error_code = MPIO_Err_return_file(adio_fh, error_code);
	goto fn_exit;
    }
    MPIO_DATATYPE_ISCOMMITTED(etype, error_code);
    if (error_code != MPI_SUCCESS) {
        error_code = MPIO_Err_return_file(adio_fh, error_code);
        goto fn_exit;
    }

    if (filetype == MPI_DATATYPE_NULL) {
	error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					  myname, __LINE__, MPI_ERR_ARG,
					  "**iofiletype", 0);
	error_code = MPIO_Err_return_file(adio_fh, error_code);
	goto fn_exit;
    }
    MPIO_DATATYPE_ISCOMMITTED(filetype, error_code);
    if (error_code != MPI_SUCCESS) {
        error_code = MPIO_Err_return_file(adio_fh, error_code);
        goto fn_exit;
    }

    if ((adio_fh->access_mode & MPI_MODE_SEQUENTIAL) &&
	(disp != MPI_DISPLACEMENT_CURRENT))
    {
	error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					  myname, __LINE__, MPI_ERR_ARG, 
					  "**iodispifseq", 0);
	error_code = MPIO_Err_return_file(adio_fh, error_code);
	goto fn_exit;
    }

    if ((disp == MPI_DISPLACEMENT_CURRENT) &&
	!(adio_fh->access_mode & MPI_MODE_SEQUENTIAL))
    {
	error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					  myname, __LINE__, MPI_ERR_ARG, 
					  "**iodispifseq", 0);
	error_code = MPIO_Err_return_file(adio_fh, error_code);
	goto fn_exit;
    }
    MPIO_CHECK_INFO_ALL(info, error_code, adio_fh->comm);
    /* --END ERROR HANDLING-- */

    MPI_Type_size_x(filetype, &filetype_size);
    MPI_Type_size_x(etype, &etype_size);

    /* --BEGIN ERROR HANDLING-- */
    if (etype_size != 0 && filetype_size % etype_size != 0)
    {
	error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					  myname, __LINE__, MPI_ERR_ARG,
					  "**iofiletype", 0);
	error_code = MPIO_Err_return_file(adio_fh, error_code);
	goto fn_exit;
    }

    if ((datarep == NULL) || (strcmp(datarep, "native") &&
	    strcmp(datarep, "NATIVE") &&
	    strcmp(datarep, "external32") &&
	    strcmp(datarep, "EXTERNAL32") &&
	    strcmp(datarep, "internal") &&
	    strcmp(datarep, "INTERNAL")) )
    {
	error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					  myname, __LINE__,
					  MPI_ERR_UNSUPPORTED_DATAREP, 
					  "**unsupporteddatarep",0);
	error_code = MPIO_Err_return_file(adio_fh, error_code);
	goto fn_exit;
    }
    /* --END ERROR HANDLING-- */

    if (disp == MPI_DISPLACEMENT_CURRENT) {
	MPI_Barrier(adio_fh->comm);
	ADIO_Get_shared_fp(adio_fh, 0, &shared_fp, &error_code);
	/* TODO: check error code */

	MPI_Barrier(adio_fh->comm);
	ADIOI_Get_byte_offset(adio_fh, shared_fp, &byte_off);
	/* TODO: check error code */

	disp = byte_off;
    }

    ADIO_Set_view(adio_fh, disp, etype, filetype, info, &error_code);

    /* --BEGIN ERROR HANDLING-- */
    if (error_code != MPI_SUCCESS) {
	error_code = MPIO_Err_return_file(adio_fh, error_code);
	goto fn_exit;
    }
    /* --END ERROR HANDLING-- */

    /* reset shared file pointer to zero */
    if (ADIO_Feature(adio_fh, ADIO_SHARED_FP) &&
        (adio_fh->shared_fp_fd != ADIO_FILE_NULL))
    {
	/* only one process needs to set it to zero, but I don't want to 
	   create the shared-file-pointer file if shared file pointers have 
	   not been used so far. Therefore, every process that has already 
	   opened the shared-file-pointer file sets the shared file pointer 
	   to zero. If the file was not opened, the value is automatically 
	   zero. Note that shared file pointer is stored as no. of etypes
	   relative to the current view, whereas indiv. file pointer is
	   stored in bytes. */

	ADIO_Set_shared_fp(adio_fh, 0, &error_code);
	/* --BEGIN ERROR HANDLING-- */
	if (error_code != MPI_SUCCESS)
	    error_code = MPIO_Err_return_file(adio_fh, error_code);
	/* --END ERROR HANDLING-- */
    }

    if (ADIO_Feature(adio_fh, ADIO_SHARED_FP))
    {
	MPI_Barrier(adio_fh->comm); /* for above to work correctly */
    }
    if (strcmp(datarep, "external32") && strcmp(datarep, "EXTERNAL32"))
	adio_fh->is_external32 = 0;
    else
	adio_fh->is_external32 = 1;

fn_exit:
    MPIU_THREAD_CS_EXIT(ALLFUNC,);

    return error_code;
fn_fail:
	/* --BEGIN ERROR HANDLING-- */
	error_code = MPIO_Err_return_file(fh, error_code);
	goto fn_exit;
	/* --END ERROR HANDLING-- */
}
