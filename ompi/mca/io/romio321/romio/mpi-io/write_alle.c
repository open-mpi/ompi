/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_write_all_end = PMPI_File_write_all_end
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_write_all_end MPI_File_write_all_end
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_write_all_end as PMPI_File_write_all_end
/* end of weak pragmas */
#elif defined(HAVE_WEAK_ATTRIBUTE)
int MPI_File_write_all_end(MPI_File fh, const void *buf, MPI_Status *status) __attribute__((weak,alias("PMPI_File_write_all_end")));
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_write_all_end - Complete a split collective write using individual file pointer

Input Parameters:
. fh - file handle (handle)

Output Parameters:
. buf - initial address of buffer (choice)
. status - status object (Status)

.N fortran
@*/
int MPI_File_write_all_end(MPI_File fh, ROMIO_CONST void *buf, MPI_Status *status)
{
    int error_code;
    static char myname[] = "MPI_FILE_WRITE_ALL_END";

    error_code = MPIOI_File_write_all_end(fh, buf, myname, status);

    return error_code;
}

/* prevent multiple definitions of this routine */
#ifdef MPIO_BUILD_PROFILING
int MPIOI_File_write_all_end(MPI_File fh,
			     const void *buf,
			     char *myname,
			     MPI_Status *status)
{
    int error_code;
    ADIO_File adio_fh;

    MPIU_UNREFERENCED_ARG(buf);

    ROMIO_THREAD_CS_ENTER();

    adio_fh = MPIO_File_resolve(fh);

    /* --BEGIN ERROR HANDLING-- */
    MPIO_CHECK_FILE_HANDLE(adio_fh, myname, error_code);

    if (!(adio_fh->split_coll_count))
    {
	error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					  myname, __LINE__, MPI_ERR_IO, 
					  "**iosplitcollnone", 0);
	error_code = MPIO_Err_return_file(adio_fh, error_code);
	goto fn_exit;
    }
    /* --END ERROR HANDLING-- */

#ifdef HAVE_STATUS_SET_BYTES
    /* FIXME - we should really ensure that the split_datatype remains
       valid by incrementing the ref count in the write_allb.c routine
       and decrement it here after setting the bytes */
    if (status != MPI_STATUS_IGNORE)
       *status = adio_fh->split_status;
#endif
    adio_fh->split_coll_count = 0;

    error_code = MPI_SUCCESS;

fn_exit:
    ROMIO_THREAD_CS_EXIT();

    return error_code;
}
#endif
