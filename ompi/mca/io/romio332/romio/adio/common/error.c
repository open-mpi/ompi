/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *
 *   Copyright (C) 1997 University of Chicago.
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"
#include "adio_extern.h"

/* Check for special error codes for those MPI error
   classes that relate to particular problems.
   Returns an MPI error code corresponding to "my_errno", for function "myname"
 * and the given file, "filename".  */
int ADIOI_Err_create_code(const char *myname, const char *filename, int my_errno)
{
    int error_code = MPI_SUCCESS;
    if (!my_errno)
        return MPI_SUCCESS;

    /* --BEGIN ERROR HANDLING-- */
    switch (my_errno) {
        case EACCES:
            error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                              MPIR_ERR_RECOVERABLE, myname,
                                              __LINE__, MPI_ERR_ACCESS,
                                              "**fileaccess", "**fileaccess %s", filename);
            break;
        case ENAMETOOLONG:
            error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                              MPIR_ERR_RECOVERABLE, myname,
                                              __LINE__, MPI_ERR_BAD_FILE,
                                              "**filenamelong",
                                              "**filenamelong %s %d", filename, strlen(filename));
            break;
        case ENOENT:
            error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                              MPIR_ERR_RECOVERABLE, myname,
                                              __LINE__, MPI_ERR_NO_SUCH_FILE,
                                              "**filenoexist", "**filenoexist %s", filename);
            break;
        case EISDIR:
            error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                              MPIR_ERR_RECOVERABLE,
                                              myname, __LINE__,
                                              MPI_ERR_BAD_FILE,
                                              "**filenamedir", "**filenamedir %s", filename);
            break;
        case EROFS:
            /* Read only file or file system and write access requested */
            error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                              MPIR_ERR_RECOVERABLE, myname,
                                              __LINE__, MPI_ERR_READ_ONLY, "**ioneedrd", 0);
            break;
        case EEXIST:
            error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                              MPIR_ERR_RECOVERABLE, myname,
                                              __LINE__, MPI_ERR_FILE_EXISTS, "**fileexist", 0);
            break;
        case ENOTDIR:
        case ELOOP:
            error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                              MPIR_ERR_RECOVERABLE,
                                              myname, __LINE__,
                                              MPI_ERR_BAD_FILE,
                                              "**filenamedir", "**filenamedir %s", filename);
            break;
        case ENOSPC:
            error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                              MPIR_ERR_RECOVERABLE, myname, __LINE__,
                                              MPI_ERR_NO_SPACE, "**filenospace", 0);
            break;
        case EDQUOT:
            error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                              MPIR_ERR_RECOVERABLE, myname, __LINE__,
                                              MPI_ERR_QUOTA, "**filequota", 0);
            break;
        default:
            error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
                                              myname, __LINE__, MPI_ERR_IO, "**io",
                                              "**io %s", strerror(my_errno));
            break;
    }
    /* --END ERROR HANDLING-- */

    return error_code;
}
