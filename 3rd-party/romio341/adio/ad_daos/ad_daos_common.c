/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "ad_daos.h"
#include <daos_errno.h>

int ADIOI_DAOS_Initialized = MPI_KEYVAL_INVALID;

static int ad_daos_end(MPI_Comm comm, int keyval, void *attribute_val, void *extra_state)
{
    int error_code = MPI_SUCCESS;
    int rc;

    adio_daos_hash_finalize();
    rc = daos_fini();

    if (rc != 0) {
        error_code = ADIOI_DAOS_err("ad_daos_end", "DAOS Finalize Error", __LINE__, rc);
        return error_code;
    }

    MPI_Keyval_free(&keyval);
    return error_code;
}

void ADIOI_DAOS_Init(int *error_code)
{
    static char myname[] = "ADIOI_DAOS_INIT";
    int rc;

    *error_code = MPI_SUCCESS;

    /** nothing to do if already initialized */
    if (ADIOI_DAOS_Initialized != MPI_KEYVAL_INVALID)
        return;

    rc = daos_init();
    if (rc) {
        *error_code = ADIOI_DAOS_err(myname, "DAOS Init Error", __LINE__, rc);
        fprintf(stderr, "daos_init() failed with %d\n", rc);
        return;
    }

    rc = adio_daos_hash_init();
    if (rc < 0) {
        *error_code = ADIOI_DAOS_err(myname, "DAOS Init Error", __LINE__, rc);
        fprintf(stderr, "Failed to init daos handle hash table\n");
        return;
    }

    /** attach to comm_self destroy to finalize DAOS */
    MPI_Keyval_create(MPI_NULL_COPY_FN, ad_daos_end, &ADIOI_DAOS_Initialized, (void *) 0);
    MPI_Attr_put(MPI_COMM_SELF, ADIOI_DAOS_Initialized, (void *) 0);
}

int ADIOI_DAOS_err(const char *myname, const char *filename, int line, int rc)
{
    int error_code = MPI_SUCCESS;

    if (rc == 0)
        return MPI_SUCCESS;

    switch (rc) {
        case -DER_NO_PERM:
        case EPERM:
            error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                              MPIR_ERR_RECOVERABLE, myname,
                                              line, MPI_ERR_ACCESS,
                                              "**fileaccess", "**fileaccess %s", filename);
            break;
        case -DER_ENOENT:
        case -DER_NONEXIST:
        case -DER_NO_HDL:
        case ENOENT:
            error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                              MPIR_ERR_RECOVERABLE, myname,
                                              line, MPI_ERR_NO_SUCH_FILE,
                                              "**filenoexist", "**filenoexist %s", filename);
            break;
        case -DER_IO:
        case EIO:
            error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
                                              myname, line, MPI_ERR_IO, "**io",
                                              "**io %s", filename);
            break;
        case -DER_EXIST:
        case EEXIST:
            error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                              MPIR_ERR_RECOVERABLE, myname,
                                              line, MPI_ERR_FILE_EXISTS, "**fileexist", 0);
            break;
        case -DER_NOTDIR:
        case ENOTDIR:
            error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                              MPIR_ERR_RECOVERABLE,
                                              myname, line,
                                              MPI_ERR_BAD_FILE,
                                              "**filenamedir", "**filenamedir %s", filename);
            break;
        case -DER_NOSPACE:
        case ENOSPC:
            error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                              MPIR_ERR_RECOVERABLE, myname, line,
                                              MPI_ERR_NO_SPACE, "**filenospace", 0);
            break;
        case -DER_INVAL:
        case EINVAL:
            error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                              MPIR_ERR_RECOVERABLE, myname, line,
                                              MPI_ERR_ARG, "**arg", 0);
            break;
        case -DER_NOSYS:
        case ENOSYS:
            error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
                                              myname, line,
                                              MPI_ERR_UNSUPPORTED_OPERATION,
                                              "**fileopunsupported", 0);
            break;
        case -DER_NOMEM:
        case ENOMEM:
            error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
                                              myname, line, MPI_ERR_NO_MEM, "**allocmem", 0);
            break;
        default:
            error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
                                              myname, line, MPI_ERR_IO, "**io",
                                              "**io %s", filename);
            break;
    }

    return error_code;
}
