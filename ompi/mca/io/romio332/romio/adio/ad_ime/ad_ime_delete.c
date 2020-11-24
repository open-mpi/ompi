/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *
 *   Copyright (C) 1997 University of Chicago.
 *   Copyright (C) 2017 DataDirect Networks.
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_ime.h"
#include "adio.h"

#include "ad_ime_common.h"

void ADIOI_IME_Delete(const char *filename, int *error_code)
{
    int ret;
    static char myname[] = "ADIOI_IME_DELETE";

    char *ime_filename = ADIOI_IME_Add_prefix(filename);
    ret = ime_native_unlink(ime_filename);
    ADIOI_Free(ime_filename);
    if (ret)
        *error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                           MPIR_ERR_RECOVERABLE,
                                           myname, __LINE__,
                                           MPI_ERR_FILE, "Error in ime_native_unlink", 0);
    else
        *error_code = MPI_SUCCESS;

    return;
}
