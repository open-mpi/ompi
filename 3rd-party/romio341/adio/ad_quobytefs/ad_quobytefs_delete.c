/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */


#include "adio.h"

#include "ad_quobytefs.h"
#include "ad_quobytefs_internal.h"

void ADIOI_QUOBYTEFS_Delete(const char *path, int *error_code)
{
    ADIOI_QUOBYTEFS_CreateAdapter(path, error_code);
    static char myname[] = "ADIOI_QUOBYTEFS_DELETE";

    if (quobyte_unlink(ADIOI_QUOBYTEFSI_GetVolumeAndPath(path))) {
        *error_code = ADIOI_Err_create_code(myname, path, errno);
        return;
    }
    *error_code = MPI_SUCCESS;
}
