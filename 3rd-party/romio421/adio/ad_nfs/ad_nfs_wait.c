/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "ad_nfs.h"

void ADIOI_NFS_ReadComplete(ADIO_Request * request, ADIO_Status * status, int *error_code)
{
    return;
}


void ADIOI_NFS_WriteComplete(ADIO_Request * request, ADIO_Status * status, int *error_code)
{
    ADIOI_NFS_ReadComplete(request, status, error_code);
}
