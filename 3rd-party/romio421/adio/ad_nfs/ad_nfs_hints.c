/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "ad_nfs.h"

void ADIOI_NFS_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code)
{
    ADIOI_GEN_SetInfo(fd, users_info, error_code);
    ADIOI_Info_set(fd->info, "romio_visibility_immediate", "false");
    fd->hints->visibility_immediate = 0;
}
