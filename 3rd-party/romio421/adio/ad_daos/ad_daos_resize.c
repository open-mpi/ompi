/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "ad_daos.h"

void ADIOI_DAOS_Resize(ADIO_File fd, ADIO_Offset size, int *error_code)
{
    int ret, rank;
    struct ADIO_DAOS_cont *cont = fd->fs_ptr;
    static char myname[] = "ADIOI_DAOS_RESIZE";

    *error_code = MPI_SUCCESS;
    MPI_Comm_rank(fd->comm, &rank);
    MPI_Barrier(fd->comm);

    if (rank == fd->hints->ranklist[0])
        ret = dfs_punch(cont->dfs, cont->obj, size, DFS_MAX_FSIZE);

    MPI_Bcast(&ret, 1, MPI_INT, fd->hints->ranklist[0], fd->comm);
    if (ret != 0)
        *error_code = ADIOI_DAOS_err(myname, cont->obj_name, __LINE__, ret);
}
