/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "ad_daos.h"

void ADIOI_DAOS_Close(ADIO_File fd, int *error_code)
{
    int rank;
    struct ADIO_DAOS_cont *cont = (struct ADIO_DAOS_cont *) fd->fs_ptr;
    static char myname[] = "ADIOI_DAOS_CLOSE";
    int rc;

    MPI_Barrier(fd->comm);
    MPI_Comm_rank(fd->comm, &rank);

    /* release the dfs object handle for the file. */
    rc = dfs_release(cont->obj);
    if (rc != 0) {
        *error_code = ADIOI_DAOS_err(myname, cont->obj_name, __LINE__, rc);
        return;
    }

    /* decrement ref count on the container and pool in the hashtable. */
    adio_daos_coh_release(cont->c);
    cont->c = NULL;
    adio_daos_poh_release(cont->p);
    cont->p = NULL;

    if (rank == 0) {
        ADIOI_Free(cont->obj_name);
        ADIOI_Free(cont->cont_name);
    }
    ADIOI_Free(fd->fs_ptr);
    fd->fs_ptr = NULL;

    *error_code = MPI_SUCCESS;
}
