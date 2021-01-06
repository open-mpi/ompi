/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "ad_pvfs2.h"

void ADIOI_PVFS2_Close(ADIO_File fd, int *error_code)
{
    ADIOI_Free(fd->fs_ptr);
    fd->fs_ptr = NULL;
    /* PVFS2 doesn't have a 'close', but MPI-IO semantics dictate that we
     * ensure all data has been flushed.
     */

    /* At some point or another it was decided that ROMIO would not
     * explicitly flush (other than any local cache) on close, because
     * there is no way to *avoid* that overhead if you implement it here
     * and don't actually want it.
     */

    *error_code = MPI_SUCCESS;
}
