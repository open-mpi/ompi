/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "adio.h"
#include "ad_nfs.h"

int ADIOI_NFS_Feature(ADIO_File fd, int flag)
{
    switch (flag) {
        case ADIO_SHARED_FP:
        case ADIO_LOCKS:
        case ADIO_SEQUENTIAL:
        case ADIO_DATA_SIEVING_WRITES:
        case ADIO_ATOMIC_MODE:
            return 1;
        case ADIO_SCALABLE_OPEN:
        case ADIO_UNLINK_AFTER_CLOSE:
        case ADIO_SCALABLE_RESIZE:
        case ADIO_IMMEDIATELY_VISIBLE:
        default:
            return 0;
    }
}
