/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *
 *  (C) 2008 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */
#include "adio.h"
#include "ad_nfs.h"

int ADIOI_NFS_Feature(ADIO_File fd, int flag)
{       
        switch(flag) {
                case ADIO_SHARED_FP:
                case ADIO_LOCKS:
                case ADIO_SEQUENTIAL:
                case ADIO_DATA_SIEVING_WRITES:
			return 1;
                case ADIO_SCALABLE_OPEN:
		case ADIO_UNLINK_AFTER_CLOSE:
		case ADIO_SCALABLE_RESIZE:
                default:
                        return 0;
        }
}
