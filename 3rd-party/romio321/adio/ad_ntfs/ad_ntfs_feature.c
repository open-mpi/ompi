/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *
 *  (C) 2008 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */
#include "adio.h"

int ADIOI_NTFS_Feature(ADIO_File fd, int flag)
{
	switch(flag) {
	    /* supported features */
		case ADIO_LOCKS:
		case ADIO_SHARED_FP:
		case ADIO_ATOMIC_MODE:
		case ADIO_DATA_SIEVING_WRITES:
			return 1;
			break;
	    /* unsupported features */
		case ADIO_SCALABLE_OPEN:
		case ADIO_UNLINK_AFTER_CLOSE:
		default:
			return 0;
			break;
	}
}
