/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *
 *  (C) 2008 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */
#include "adio.h"

int ADIOI_GEN_Feature(ADIO_File fd, int flag)
{
	switch(flag) {
		case ADIO_LOCKS:
		case ADIO_SHARED_FP:
		case ADIO_ATOMIC_MODE:
		case ADIO_DATA_SIEVING_WRITES:
		case ADIO_UNLINK_AFTER_CLOSE:
		case ADIO_TWO_PHASE:
		case ADIO_SCALABLE_RESIZE:
			return 1;
			break;
		case ADIO_SCALABLE_OPEN:
		default:
			return 0;
			break;
	}
}
