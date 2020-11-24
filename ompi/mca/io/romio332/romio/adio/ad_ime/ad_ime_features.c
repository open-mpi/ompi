/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *
 *   Copyright (C) 1997 University of Chicago.
 *   Copyright (C) 2017 DataDirect Networks.
 *   See COPYRIGHT notice in top-level directory.
 */
#include "adio.h"
#include "ad_ime.h"

int ADIOI_IME_Feature(ADIO_File fd, int flag)
{
    switch (flag) {
        case ADIO_SCALABLE_OPEN:
        case ADIO_SHARED_FP:
        case ADIO_LOCKS:
        case ADIO_SEQUENTIAL:
        case ADIO_DATA_SIEVING_WRITES:
        default:
            return 0;
    }
}
