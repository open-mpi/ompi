/*
 * Copyright (c) 2018-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "coll_han.h"

/* Get root's low_rank and up_rank from vranks array */
void mca_coll_han_get_ranks(int *vranks, int root, int low_size, int *root_low_rank,
                            int *root_up_rank)
{
    *root_up_rank = vranks[root] / low_size;
    *root_low_rank = vranks[root] % low_size;
}

uint32_t han_auto_tuned_get_n(uint32_t n)
{
    uint32_t avail[5] = { 4, 8, 16, 32, 64 };
    uint32_t i;
    for (i = 0; i < 5; i++) {
        if (avail[i] >= n) {
            return i;
        }
    }
    return i - 1;
}

uint32_t han_auto_tuned_get_c(uint32_t c)
{
    uint32_t avail[3] = { 4, 8, 12 };
    uint32_t i;
    for (i = 0; i < 3; i++) {
        if (avail[i] >= c) {
            return i;
        }
    }
    return i - 1;
}

uint32_t han_auto_tuned_get_m(uint32_t m)
{
    uint32_t avail[21] =
        { 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072,
262144, 524288, 1048576, 2097152, 4194304 };
    uint32_t i;
    for (i = 0; i < 21; i++) {
        if (avail[i] >= m) {
            return i;
        }
    }
    return i - 1;
}
