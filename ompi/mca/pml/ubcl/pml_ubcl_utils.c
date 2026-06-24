/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2019-2024 Bull SAS.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file pml_ubcl_utils.c
 *
 * UBCL PML utilities
 *
 * Contains some usefull fonctions
 *
 */

#include "pml_ubcl_utils.h"
#include "pml_ubcl.h"
#include <stdint.h>

/* Reserve 1 cid bit to prevent MPI_ANY_TAG to match
 * messages with negative tag, which are ompi reserved tags
 */
#define CID_RESERVED_BIT (((uint64_t) 1) << 63)

ubcl_cid_t mca_pml_ubcl_compute_ubcl_cid(int tag, int cid)
{
    ubcl_cid_t ubcl_cid;
    ubcl_cid.cid.communicator = cid;

    int is_collective_tag = tag < 0 && MPI_ANY_TAG != tag;
    if (is_collective_tag) {
        ubcl_cid.cid.runtime = UBCL_CID_MPI_INTERNAL;
    } else {
        ubcl_cid.cid.runtime = UBCL_CID_MPI_APPLICATION;
    }

    return ubcl_cid;
}

