/*
 * Copyright (c) 2022      Triad National Security, LLC. All rights reserved.
 * Copyright (c) 2024      Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_GDS_SHMEM2_FETCH_H
#define PMIX_GDS_SHMEM2_FETCH_H

#include "gds_shmem2.h"

PMIX_EXPORT pmix_status_t
pmix_gds_shmem2_fetch(
    const pmix_proc_t *proc,
    pmix_scope_t scope,
    bool copy,
    const char *key,
    pmix_info_t qualifiers[],
    size_t nqual,
    pmix_list_t *kvs
);

#endif
