/*
 * Copyright (c) 2022      Triad National Security, LLC. All rights reserved.
 * Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_UTIL_VMEM_H
#define PMIX_UTIL_VMEM_H

#include "src/include/pmix_config.h"
#include "include/pmix_common.h"

typedef enum {
    VMEM_HOLE_NONE = -1,
    /** Use hole at the very beginning. */
    VMEM_HOLE_BEGIN = 0,
    /** Use hole right after the heap. */
    VMEM_HOLE_AFTER_HEAP = 1,
    /* Use hole right before stack. */
    VMEM_HOLE_BEFORE_STACK = 2,
    /* Use the biggest hole. */
    VMEM_HOLE_BIGGEST = 3,
    /* Use the biggest hole between heap and stack. */
    VMEM_HOLE_IN_LIBS = 4,
    /* Use given address, if available. */
    VMEM_HOLE_CUSTOM = 5
} pmix_vmem_hole_kind_t;

PMIX_EXPORT pmix_status_t
pmix_vmem_find_hole(
    pmix_vmem_hole_kind_t hkind,
    size_t *addrp,
    size_t size
);

#endif
