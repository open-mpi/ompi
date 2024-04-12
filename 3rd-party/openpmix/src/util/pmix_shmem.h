/*
 * Copyright (c) 2021-2023 Triad National Security, LLC. All rights reserved.
 * Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_UTIL_SHMEM_H
#define PMIX_UTIL_SHMEM_H

#include "src/include/pmix_config.h"
#include "include/pmix_common.h"
#include "src/class/pmix_object.h"

#include <sys/stat.h>

/**
 * Bitmap container for pmix_shmem flags.
 */
typedef uint8_t pmix_shmem_flags_t;

typedef enum {
    /**
     * Indicates that during attach the requested address
     * must match the address returned by memory mapping.
     */
    PMIX_SHMEM_MUST_MAP_AT_RADDR = 0x01
} pmix_shmem_flag_t;

typedef struct pmix_shmem_t {
    /** Parent class. */
    pmix_object_t super;
    /** Flag indicating if attached to segment. */
    volatile bool attached;
    /** Size of shared-memory segment. */
    size_t size;
    /** Address of shared memory segment header. */
    void *hdr_address;
    /** Base data address of shared memory segment. */
    void *data_address;
    /** Buffer holding path to backing store. */
    char backing_path[PMIX_PATH_MAX];
} pmix_shmem_t;
PMIX_EXPORT PMIX_CLASS_DECLARATION(pmix_shmem_t);

PMIX_EXPORT pmix_status_t
pmix_shmem_segment_create(
    pmix_shmem_t *shmem,
    size_t size,
    const char *backing_path
);

PMIX_EXPORT pmix_status_t
pmix_shmem_segment_attach(
    pmix_shmem_t *shmem,
    uintptr_t desired_base_address,
    pmix_shmem_flags_t flags
);

PMIX_EXPORT pmix_status_t
pmix_shmem_segment_detach(
    pmix_shmem_t *shmem
);

PMIX_EXPORT pmix_status_t
pmix_shmem_segment_unlink(
    pmix_shmem_t *shmem
);

/**
 * Change ownership of given shmem. Similar to chown(2).
 */
PMIX_EXPORT pmix_status_t
pmix_shmem_segment_chown(
    pmix_shmem_t *shmem,
    uid_t owner,
    gid_t group
);

/**
 * Change permissions of given shmem. Similar to chmod(2).
 */
PMIX_EXPORT pmix_status_t
pmix_shmem_segment_chmod(
    pmix_shmem_t *shmem,
    mode_t mode
);

/**
 * Returns size padded to page boundary.
 */
PMIX_EXPORT size_t
pmix_shmem_utils_pad_to_page(
    size_t size
);

#endif
