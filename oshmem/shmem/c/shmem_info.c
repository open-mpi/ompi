/*
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "oshmem_config.h"

#include <string.h>
#include <assert.h>

#include "oshmem/constants.h"
#include "oshmem/include/shmem.h"

void shmem_info_get_version(int *major, int *minor)
{
    if (major) {
        *major = SHMEM_MAJOR_VERSION;
    }

    if (minor) {
        *minor = SHMEM_MINOR_VERSION;
    }
}

void shmem_info_get_name(char *name)
{
    if (name) {
        assert(sizeof(SHMEM_VENDOR_STRING) < SHMEM_MAX_NAME_LEN);
        memset(name, 0, SHMEM_MAX_NAME_LEN);
        memcpy(name, SHMEM_VENDOR_STRING, sizeof(SHMEM_VENDOR_STRING));
    }
}
