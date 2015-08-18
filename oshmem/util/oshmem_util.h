/*
 *  Copyright (c) 2014      Mellanox Technologies, Inc.
 *                          All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OSHMEM_UTIL_H
#define OSHMEM_UTIL_H

#include "oshmem_config.h"

/*
 * Environment variables
 */
#define OSHMEM_ENV_SYMMETRIC_SIZE      "SMA_SYMMETRIC_SIZE"
#define OSHMEM_ENV_DEBUG               "SMA_DEBUG"
#define OSHMEM_ENV_INFO                "SMA_INFO"
#define OSHMEM_ENV_VERSION             "SMA_VERSION"


void oshmem_output_verbose(int level, int output_id, const char* prefix,
    const char* file, int line, const char* function, const char* format, ...);

/*
 * Temporary wrapper which ingores output verbosity level
 * to ensure error messages are seeing by user
 */
void oshmem_output(int output_id, const char* prefix, const char* file,
    int line, const char* function, const char* format, ...);

#endif /* OSHMEM_UTIL_H */
