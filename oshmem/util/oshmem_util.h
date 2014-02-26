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

void oshmem_output_verbose(int level, int output_id, const char* prefix, const char* file, int line, const char* function, const char* format, ...);

#endif /* OSHMEM_UTIL_H */
