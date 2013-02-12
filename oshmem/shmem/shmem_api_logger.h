/*
 * Copyright (c) 2012      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef SHMEM_API_LOGGER_H
#define SHMEM_API_LOGGER_H
#include "oshmem_config.h"
#include "opal/util/output.h"
OSHMEM_DECLSPEC extern int shmem_api_logger_output;
#ifdef __BASE_FILE__
#define __SPML_FILE__ __BASE_FILE__
#else
#define __SPML_FILE__ __FILE__
#endif

#define SHMEM_API_VERBOSE(level, format, ...) \
            opal_output_verbose(level, shmem_api_logger_output, "%s:%d - %s() " format, \
                                                            __SPML_FILE__, __LINE__, __FUNCTION__, ## __VA_ARGS__)

#define SHMEM_API_ERROR(format, ... ) \
            opal_output_verbose(0, shmem_api_logger_output, "Error: %s:%d - %s() " format, \
                                                            __SPML_FILE__, __LINE__, __FUNCTION__, ## __VA_ARGS__)
#endif /*SHMEM_API_LOGGER_H*/
