/**
  Copyright (c) 2020 Mellanox Technologies. All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
 */

#ifndef COLL_MCCL_DEBUG_H
#define COLL_MCCL_DEBUG_H
#include "ompi_config.h"
#pragma GCC system_header

#ifdef __BASE_FILE__
#define __MCCL_FILE__ __BASE_FILE__
#else
#define __MCCL_FILE__ __FILE__
#endif

#define MCCL_VERBOSE(level, format, ...) \
    opal_output_verbose(level, mca_coll_mccl_output, "%s:%d - %s() " format, \
                        __MCCL_FILE__, __LINE__, __FUNCTION__, ## __VA_ARGS__)

#define MCCL_ERROR(format, ... ) \
    opal_output_verbose(0, mca_coll_mccl_output, "Error: %s:%d - %s() " format, \
                        __MCCL_FILE__, __LINE__, __FUNCTION__, ## __VA_ARGS__)

extern int mca_coll_mccl_output;
#endif
