/**
  Copyright (c) 2021 Mellanox Technologies. All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
 */

#ifndef SCOLL_UCC_DEBUG_H
#define SCOLL_UCC_DEBUG_H
#include "oshmem_config.h"
#pragma GCC system_header

#ifdef __BASE_FILE__
#define __UCC_FILE__ __BASE_FILE__
#else
#define __UCC_FILE__ __FILE__
#endif

#ifdef OPAL_ENABLE_DEBUG
#define UCC_VERBOSE(level, ...) \
    oshmem_output_verbose(level, mca_scoll_ucc_output, "%s:%d - %s() ", \
                        __UCC_FILE__, __LINE__, __FUNCTION__, __VA_ARGS__)
#else
#define UCC_VERBOSE(level, ...)
#endif

#define UCC_ERROR(...) \
    oshmem_output_verbose(0, mca_scoll_ucc_output, "Error: %s:%d - %s() ", \
                        __UCC_FILE__, __LINE__, __FUNCTION__, __VA_ARGS__)

extern int mca_scoll_ucc_output;

#endif // SCOLL_UCC_DEBUG_H
