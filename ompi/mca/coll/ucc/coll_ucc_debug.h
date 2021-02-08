/**
  Copyright (c) 2021 Mellanox Technologies. All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
 */

#ifndef COLL_UCC_DEBUG_H
#define COLL_UCC_DEBUG_H
#include "ompi_config.h"
#pragma GCC system_header

#ifdef __BASE_FILE__
#define __UCC_FILE__ __BASE_FILE__
#else
#define __UCC_FILE__ __FILE__
#endif

#define UCC_VERBOSE(level, format, ...) \
    opal_output_verbose(level, mca_coll_ucc_output, "%s:%d - %s() " format, \
                        __UCC_FILE__, __LINE__, __FUNCTION__, ## __VA_ARGS__)

#define UCC_ERROR(format, ... ) \
    opal_output_verbose(0, mca_coll_ucc_output, "Error: %s:%d - %s() " format, \
                        __UCC_FILE__, __LINE__, __FUNCTION__, ## __VA_ARGS__)

extern int mca_coll_ucc_output;
#endif
