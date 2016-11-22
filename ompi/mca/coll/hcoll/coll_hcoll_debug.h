/**
  Copyright (c) 2011 Mellanox Technologies. All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
 */

#ifndef COLL_HCOL_DEBUG_H
#define COLL_HCOL_DEBUG_H
#include "ompi_config.h"
#pragma GCC system_header

#ifdef __BASE_FILE__
#define __HCOL_FILE__ __BASE_FILE__
#else
#define __HCOL_FILE__ __FILE__
#endif

#define HCOL_VERBOSE(level, format, ...) \
    opal_output_verbose(level, mca_coll_hcoll_output, "%s:%d - %s() " format, \
                        __HCOL_FILE__, __LINE__, __FUNCTION__, ## __VA_ARGS__)

#define HCOL_ERROR(format, ... ) \
    opal_output_verbose(0, mca_coll_hcoll_output, "Error: %s:%d - %s() " format, \
                        __HCOL_FILE__, __LINE__, __FUNCTION__, ## __VA_ARGS__)


#define HCOL_MODULE_VERBOSE(hcoll_module, level, format, ...) \
        HCOL_VERBOSE(level, "[%p:%d] " format, (void*)(hcoll_module)->comm, (hcoll_module)->rank, ## __VA_ARGS__)

extern int mca_coll_hcoll_output;

#endif // COLL_HCOL_DEBUG_H
