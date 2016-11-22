/**
  Copyright (c) 2011 Mellanox Technologies. All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
 */

#ifndef SCOLL_MPI_DEBUG_H
#define SCOLL_MPI_DEBUG_H
#include "oshmem_config.h"
#pragma GCC system_header

#ifdef __BASE_FILE__
#define __MPI_FILE__ __BASE_FILE__
#else
#define __MPI_FILE__ __FILE__
#endif

#ifdef OPAL_ENABLE_DEBUG
#define MPI_COLL_VERBOSE(level, ...) \
    oshmem_output_verbose(level, mca_scoll_mpi_output, "%s:%d - %s() ", \
                        __MPI_FILE__, __LINE__, __FUNCTION__, __VA_ARGS__)
#else
#define MPI_COLL_VERBOSE(level, ...)
#endif

#define MPI_COLL_ERROR(...) \
    oshmem_output_verbose(0, mca_scoll_mpi_output, "Error: %s:%d - %s() ", \
                        __MPI_FILE__, __LINE__, __FUNCTION__, __VA_ARGS__)

extern int mca_scoll_mpi_output;

#endif // SCOLL_MPI_DEBUG_H
