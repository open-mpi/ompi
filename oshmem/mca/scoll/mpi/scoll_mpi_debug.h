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

#define MPI_COLL_VERBOSE(level, format, ...) \
    opal_output_verbose(level, mca_scoll_mpi_output, "%s:%d - %s() " format, \
                        __MPI_FILE__, __LINE__, __FUNCTION__, ## __VA_ARGS__)

#define MPI_COLL_ERROR(format, ... ) \
    opal_output_verbose(0, mca_scoll_mpi_output, "Error: %s:%d - %s() " format, \
                        __MPI_FILE__, __LINE__, __FUNCTION__, ## __VA_ARGS__)


#define MPI_MODULE_VERBOSE(mpi_module, level, format, ...) \
        MPI_COLL_VERBOSE(level, "[%p:%d] " format, (void*)(mpi_module)->comm, (mpi_module)->rank, ## __VA_ARGS__)

extern int mca_scoll_mpi_output;

#endif // SCOLL_MPI_DEBUG_H
